{-# LANGUAGE OverloadedStrings #-}
-- | Conduit Interface for JSON-RPC.
module Network.JsonRpc.Conduit
( -- * Conduits
  -- ** High-Level
  AppConduits
, IncomingMsg(..)
, runConduits
, tcpClient
, tcpServer
, query
  -- ** Low-Level
, Session(..)
, initSession
, encodeConduit
, msgConduit
, decodeConduit
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TMChan
import Data.Maybe
import Data.Text (Text)
import Network.JsonRpc.Data

type AppConduits qo no ro qi ni ri m =
    (Source m (IncomingMsg qo qi ni ri), Sink (Message qo no ro) m ())

type SentRequests qo = HashMap Id (Request qo)

data IncomingMsg qo qi ni ri
    = IncomingMsg   { incomingMsg :: !(Message qi ni ri)
                    , matchingReq :: !(Maybe (Request qo))
                    }
    | IncomingError { incomingError :: !ErrorObj
                    }
    deriving (Eq, Show)

instance (NFData qo, NFData qi, NFData ni, NFData ri)
    => NFData (IncomingMsg qo qi ni ri)
  where
    rnf (IncomingMsg msg qr) = rnf msg `seq` rnf qr
    rnf (IncomingError e) = rnf e

data Session qo = Session
    { lastId        :: TVar Id
    , sentRequests  :: TVar (SentRequests qo)
    , isLast        :: TVar Bool
    }

initSession :: STM (Session qo)
initSession = Session <$> newTVar (IdInt 0)
                      <*> newTVar M.empty
                      <*> newTVar False

-- | Conduit that serializes JSON documents for sending to the network.
encodeConduit :: (ToJSON a, Monad m) => Conduit a m ByteString
encodeConduit = CL.map (L8.toStrict . flip L8.append "\n" . encode)

-- | Conduit for outgoing JSON-RPC messages.
-- Adds an id to requests whose id is 'IdNull'.
msgConduit :: MonadIO m
           => Session qo
           -> Conduit (Message qo no ro) m (Message qo no ro)
msgConduit qs = await >>= \nqM -> case nqM of
    Nothing ->
        liftIO (atomically $ writeTVar (isLast qs) True) >> return ()
    Just (MsgRequest rq) -> do
        msg <- MsgRequest <$> liftIO (atomically $ addId rq)
        yield msg >> msgConduit qs
    Just msg ->
        yield msg >> msgConduit qs
  where
    addId rq = case getReqId rq of
        IdNull -> do
            i <- readTVar (lastId qs)
            h <- readTVar (sentRequests qs)
            let i'  = succ i
                rq' = rq { getReqId = i' }
                h'  = M.insert i' rq' h
            writeTVar (lastId qs) i'
            writeTVar (sentRequests qs) h'
            return rq'
        i -> do
            h <- readTVar (sentRequests qs)
            writeTVar (sentRequests qs) (M.insert i rq h)
            return rq

-- | Conduit to decode incoming JSON-RPC messages.  An error in the decoding
-- operation will output an 'IncomingError', which should be relayed to the
-- remote party.
decodeConduit
    :: (FromRequest qi, FromNotif ni, FromResponse ri, MonadIO m)
    => Ver
    -> Bool -- ^ Close on last response
    -> Session qo
    -> Conduit ByteString m (IncomingMsg qo qi ni ri)
decodeConduit ver c qs = CB.lines =$= f where
    f = await >>= \bsM -> case bsM of
        Nothing ->
            return ()
        Just bs -> do
            (m, d) <- liftIO . atomically $ decodeSTM bs
            yield m >> unless d f

    decodeSTM bs = readTVar (sentRequests qs) >>= \h -> case decodeMsg h bs of
        Right m@(MsgResponse rs) -> do
            (rq, dis) <- requestSTM h $ getResId rs
            return (IncomingMsg m (Just rq), dis)
        Right m@(MsgError re) -> case getErrId re of
            IdNull ->
                return (IncomingMsg m Nothing, False)
            i -> do
                (rq, dis) <- requestSTM h i
                return (IncomingMsg m (Just rq), dis)
        Right m -> return (IncomingMsg m Nothing, False)
        Left  e -> return (IncomingError e, False)

    requestSTM h i = do
       let rq = fromJust $ i `M.lookup` h
           h' = M.delete i h
       writeTVar (sentRequests qs) h'
       t <- readTVar $ isLast qs
       return (rq, c && t && M.null h')

    decodeMsg h bs = case eitherDecodeStrict' bs of
        Left  _ -> Left $ errorParse ver Null
        Right v -> case parseEither (topParse h) v of
            Left  _ -> Left $ errorParse ver Null
            Right x -> x

    topParse h v = parseReq v
               <|> parseNot v
               <|> parseRes h v
               <|> return (Left $ errorInvalid ver v)

    parseRes h = withObject "response" $ \o -> do
        r <- o .:? "result" .!= Null
        e <- o .:? "error"  .!= Null
        when (r == Null && e == Null) mzero
        i <- o .:? "id" .!= IdNull
        j <- o .:? "jsonrpc"
        let ver' = if j == Just ("2.0" :: Text) then V2 else V1
        case i of
            IdNull ->
                Right . MsgError <$> parseJSON (Object o)
            _ -> case M.lookup i h of
                Nothing -> return . Left $ errorId ver' i
                Just rq -> do
                    rsE <- parseResponse rq (Object o)
                    return $ case rsE of
                        Left  er -> Right $ MsgError er
                        Right rs -> Right $ MsgResponse rs

    parseReq v = flip fmap (parseRequest v) $ \rqE -> case rqE of
        Left   e -> Left e
        Right rq -> Right $ MsgRequest rq

    parseNot v = flip fmap (parseNotif v) $ \rnE -> case rnE of
        Left   e -> Left e
        Right rn -> Right $ MsgNotif rn

-- | Send requests and get responses (or errors).
query :: (ToJSON qo, ToRequest qo, FromResponse ri)
      => Ver
      -> [qo]
      -> Source IO (IncomingMsg qo () () ri)
      -> Sink (Message qo () ri) IO ()
      -> IO [IncomingMsg qo () () ri]
query ver qs src snk = withAsync (src $$ CL.consume) $ \a -> do
    link a
    CL.sourceList qs $= CL.map (MsgRequest . buildRequest ver) $$ snk
    wait a

runConduits :: ( FromRequest qi, FromNotif ni, FromResponse ri 
               , ToJSON qo, ToJSON no, ToJSON ro )
            => Ver
            -> Bool          -- ^ Disconnect on last response
            -> Sink ByteString IO ()
            -> Source IO ByteString
            -> (AppConduits qo no ro qi ni ri IO -> IO a)
            -> IO a
runConduits ver d rpcSnk rpcSrc f = do
    (reqChan, msgChan) <- atomically $ (,) <$> newTBMChan 128 <*> newTBMChan 128
    let inbSrc = sourceTBMChan msgChan
        inbSnk = sinkTBMChan   msgChan True
        outSrc = sourceTBMChan reqChan
        outSnk = sinkTBMChan   reqChan True
    withAsync (rpcThread outSrc inbSnk) (g inbSrc outSnk)
  where
    rpcThread outSrc inbSnk = do
        qs <- atomically initSession
        withAsync (outThread qs outSrc) $ \a -> do
            link a
            _ <- rpcSrc $= decodeConduit ver d qs $$ inbSnk
            wait a
    outThread qs outSrc = outSrc $= msgConduit qs $= encodeConduit $$ rpcSnk
    g inbSrc outSnk a = do
        link a
        x <- f (inbSrc, outSnk)
        _ <- wait a
        return x

tcpClient :: ( FromRequest qi, FromNotif ni, FromResponse ri
             , ToJSON qo, ToJSON no, ToJSON ro )
          => Ver
          -> Bool   -- ^ Disconnect on last response
          -> ClientSettings
          -> (AppConduits qo no ro qi ni ri IO -> IO a)
          -> IO a
tcpClient ver d cs f = runTCPClient cs $ \ad -> do
    runConduits ver d (appSink ad) (appSource ad) f

tcpServer :: ( FromRequest qi, FromNotif ni, FromResponse ri
             , ToJSON qo, ToJSON no, ToJSON ro )
          => Ver
          -> ServerSettings
          -> (AppConduits qo no ro qi ni ri IO -> IO ())
          -> IO ()
tcpServer ver ss f = runTCPServer ss $ \cl -> do
    runConduits ver False (appSink cl) (appSource cl) f

