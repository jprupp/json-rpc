{-# LANGUAGE OverloadedStrings #-}
-- | Conduit Interface for JSON-RPC.
module Network.JsonRpc.Conduit
( -- * High-Level
  App
, IncomingMsg(..)
, runConduits
, tcpClient
, tcpServer
, query
  -- * Low-Level
, Session(..)
, initSession
, encodeConduit
, msgConduit
, decodeConduit
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
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

type App qo no ro qi ni ri m a =
    Source m (IncomingMsg qo qi ni ri) -> Sink (Message qo no ro) m () -> m a

type SentRequests qo = HashMap Id (Request qo)

data IncomingMsg qo qi ni ri
    = IncomingMsg   { incomingMsg :: !(Message qi ni ri)
                    , matchingReq :: !(Maybe (Request qo))
                    }
    | IncomingError { incomingError :: !Error
                    }
    deriving (Eq, Show)

-- | Session state.
data Session qo = Session
    { lastId        :: TVar Id                 -- ^ Last used id
    , sentRequests  :: TVar (SentRequests qo)  -- ^ Requests sent
    , isLast        :: TVar Bool               -- ^ No more requests
    }

-- | Create initial session.
initSession :: STM (Session qo)
initSession = Session <$> newTVar (IdInt 0)
                      <*> newTVar M.empty
                      <*> newTVar False

-- | Conduit that serializes JSON documents in lines.
encodeConduit :: (ToJSON a, Monad m) => Conduit a m ByteString
encodeConduit = CL.map (L8.toStrict . flip L8.append "\n" . encode)

-- | Conduit for outgoing JSON-RPC messages.
msgConduit :: ( ToJSON qo, ToRequest qo, ToJSON no, ToNotif no
              , ToJSON ro, MonadIO m )
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

-- | Conduit to decode incoming JSON-RPC messages.
-- Left is an error if decoding an incoming request went wrong.
-- Right is the incoming message decoded, with optional request if it is a
-- response.
decodeConduit
    :: (FromRequest qi, FromNotif ni, FromResponse ri, MonadIO m)
    => Bool          -- ^ RPCv1
    -> Bool          -- ^ Close on last response
    -> Session qo
    -> Conduit ByteString m (IncomingMsg qo qi ni ri)
decodeConduit r1 c qs = CB.lines =$= f where
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
        Left  e -> Left $ errorParse r1 e
        Right v -> case parseEither (topParse h) v of
            Left  e -> Left $ errorParse r1 e
            Right x -> x

    topParse h v = parseReq v
               <|> parseNot v
               <|> parseRes h v
               <|> return (Left $ errorInvalid r1 v)

    parseRes h = withObject "response" $ \o -> do
        r <- o .:? "result" .!= Null
        e <- o .:? "error"  .!= Null
        when (r == Null && e == Null) mzero
        i <- o .:? "id" .!= IdNull
        j <- o .:? "jsonrpc"
        let r1' = j /= Just ("2.0" :: Text)
        case i of
            IdNull ->
                Right . MsgError <$> parseError i o
            _ -> case M.lookup i h of
                Nothing -> return . Left $ errorId r1' i
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
         => Bool    -- ^ RPCv1
         -> [qo]
         -> Source IO (IncomingMsg qo () () ri)
         -> Sink (Message qo () ri) IO ()
         -> IO [IncomingMsg qo () () ri]
query r1 qs src snk = withAsync (src $$ CL.consume) $ \a -> do
    link a
    CL.sourceList qs $= CL.map r $$ snk
    wait a
  where
    r q = if r1 then MsgRequest (Request1 (requestMethod q) q IdNull)
                else MsgRequest (Request  (requestMethod q) q IdNull)

-- | Run JSON-RPC Conduits.
runConduits :: ( FromRequest qi, FromNotif ni, FromResponse ri 
               , ToJSON qo, ToRequest qo, ToJSON no, ToNotif no, ToJSON ro )
            => Bool          -- ^ RPCv1
            -> Bool          -- ^ Disconnect on last response
            -> Sink ByteString IO ()
            -> Source IO ByteString
            -> App qo no ro qi ni ri IO a
            -> IO a
runConduits r1 d rpcSnk rpcSrc f = do
    (reqChan, msgChan) <- atomically $ do
        q <- newTBMChan 128
        m <- newTBMChan 128
        return (q, m)
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
            _ <- rpcSrc $= decodeConduit r1 d qs $$ inbSnk
            wait a
    outThread qs outSrc =
        outSrc $= msgConduit qs $= encodeConduit $$ rpcSnk
    g inbSrc outSnk a = do
        link a
        x <- f inbSrc outSnk
        _ <- wait a
        return x

tcpClient :: ( FromRequest qi, FromNotif ni, FromResponse ri
             , ToJSON qo, ToRequest qo, ToJSON no, ToNotif no, ToJSON ro )
          => Bool   -- ^ RPCv1
          -> Bool   -- ^ Disconnect on last response
          -> ClientSettings
          -> App qo no ro qi ni ri IO a
          -> IO a
tcpClient r1 d cs f = runTCPClient cs $ \ad -> do
    runConduits r1 d (appSink ad) (appSource ad) f

tcpServer :: ( FromRequest qi, FromNotif ni, FromResponse ri
             , ToJSON qo, ToRequest qo, ToJSON no, ToNotif no, ToJSON ro )
          => Bool   -- ^ RPCv1
          -> ServerSettings
          -> App qo no ro qi ni ri IO ()
          -> IO ()
tcpServer r1 ss f = runTCPServer ss $ \cl -> do
    runConduits r1 False (appSink cl) (appSource cl) f

