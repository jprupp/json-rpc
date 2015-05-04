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
, SentRequests
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
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TMChan
import Data.Maybe
import Data.Text (Text)
import Network.JsonRpc.Data

-- | Conduits for sending and receiving JSON-RPC messages.
type AppConduits qo no ro qi ni ri m =
    (Source m (IncomingMsg qo qi ni ri), Sink (Message qo no ro) m ())

-- | Map of ids to sent requests.
type SentRequests qo = HashMap Id (Request qo)

-- | Incoming messages. Responses and corresponding requests go together.
-- 'IncomingError' is for problems decoding incoming messages. These should be
-- sent to the remote party.
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

-- | JSON-RPC session mutable data.
data Session qo = Session
    { lastId        :: TVar Id                  -- ^ Last generated id
    , sentRequests  :: TVar (SentRequests qo)   -- ^ Map of ids to requests
    , isLast        :: TQueue Bool
    -- ^ For each sent request, write a False, when sink closes, write a True
    }

-- | Initialize JSON-RPC session.
initSession :: STM (Session qo)
initSession = Session <$> newTVar (IdInt 0)
                      <*> newTVar M.empty
                      <*> newTQueue

-- | Conduit that serializes JSON documents for sending to the network.
encodeConduit :: (ToJSON a, Monad m) => Conduit a m ByteString
encodeConduit = CL.map (L8.toStrict . encode)

-- | Conduit for outgoing JSON-RPC messages.
-- Adds an id to requests whose id is 'IdNull'.
-- Tracks all sent requests to match corresponding responses.
msgConduit :: MonadIO m
           => Bool
           -- ^ Set to true if decodeConduit must disconnect on last response
           -> Session qo
           -> Conduit (Message qo no ro) m (Message qo no ro)
msgConduit c qs = await >>= \nqM -> case nqM of
    Nothing ->
        when c . liftIO . atomically $ writeTQueue (isLast qs) True
    Just (MsgRequest rq) -> do
        msg <- MsgRequest <$> liftIO (atomically $ addId rq)
        yield msg >> msgConduit c qs
    Just msg ->
        yield msg >> msgConduit c qs
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
            when c $ writeTQueue (isLast qs) False
            return rq'
        i -> do
            h <- readTVar (sentRequests qs)
            writeTVar (sentRequests qs) (M.insert i rq h)
            when c $ writeTQueue (isLast qs) False
            return rq

-- | Conduit to decode incoming JSON-RPC messages.  An error in the decoding
-- operation will output an 'IncomingError', which should be relayed to the
-- remote party.
decodeConduit
    :: (FromRequest qi, FromNotif ni, FromResponse ri, MonadIO m)
    => Ver                          -- ^ JSON-RPC version
    -> Bool                         -- ^ Close on last response
    -> Session qo                   -- ^ JSON-RPC session
    -> Conduit ByteString m (IncomingMsg qo qi ni ri)
                                    -- ^ Decoded incoming messages
decodeConduit ver c qs = evalStateT (f True) Nothing where
    f re = if c && re
      then do
        l <- liftIO . atomically $ readTQueue (isLast qs)
        unless l loop
      else loop

    loop = lift await >>= maybe flush process

    process = runParser >=> handle

    flush = do
      p <- get
      case p of
        Nothing -> return ()
        Just k -> handle (k B.empty)

    runParser chunk = do
      p <- getPartialParser
      return $ case p of
        Nothing -> parse json' chunk
        Just k  -> k chunk

    getPartialParser = get <* put Nothing

    handle (Fail {})     = lift (yield . IncomingError $ errorParse ver Null)
    handle (Partial k)   = put (Just k) >> loop
    handle (Done rest v) = do
        msg <- liftIO . atomically $ decodeSTM v
        lift $ yield msg
        if B.null rest
          then f (re msg)
          else process rest
      where
        re (IncomingMsg _ (Just _)) = True
        re _ = False

    decodeSTM v = do
        h <- readTVar (sentRequests qs)
        case parseEither (topParse h) v of
          Left _ -> return . IncomingError $ errorParse ver Null
          Right x -> case x of
            Right m@(MsgResponse rs) -> do
                rq <- requestSTM h $ getResId rs
                return $ IncomingMsg m (Just rq)
            Right m@(MsgError re) -> case getErrId re of
                IdNull ->
                    return $ IncomingMsg m Nothing
                i -> do
                    rq <- requestSTM h i
                    return $ IncomingMsg m (Just rq)
            Right m -> return $ IncomingMsg m Nothing
            Left  e -> return $ IncomingError e

    requestSTM h i = do
       let rq = fromJust $ i `M.lookup` h
           h' = M.delete i h
       writeTVar (sentRequests qs) h'
       return rq

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
--
-- Example:
--
-- >tcpClient V2 True (clientSettings 31337 "127.0.0.1") (query V2 [TimeReq])
query :: (ToJSON qo, ToRequest qo, FromResponse ri)
      => Ver                              -- ^ JSON-RPC version
      -> [qo]                             -- ^ List of requests
      -> AppConduits qo () () () () ri IO -- ^ Message conduits
      -> IO [IncomingMsg qo () () ri]     -- ^ Incoming messages
query ver qs (src, snk) = withAsync (src $$ CL.consume) $ \a -> do
    link a
    CL.sourceList qs $= CL.map (MsgRequest . buildRequest ver) $$ snk
    wait a

-- Will create JSON-RPC conduits around 'ByteString' conduits from a transport
-- layer.
runConduits :: ( FromRequest qi, FromNotif ni, FromResponse ri
               , ToJSON qo, ToJSON no, ToJSON ro )
            => Ver                      -- ^ JSON-RPC version
            -> Bool                     -- ^ Disconnect on last response
            -> Sink ByteString IO ()    -- ^ Sink to send messages
            -> Source IO ByteString     -- ^ Source of incoming messages
            -> (AppConduits qo no ro qi ni ri IO -> IO a)
                                        -- ^ JSON-RPC action
            -> IO a                     -- ^ Output of action
runConduits ver d rpcSnk rpcSrc f = do
    (reqChan, msgChan) <- atomically $ (,) <$> newTBMChan 128
                                           <*> newTBMChan 128
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
    outThread qs outSrc = outSrc $= msgConduit d qs $= encodeConduit $$ rpcSnk
    g inbSrc outSnk a = do
        link a
        x <- f (inbSrc, outSnk)
        _ <- wait a
        return x

-- JSON-RPC-over-TCP client.
tcpClient :: ( FromRequest qi, FromNotif ni, FromResponse ri
             , ToJSON qo, ToJSON no, ToJSON ro )
          => Ver                      -- ^ JSON-RPC version
          -> Bool                     -- ^ Disconnect on last response
          -> ClientSettings           -- ^ Connection settings
          -> (AppConduits qo no ro qi ni ri IO -> IO a)
                                      -- ^ JSON-RPC action
          -> IO a                     -- ^ Output of action
tcpClient ver d cs f = runTCPClient cs $ \ad ->
    runConduits ver d (cr =$ appSink ad) (appSource ad $= ln) f
  where
    cr = CL.map (`B8.snoc` '\n')
    ln = await >>= \bsM -> case bsM of
        Nothing -> return ()
        Just bs -> let (l, ls) = B8.break (=='\n') bs in case ls of
            "" -> await >>= \bsM' -> case bsM' of
                Nothing  -> if B8.null l then return () else yield l
                Just bs' -> leftover (bs `B8.append` bs') >> ln
            _  -> case l of
                "" -> leftover (B8.tail ls) >> ln
                _  -> leftover (B8.tail ls) >> yield l >> ln

-- JSON-RPC-over-TCP server.
tcpServer :: ( FromRequest qi, FromNotif ni, FromResponse ri
             , ToJSON qo, ToJSON no, ToJSON ro )
          => Ver                        -- ^ JSON-RPC version
          -> ServerSettings             -- ^ Connection settings
          -> (AppConduits qo no ro qi ni ri IO -> IO ())
          -- ^ JSON-RPC action to perform on connecting client thread
          -> IO ()
tcpServer ver ss f = runTCPServer ss $ \cl ->
    runConduits ver False (cr =$ appSink cl) (appSource cl) f
  where
    cr = CL.map (`B8.snoc` '\n')
