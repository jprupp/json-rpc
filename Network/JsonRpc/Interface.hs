{-# LANGUAGE OverloadedStrings #-}
-- | Interface for JSON-RPC.
module Network.JsonRpc.Interface
( Respond
, JsonRpcT
, runJsonRpcT
, sendRequest
, sendNotif
, receiveNotif
, jsonRpcTcpClient
, jsonRpcTcpServer
, dummySrv
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
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
import Network.JsonRpc.Data

-- | Map of ids to corresponding requests and promises of responses.
type SentRequests = HashMap Id (TMVar (Either Error Response))

-- | Conduits for sending and receiving JSON-RPC messages.
data Session = Session { inCh     :: TBMChan (Either Error Message)
                       , outCh    :: TBMChan Message
                       , notifCh  :: TBMChan (Either Error Notif)
                       , lastId   :: TVar Id
                       , sentReqs :: TVar SentRequests
                       , rpcVer   :: Ver
                       }

type JsonRpcT = ReaderT Session

-- | Initialize JSON-RPC session.
initSession :: Ver -> STM Session
initSession v = Session <$> newTBMChan 16
                        <*> newTBMChan 16
                        <*> newTBMChan 16
                        <*> newTVar (IdInt 0)
                        <*> newTVar M.empty
                        <*> return v

-- | Conduit that serializes JSON documents for sending to the network.
encodeConduit :: (ToJSON a, Monad m) => Conduit a m ByteString
encodeConduit = CL.map $ L8.toStrict . encode

-- | Conduit to decode incoming JSON-RPC messages.
parseMessages :: Monad m
              => Ver -> Conduit ByteString m (Either Error Message)
parseMessages ver = evalStateT loop Nothing where
    loop = lift await >>= maybe flush process

    flush = get >>= \kM -> case kM of Nothing -> return ()
                                      Just k  -> handle (k B.empty)
    process = runParser >=> handle

    runParser ck = maybe (parse json' ck) ($ ck) <$> get <* put Nothing

    handle (Fail {}) = do
        lift . yield . Left $ Error ver (errorParse ver Null) IdNull
        loop
    handle (Partial k) = put (Just k) >> loop
    handle (Done rest v) = do
        let msg = decodeJsonRpc v
        lift $ yield msg
        if B.null rest then loop else process rest

    decodeJsonRpc v = case parseMaybe parseJSON v of
        Just msg -> Right msg
        Nothing -> Left $ Error ver (errorInvalid ver v) IdNull

processIncoming :: (FromRequest q, ToJSON r)
                => Respond q IO r -> JsonRpcT IO ()
processIncoming r = do
    i <- reader inCh
    o <- reader outCh
    n <- reader notifCh
    s <- reader sentReqs
    v <- reader rpcVer
    join . liftIO . atomically $ readTBMChan i >>= \inc -> case inc of
        Nothing -> return $ return ()
        Just (Left e) -> do
            writeTBMChan o (MsgError e)
            return $ processIncoming r
        Just (Right (MsgNotif t)) ->
            writeTBMChan n (Right t) >> return (processIncoming r)
        Just (Right (MsgRequest q)) -> return $ do
            msg <- either MsgError MsgResponse <$> liftIO (buildResponse r q)
            liftIO . atomically $ writeTBMChan o msg
            processIncoming r
        Just (Right (MsgResponse res@(Response _ _ x))) -> do
            m <- readTVar s
            case x `M.lookup` m of
                Nothing ->
                    writeTBMChan o . MsgError $ Error v (errorId v x) IdNull
                Just p ->
                    writeTVar s (x `M.delete` m) >> putTMVar p (Right res)
            return $ processIncoming r
        Just (Right (MsgError err@(Error _ _ IdNull))) -> do
            writeTBMChan n $ Left err
            return $ processIncoming r
        Just (Right (MsgError err@(Error _ _ x))) -> do
            m <- readTVar s
            case x `M.lookup` m of
                Nothing ->
                    writeTBMChan o . MsgError $ Error v (errorId v x) IdNull
                Just p ->
                    writeTVar s (x `M.delete` m) >> putTMVar p (Left err)
            return $ processIncoming r

-- | Send request. return Nothing if could not parse response.
sendRequest :: (ToJSON q, ToRequest q, FromResponse r, MonadIO m)
            => q -> JsonRpcT m (STM (Either ErrorObj (Maybe r)))
sendRequest q = do
    o <- reader outCh
    v <- reader rpcVer
    l <- reader lastId
    s <- reader sentReqs
    p <- liftIO . atomically $ do
        p <- newEmptyTMVar 
        i <- succ <$> readTVar l
        m <- readTVar s
        let req = buildRequest v q i
        writeTVar s $ M.insert i p m
        writeTBMChan o $ MsgRequest req 
        writeTVar l i
        return p
    return $ takeTMVar p >>= \pE -> case pE of
        Left e -> return . Left $ getErrObj e
        Right y@(Response ver r _) -> 
            case fromResponse (requestMethod q) y of
                Nothing -> do
                    let err = MsgError $ Error ver (errorInvalid ver r) IdNull
                    writeTBMChan o err
                    return $ Right Nothing
                Just x -> return . Right $ Just x

sendNotif :: (ToJSON no, ToNotif no, Monad m) => no -> JsonRpcT m (STM ())
sendNotif n = do
    o <- reader outCh
    v <- reader rpcVer
    let notif = buildNotif v n
    return $ writeTBMChan o (MsgNotif notif)

-- | Receive notifications from peer.
-- Returns Nothing if incoming channel is closed and empty.
-- Notification is Nothing if it failed to parse.
receiveNotif :: (Monad m, FromNotif n)
             => JsonRpcT m (STM (Maybe (Either ErrorObj (Maybe n))))
receiveNotif = do
    c <- reader notifCh
    o <- reader outCh
    return $ readTBMChan c >>= \nM -> case nM of
        Nothing -> return Nothing
        Just (Left e) -> return . Just . Left $ getErrObj e
        Just (Right n@(Notif v _ p)) -> case fromNotif n of
            Nothing -> do
                let err = MsgError $ Error v (errorParse v p) IdNull
                writeTBMChan o err
                return . Just $ Right Nothing
            Just x -> return . Just . Right $ Just x

-- | Will create JSON-RPC conduits around 'ByteString' conduits from
-- a transport layer.
runJsonRpcT :: (FromRequest q, ToJSON r)
            => Ver                     -- ^ JSON-RPC version
            -> Respond q IO r          -- ^ Respond to incoming requests
            -> Sink ByteString IO ()   -- ^ Sink to send messages
            -> Source IO ByteString    -- ^ Source of incoming messages
            -> JsonRpcT IO a           -- ^ JSON-RPC action
            -> IO a                    -- ^ Output of action
runJsonRpcT ver r snk src f = do
    qs <- atomically $ initSession ver
    let inSnk  = sinkTBMChan (inCh qs) True
        outSrc = sourceTBMChan (outCh qs)
    withAsync (fromNet inSnk) $ const $
        withAsync (toNet outSrc) $ const $
            withAsync (runReaderT (processIncoming r) qs) $ const $
                runReaderT f qs
  where
    fromNet inSnk = src $= parseMessages ver $$ inSnk
    toNet outSrc = outSrc $= encodeConduit $$ snk


cr :: Monad m => Conduit ByteString m ByteString
cr = CL.map (`B8.snoc` '\n')

ln :: Monad m => Conduit ByteString m ByteString
ln = await >>= \bsM -> case bsM of
    Nothing -> return ()
    Just bs -> let (l, ls) = B8.break (=='\n') bs in case ls of
        "" -> await >>= \bsM' -> case bsM' of
            Nothing  -> unless (B8.null l) $ yield l
            Just bs' -> leftover (bs `B8.append` bs') >> ln
        _  -> case l of
            "" -> leftover (B8.tail ls) >> ln
            _  -> leftover (B8.tail ls) >> yield l >> ln


-- | JSON-RPC-over-TCP client.
jsonRpcTcpClient
    :: (FromRequest q, ToJSON r)
    => Ver             -- ^ JSON-RPC version
    -> ClientSettings  -- ^ Connection settings
    -> Respond q IO r  -- ^ Respond to incoming requests
    -> JsonRpcT IO a   -- ^ JSON-RPC action
    -> IO a            -- ^ Output of action
jsonRpcTcpClient ver cs r f = runTCPClient cs $ \ad ->
    runJsonRpcT ver r (cr =$ appSink ad) (appSource ad $= ln) f

-- | JSON-RPC-over-TCP server.
jsonRpcTcpServer
    :: (FromRequest q, ToJSON r)
    => Ver             -- ^ JSON-RPC version
    -> ServerSettings  -- ^ Connection settings
    -> Respond q IO r  -- ^ Respond to incoming requests
    -> JsonRpcT IO ()  -- ^ Action to perform on connecting client thread
    -> IO ()
jsonRpcTcpServer ver ss r f = runTCPServer ss $ \cl ->
    runJsonRpcT ver r (cr =$ appSink cl) (appSource cl $= ln) f

-- | Dummy server when not expecting client to send anything but requests.
dummySrv :: MonadIO m => JsonRpcT m ()
dummySrv = forever $ do
    n <- receiveNotif
    liftIO (atomically n :: IO (Maybe (Either ErrorObj (Maybe ()))))
