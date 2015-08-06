{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Interface for JSON-RPC.
module Network.JSONRPC.Interface
( -- * Establish JSON-RPC context
  JSONRPCT
, runJSONRPCT

  -- * Conduits for encoding/decoding
, decodeConduit
, encodeConduit

  -- * Communicate with remote party
, sendRequest
, sendNotif
, receiveNotif
  -- ** Dummies
, dummyRespond
, dummySrv

  -- * Transports
  -- ** Client
, jsonRPCTCPClient
, jsonRPCHTTPClient
  -- ** Server
, jsonRPCTCPServer
-- , jsonRPCHttpServer
) where

import Control.Applicative
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import Network.JSONRPC.Data

type SentRequests = HashMap Id (TMVar (Either RPCError Response))

data Session = Session { inCh     :: TBMChan (Either RPCError Message)
                       , outCh    :: TBMChan Message
                       , notifCh  :: TBMChan (Either RPCError Notif)
                       , lastId   :: TVar Id
                       , sentReqs :: TVar SentRequests
                       , rpcVer   :: Ver
                       }

-- Context for JSON-RPC connection. Connection will remain active as long
-- as context is maintaned.
type JSONRPCT = ReaderT Session

initSession :: Ver -> STM Session
initSession v = Session <$> newTBMChan 16
                        <*> newTBMChan 16
                        <*> newTBMChan 16
                        <*> newTVar (IdInt 0)
                        <*> newTVar M.empty
                        <*> return v

encodeConduit :: MonadLogger m => Conduit Message m ByteString
encodeConduit = CL.mapM $ \m -> do
    $(logDebug) $ T.pack $ unwords $ case m of
        MsgError e -> [ "Sending error id:", fromId (getErrId e) ]
        MsgRequest q -> [ "Sending request id:", fromId (getReqId q) ]
        MsgNotif _ -> [ "Sending notification" ]
        MsgResponse r -> [ "Sending response id:", fromId (getResId r) ]
    return . L8.toStrict $ encode m

decodeConduit :: MonadLogger m
              => Ver -> Conduit ByteString m (Either RPCError Message)
decodeConduit ver = evalStateT loop Nothing where
    loop = lift await >>= maybe flush process

    flush = get >>= \kM -> case kM of Nothing -> return ()
                                      Just k  -> handle (k B.empty)
    process = runParser >=> handle

    runParser ck = maybe (parse json' ck) ($ ck) <$> get <* put Nothing

    handle (Fail {}) = do
        $(logWarn) "Error parsing incoming message"
        lift . yield . Left $ RPCError ver (errorParse Null) IdNull
        loop
    handle (Partial k) = put (Just k) >> loop
    handle (Done rest v) = do
        let msg = decodeJSONRPC v
        when (isLeft msg) $ $(logWarn) "Received invalid message"
        lift $ yield msg
        if B.null rest then loop else process rest

    decodeJSONRPC v = case parseMaybe parseJSON v of
        Just msg -> Right msg
        Nothing -> Left $ RPCError ver (errorInvalid v) IdNull

processIncoming :: (Functor m, MonadLoggerIO m, FromRequest q, ToJSON r)
                => Respond q m r -> JSONRPCT m ()
processIncoming r = do
    i <- reader inCh
    o <- reader outCh
    n <- reader notifCh
    s <- reader sentReqs
    v <- reader rpcVer
    join . liftIO . atomically $ readTBMChan i >>= \inc -> case inc of
        Nothing -> return $ do
            $(logDebug) "Closed incoming channel"
            return ()
        Just (Left e) -> do
            writeTBMChan o (MsgError e)
            return $ processIncoming r
        Just (Right (MsgNotif t)) -> do
            writeTBMChan n (Right t)
            return $ do
                $(logDebug) "Received notification"
                processIncoming r
        Just (Right (MsgRequest q)) -> return $ do
            $(logDebug) $ T.pack $ unwords
                [ "Received request id:", fromId (getReqId q) ]
            msg <- lift $ either MsgError MsgResponse <$> buildResponse r q
            liftIO . atomically $ writeTBMChan o msg
            processIncoming r
        Just (Right (MsgResponse res@(Response _ _ x))) -> do
            m <- readTVar s
            let pM = x `M.lookup` m
            case pM of
                Nothing ->
                    writeTBMChan o . MsgError $ RPCError v (errorId x) IdNull
                Just p ->
                    writeTVar s (x `M.delete` m) >> putTMVar p (Right res)
            return $ do
                case pM of
                    Nothing -> $(logWarn) $ T.pack $ unwords
                        [ "Got response with unkwnown id:", fromId x ]
                    _ -> $(logDebug) $ T.pack $ unwords
                        [ "Received response id:", fromId x ]
                processIncoming r
        Just (Right (MsgError err@(RPCError _ _ IdNull))) -> do
            writeTBMChan n $ Left err
            return $ do
                $(logWarn) "Got standalone error message"
                processIncoming r
        Just (Right (MsgError err@(RPCError _ _ x))) -> do
            m <- readTVar s
            let pM = x `M.lookup` m
            case pM of
                Nothing ->
                    writeTBMChan o . MsgError $ RPCError v (errorId x) IdNull
                Just p ->
                    writeTVar s (x `M.delete` m) >> putTMVar p (Left err)
            return $ do
                case pM of
                    Nothing -> $(logWarn) $ T.pack $ unwords
                        [ "Got error with unknown id:", fromId x ]
                    _ -> $(logWarn) $ T.pack $ unwords
                        [ "Received error id:", show x ]
                processIncoming r

-- | Returns Right Nothing if could not parse response.
sendRequest :: (MonadLoggerIO m, ToJSON q, ToRequest q, FromResponse r)
            => q -> JSONRPCT m (Either ErrorObj (Maybe r))
sendRequest q = do
    v <- reader rpcVer
    l <- reader lastId
    s <- reader sentReqs
    o <- reader outCh
    p <- liftIO . atomically $ do
        p <- newEmptyTMVar 
        i <- succ <$> readTVar l
        m <- readTVar s
        let req = buildRequest v q i
        writeTVar s $ M.insert i p m
        writeTBMChan o $ MsgRequest req 
        writeTVar l i
        return p
    liftIO . atomically $ takeTMVar p >>= \pE -> case pE of
        Left e -> return . Left $ getErrObj e
        Right y -> case fromResponse (requestMethod q) y of
            Nothing -> return $ Right Nothing
            Just x -> return . Right $ Just x

-- | Send notification. Will not block.
sendNotif :: (ToJSON no, ToNotif no, MonadLoggerIO m) => no -> JSONRPCT m ()
sendNotif n = do
    o <- reader outCh
    v <- reader rpcVer
    let notif = buildNotif v n
    liftIO . atomically $ writeTBMChan o (MsgNotif notif)

-- | Receive notifications from peer. Will not block.
-- Returns Nothing if incoming channel is closed and empty.
-- Result is Right Nothing if it failed to parse notification.
receiveNotif :: (MonadLoggerIO m, FromNotif n)
             => JSONRPCT m (Maybe (Either ErrorObj (Maybe n)))
receiveNotif = do
    c <- reader notifCh
    liftIO . atomically $ readTBMChan c >>= \nM -> case nM of
        Nothing -> return Nothing
        Just (Left e) -> return . Just . Left $ getErrObj e
        Just (Right n) -> case fromNotif n of
            Nothing -> return . Just $ Right Nothing
            Just x -> return . Just . Right $ Just x

-- | Create JSON-RPC session around conduits from transport
-- layer. When context exits session disappears.
runJSONRPCT :: ( MonadLoggerIO m, MonadBaseControl IO m
               , FromRequest q, ToJSON r
               )
            => Ver                    -- ^ JSON-RPC version
            -> Respond q m r          -- ^ Respond to incoming requests
            -> Sink Message m ()      -- ^ Sink to send messages
            -> Source m (Either RPCError Message)
            -- ^ Source of incoming messages
            -> JSONRPCT m a           -- ^ JSON-RPC action
            -> m a                    -- ^ Output of action
runJSONRPCT ver r snk src f = do
    qs <- liftIO . atomically $ initSession ver
    let inSnk  = sinkTBMChan (inCh qs) True
        outSrc = sourceTBMChan (outCh qs)
    withAsync (src $$ inSnk) $ const $
        withAsync (outSrc $$ snk) $ const $
            withAsync (runReaderT (processIncoming r) qs) $ const $
                runReaderT f qs


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


-- | Dummy action for servers not expecting clients to send notifications,
-- which is true in most cases.
dummySrv :: MonadLoggerIO m => JSONRPCT m ()
dummySrv = receiveNotif >>= \nM -> case nM of
        Just n -> (n :: Either ErrorObj (Maybe ())) `seq` dummySrv
        Nothing -> return ()

-- | Respond function for systems that do not reply to requests, as usual
-- in clients.
dummyRespond :: MonadLoggerIO m => Respond () m ()
dummyRespond = const . return $ Right () 

--
-- Transports
--

-- | TCP client transport for JSON-RPC.
jsonRPCTCPClient
    :: ( MonadLoggerIO m, MonadBaseControl IO m
       , FromRequest q, ToJSON r
       )
    => Ver            -- ^ JSON-RPC version
    -> ClientSettings -- ^ Connection settings
    -> Respond q m r  -- ^ Respond to incoming requests
    -> JSONRPCT m a   -- ^ JSON-RPC action
    -> m a            -- ^ Output of action
jsonRPCTCPClient ver cs r f = runGeneralTCPClient cs $ \ad ->
    runJSONRPCT ver r
        (encodeConduit =$ cr =$ appSink ad)
        (appSource ad $= ln $= decodeConduit ver) f

-- | HTTP client transport for JSON-RPC
jsonRPCHTTPClient
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => Ver            -- ^ JSON-RPC version
    -> HC.Request     -- ^ URL and settings for remote endpoint
    -> JSONRPCT m a   -- ^ JSON-RPC action
    -> m ()           -- ^ Output of action
jsonRPCHTTPClient ver req f = do
    (ic, oc) <- liftIO . atomically $ (,) <$> newTBMChan 16 <*> newTBMChan 16
    let snk = sinkTBMChan oc True
        src = sourceTBMChan ic
    _ <- runJSONRPCT ver dummyRespond
        (sender =$ encodeConduit =$ snk)
        (src $= decodeConduit ver $= receiver) f
    man <- liftIO $ HC.newManager HC.defaultManagerSettings
    loop ic oc man
    $(logDebug) "Finished HTTP client session"
  where
    sender = await >>= \msgM -> case msgM of
        Nothing -> return ()
        Just m@(MsgRequest _) -> yield m >> sender
        _ -> $(logError) "Only requests can be sent over HTTP" >> sender
    receiver = await >>= \msgM -> case msgM of
        Nothing -> return ()
        Just m@(Right (MsgResponse _)) -> yield m >> receiver
        Just m@(Right (MsgError    _)) -> yield m >> receiver
        _ -> do
            $(logError) "Only responses can be received over HTTP"
            receiver
    loop ic oc man = do
        msgM <- liftIO . atomically $ readTBMChan oc
        case msgM of
            Nothing -> return ()
            Just msg -> do
                let req' = req { HC.method = "POST"
                               , HC.requestBody = HC.RequestBodyBS msg
                               , HC.requestHeaders =
                                   [("content-type", "application/json-rpc")]
                               }
                liftIO . HC.withResponse req' man $ \res -> do
                    let body = HC.responseBody res
                    sendBody ic body
                loop ic oc man
    sendBody ic body = do
        bs <- body
        case bs of
            "" -> return ()
            _ -> do
                liftIO . atomically $ writeTBMChan ic bs
                sendBody ic body


-- | TCP server transport for JSON-RPC.
jsonRPCTCPServer
    :: ( MonadLoggerIO m, MonadBaseControl IO m
       , FromRequest q, ToJSON r)
    => Ver             -- ^ JSON-RPC version
    -> ServerSettings  -- ^ Connection settings
    -> Respond q m r   -- ^ Respond to incoming requests
    -> JSONRPCT m ()   -- ^ Action to perform on connecting client thread
    -> m a
jsonRPCTCPServer ver ss r f = runGeneralTCPServer ss $ \cl ->
    runJSONRPCT ver r
        (encodeConduit =$ cr =$ appSink cl)
        (appSource cl $= ln $= decodeConduit ver) f
