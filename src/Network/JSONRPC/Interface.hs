{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Interface for JSON-RPC.
module Network.JSONRPC.Interface
( -- * Establish JSON-RPC context
  JSONRPCT
, runJSONRPCT

  -- * Conduits for encoding/decoding
, decodeConduit
, encodeConduit

  -- * Communicate with remote party
, receiveRequest
, receiveBatchRequest
, sendResponse
, sendBatchResponse
, sendRequest
, sendBatchRequest

  -- * Transports
  -- ** Client
, jsonrpcTCPClient
  -- ** Server
, jsonrpcTCPServer

  -- * Internal data and functions
, SentRequests
, Session(..)
, initSession
, processIncoming
, sendMessage
) where

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types           (parseMaybe)
import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Network
import           Data.Conduit.TMChan
import           Data.Either
import qualified Data.Foldable              as F
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.Maybe
import qualified Data.Vector                as V
import           Network.JSONRPC.Data
import           UnliftIO

type SentRequests = HashMap Id (TMVar (Maybe Response))

data Session = Session { inCh     :: TBMChan (Either Response Value)
                       , outCh    :: TBMChan Message
                       , reqCh    :: Maybe (TBMChan BatchRequest)
                       , lastId   :: TVar Id
                       , sentReqs :: TVar SentRequests
                       , rpcVer   :: Ver
                       , dead     :: TVar Bool
                       }

-- Context for JSON-RPC connection.  Connection will remain active as long
-- as context is maintaned.
type JSONRPCT = ReaderT Session

initSession :: Ver -> Bool -> STM Session
initSession v ignore =
    Session <$> newTBMChan 128
            <*> newTBMChan 128
            <*> (if ignore then return Nothing else Just <$> newTBMChan 128)
            <*> newTVar (IdInt 0)
            <*> newTVar M.empty
            <*> return v
            <*> newTVar False

-- Conduit to encode JSON to ByteString.
encodeConduit :: (ToJSON j, MonadLogger m) => ConduitT j ByteString m ()
encodeConduit = CL.mapM $ \m -> return . L8.toStrict $ encode m

-- | Conduit to decode incoming messages.  Left Response indicates
-- a response to send back to sender if parsing JSON fails.
decodeConduit :: MonadLogger m
              => Ver -> ConduitT ByteString (Either Response Value) m ()
decodeConduit ver = evalStateT loop Nothing where
    loop = lift await >>= maybe flush (process False)
    flush = get >>= maybe (return ()) (handl True . ($ B8.empty))
    process b = runParser >=> handl b
    runParser ck = maybe (parse json ck) ($ ck) <$> get <* put Nothing

    handl True (Fail "" _ _) =
        $(logDebug) "ignoring null string at end of incoming data"
    handl b (Fail i _ _) = do
        $(logError) "error parsing incoming message"
        lift . yield . Left $ OrphanError ver (errorParse i)
        unless b loop
    handl _ (Partial k) = put (Just k) >> loop
    handl b (Done rest v) = do
        lift $ yield $ Right v
        if B8.null rest
           then unless b loop
           else process b rest

-- | Process incoming messages. Do not use this directly unless you know
-- what you are doing. This is an internal function.
processIncoming :: (Functor m, MonadLoggerIO m) => JSONRPCT m ()
processIncoming =
    ask >>= \qs ->
        join . liftIO . atomically $
        readTBMChan (inCh qs) >>= \case
            Nothing -> flush qs
            Just vE ->
                case vE of
                    Right v@Object {} -> do
                        single qs v
                        return $ do
                            $(logDebug) "received message"
                            processIncoming
                    Right v@(Array a) -> do
                        if V.null a
                            then do
                                let e = OrphanError (rpcVer qs) (errorInvalid v)
                                writeTBMChan (outCh qs) $ MsgResponse e
                            else batch qs (V.toList a)
                        return $ do
                            $(logDebug) "received batch"
                            processIncoming
                    Right v -> do
                        let e = OrphanError (rpcVer qs) (errorInvalid v)
                        writeTBMChan (outCh qs) $ MsgResponse e
                        return $ do
                            $(logWarn) "got invalid message"
                            processIncoming
                    Left e -> do
                        writeTBMChan (outCh qs) $ MsgResponse e
                        return $ do
                            $(logWarn) "error parsing JSON"
                            processIncoming
  where
    flush qs = do
        m <- readTVar $ sentReqs qs
        F.forM_ (reqCh qs) closeTBMChan
        closeTBMChan $ outCh qs
        writeTVar (dead qs) True
        mapM_ ((`putTMVar` Nothing) . snd) $ M.toList m
        return $ do
            $(logDebug) "session is now dead"
            unless (M.null m) $ $(logError) "requests remained unfulfilled"
    batch qs vs = do
        ts <- catMaybes <$> forM vs (process qs)
        unless (null ts) $
            if any isRight ts
                then do
                    let ch = fromJust $ reqCh qs
                    writeTBMChan ch $ BatchRequest $ rights ts
                else writeTBMChan (outCh qs) $ MsgBatch $ lefts ts
    single qs v = do
        tM <- process qs v
        case tM of
            Nothing -> return ()
            Just (Right t) -> do
                let ch = fromJust $ reqCh qs
                writeTBMChan ch $ SingleRequest t
            Just (Left e) -> writeTBMChan (outCh qs) e
    process qs v = do
        let qM = parseMaybe parseJSON v
        case qM of
            Just q -> request qs q
            Nothing -> do
                let rM = parseMaybe parseJSON v
                case rM of
                    Just r -> response qs r >> return Nothing
                    Nothing -> do
                        let e = OrphanError (rpcVer qs) (errorInvalid v)
                            m = MsgResponse e
                        return $ Just $ Left m
    request qs t =
        case reqCh qs of
            Just _ -> return $ Just $ Right t
            Nothing ->
                case t of
                    Notif {} -> return Nothing
                    Request {} -> do
                        let e = errorMethod (getReqMethod t)
                            v = getReqVer t
                            i = getReqId t
                            m = MsgResponse $ ResponseError v e i
                        return $ Just $ Left m
    response qs r = do
        let hasid =
                case r of
                    Response {} -> True
                    ResponseError {} -> True
                    OrphanError {} -> False -- Ignore orphan errors
        when hasid $ do
            let x = getResId r
            m <- readTVar (sentReqs qs)
            case x `M.lookup` m of
                Nothing -> return () -- Ignore orphan responses
                Just p -> do
                    writeTVar (sentReqs qs) $ M.delete x m
                    putTMVar p $ Just r

-- | Returns Nothing if did not receive response, could not parse it, or
-- request is a notification. Just Left contains the error object returned
-- by server if any. Just Right means response was received just right.
sendRequest :: (MonadLoggerIO m , ToJSON q, ToRequest q, FromResponse r)
            => q -> JSONRPCT m (Maybe (Either ErrorObj r))
sendRequest q = head `liftM` sendBatchRequest [q]

-- | Send multiple requests in a batch. If only a single request, do not
-- put it in a batch.
sendBatchRequest :: (MonadLoggerIO m, ToJSON q, ToRequest q, FromResponse r)
                 => [q] -> JSONRPCT m [Maybe (Either ErrorObj r)]
sendBatchRequest qs = do
    v <- reader rpcVer
    l <- reader lastId
    s <- reader sentReqs
    o <- reader outCh
    k <- reader dead
    aps <- liftIO . atomically $ do
        d <- readTVar k
        aps <- forM qs $ \q ->
            if requestIsNotif q
                then return (buildRequest v q undefined, Nothing)
                else do
                    p <- newEmptyTMVar
                    i <- succ <$> readTVar l
                    m <- readTVar s
                    unless d $ writeTVar s $ M.insert i p m
                    unless d $ writeTVar l i
                    if d
                        then return (buildRequest v q i, Nothing)
                        else return (buildRequest v q i, Just p)
        case map fst aps of
            []  -> return ()
            [a] -> unless d $ writeTBMChan o $ MsgRequest a
            as  -> unless d $ writeTBMChan o $ MsgBatch $ map MsgRequest as
        return aps
    if null aps
        then $(logDebug) "no responses pending"
        else $(logDebug) "listening for responses if pending"
    liftIO . atomically $ forM aps $ \(a, pM) ->
        case pM of
            Nothing -> return Nothing
            Just  p -> do
                rM <- takeTMVar p
                case rM of
                    Nothing -> return Nothing
                    Just r@Response{} ->
                        case fromResponse (getReqMethod a) r of
                            Nothing -> return Nothing
                            Just  x -> return $ Just $ Right x
                    Just e -> return $ Just $ Left $ getError e

-- | Receive requests from remote endpoint. Returns Nothing if incoming
-- channel is closed or has never been opened. Will reject incoming request
-- if sent in a batch.
receiveRequest :: MonadLoggerIO m => JSONRPCT m (Maybe Request)
receiveRequest = do
    bt <- receiveBatchRequest
    case bt of
        Nothing -> return Nothing
        Just (SingleRequest q) -> return $ Just q
        Just BatchRequest{} -> do
            v <- reader rpcVer
            let e = errorInvalid $ String "not accepting batches"
                m = OrphanError v e
            sendResponse m
            return Nothing

-- | Receive batch of requests. Will also accept single requests.
receiveBatchRequest :: MonadLoggerIO m => JSONRPCT m (Maybe BatchRequest)
receiveBatchRequest = do
    chM <- reader reqCh
    case chM of
        Just ch -> do
            $(logDebug) "listening for a new request"
            liftIO . atomically $ readTBMChan ch
        Nothing -> do
            $(logError) "ignoring requests from remote endpoint"
            return Nothing

-- | Send response message. Do not use to respond to a batch of requests.
sendResponse :: MonadLoggerIO m => Response -> JSONRPCT m ()
sendResponse r = do
    o <- reader outCh
    liftIO . atomically . writeTBMChan o $ MsgResponse r

-- | Send batch of responses. Use to respond to a batch of requests.
sendBatchResponse :: MonadLoggerIO m => BatchResponse -> JSONRPCT m ()
sendBatchResponse (BatchResponse rs) = do
    o <- reader outCh
    liftIO . atomically . writeTBMChan o $ MsgBatch $ map MsgResponse rs
sendBatchResponse (SingleResponse r) = do
    o <- reader outCh
    liftIO . atomically . writeTBMChan o $ MsgResponse r

-- | Send any message. Do not use this. Use the other high-level functions
-- instead. Will not track request ids. Incoming responses to requests sent
-- using this method will be ignored.
sendMessage :: MonadLoggerIO m => Message -> JSONRPCT m ()
sendMessage msg = reader outCh >>= liftIO . atomically . (`writeTBMChan` msg)

-- | Create JSON-RPC session around conduits from transport layer.  When
-- context exits session disappears.
runJSONRPCT ::
       (MonadLoggerIO m, MonadUnliftIO m)
    => Ver -- ^ JSON-RPC version
    -> Bool -- ^ Ignore incoming requests/notifs
    -> ConduitT ByteString Void m () -- ^ Sink to send messages
    -> ConduitT () ByteString m () -- ^ Source to receive messages from
    -> JSONRPCT m a -- ^ JSON-RPC action
    -> m a -- ^ Output of action
runJSONRPCT ver ignore snk src f = do
    qs <- liftIO . atomically $ initSession ver ignore
    let inSnk  = sinkTBMChan (inCh qs)
        outSrc = sourceTBMChan (outCh qs)
    withAsync (runConduit $ src .| decodeConduit ver .| inSnk) $ const $
        withAsync (runConduit $ outSrc .| encodeConduit .| snk) $ \o ->
            withAsync (runReaderT processIncoming qs) $ const $ do
                a <- runReaderT f qs
                liftIO $ do
                    atomically . closeTBMChan $ outCh qs
                    _ <- wait o
                    return a


cr :: Monad m => ConduitT ByteString ByteString m ()
cr = CL.map (`B8.snoc` '\n')

--
-- Transports
--

-- | TCP client transport for JSON-RPC.
jsonrpcTCPClient
    :: (MonadLoggerIO m, MonadUnliftIO m)
    => Ver            -- ^ JSON-RPC version
    -> Bool           -- ^ Ignore incoming requests or notifications
    -> ClientSettings -- ^ Connection settings
    -> JSONRPCT m a   -- ^ JSON-RPC action
    -> m a            -- ^ Output of action
jsonrpcTCPClient ver ignore cs f = runGeneralTCPClient cs $ \ad ->
    runJSONRPCT ver ignore (cr .| appSink ad) (appSource ad) f

-- | TCP server transport for JSON-RPC.
jsonrpcTCPServer
    :: (MonadLoggerIO m, MonadUnliftIO m)
    => Ver             -- ^ JSON-RPC version
    -> Bool            -- ^ Ignore incoming requests or notifications
    -> ServerSettings  -- ^ Connection settings
    -> JSONRPCT m ()   -- ^ Action to perform on connecting client thread
    -> m a
jsonrpcTCPServer ver ignore ss f = runGeneralTCPServer ss $ \cl ->
    runJSONRPCT ver ignore (cr .| appSink cl) (appSource cl) f
