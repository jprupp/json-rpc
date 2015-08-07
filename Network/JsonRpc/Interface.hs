{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Interface for JSON-RPC.
module Network.JsonRpc.Interface
( -- * Establish JSON-RPC context
  JsonRpcT
, runJsonRpcT

  -- * Conduits for encoding/decoding
, decodeConduit
, encodeConduit

  -- * Communicate with remote party
, receiveRequest
, sendResponse
, sendRequest

  -- * Transports
  -- ** Client
, jsonRpcTcpClient
  -- ** Server
, jsonRpcTcpServer

  -- * Internal data and functions
, SentRequests
, Session(..)
, initSession
, processIncoming
) where

import Control.Applicative
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
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
import Network.JsonRpc.Data

type SentRequests = HashMap Id (TMVar (Maybe Response))

data Session = Session { inCh     :: TBMChan (Either Response Message)
                       , outCh    :: TBMChan Message
                       , reqCh    :: Maybe (TBMChan Request)
                       , lastId   :: TVar Id
                       , sentReqs :: TVar SentRequests
                       , rpcVer   :: Ver
                       }

-- Context for JSON-RPC connection. Connection will remain active as long
-- as context is maintaned.
type JsonRpcT = ReaderT Session

initSession :: Ver -> Bool -> STM Session
initSession v ignore =
    Session <$> newTBMChan 128
            <*> newTBMChan 128
            <*> (if ignore then return Nothing else Just <$> newTBMChan 128)
            <*> newTVar (IdInt 0)
            <*> newTVar M.empty
            <*> return v

encodeConduit :: MonadLogger m => Conduit Message m ByteString
encodeConduit = CL.mapM $ \m -> do
    $(logDebug) $ T.pack $ unwords $ case m of
        MsgRequest Request{getReqId = i} ->
            [ "encoding request id:", fromId i ]
        MsgRequest Notif{} ->
            [ "encoding notification" ]
        MsgResponse Response{getResId = i} ->
            [ "encoding response id:", fromId i ]
        MsgResponse ResponseError{getResId = i} ->
            [ "encoding error id:", fromId i ]
        MsgResponse OrphanError{} ->
            [ "encoding error without id" ]
    return . L8.toStrict $ encode m

decodeConduit :: MonadLogger m
              => Ver -> Conduit ByteString m (Either Response Message)
decodeConduit ver = evalStateT loop Nothing where
    loop = lift await >>= maybe flush process
    flush = get >>= maybe (return ()) (handle True . ($ B8.empty))
    process = runParser >=> handle False
    runParser ck = maybe (parse json' ck) ($ ck) <$> get <* put Nothing

    handle True (Fail "" _ _) =
        $(logDebug) "ignoring null string at end of incoming data"
    handle _ (Fail i _ _) = do
        $(logError) "error parsing incoming message"
        lift . yield . Left $ OrphanError ver (errorParse i)
        loop
    handle _ (Partial k) = put (Just k) >> loop
    handle _ (Done rest v) = do
        let msg = decod v
        when (isLeft msg) $ $(logError) "received invalid message"
        lift $ yield msg
        if B8.null rest then loop else process rest

    decod v = case parseMaybe parseJSON v of
        Just msg -> Right msg
        Nothing -> Left $ OrphanError ver (errorInvalid v)

processIncoming :: (Functor m, MonadLoggerIO m) => JsonRpcT m ()
processIncoming = do
    i  <- reader inCh
    o  <- reader outCh
    qM <- reader reqCh
    s  <- reader sentReqs
    join . liftIO . atomically $ readTBMChan i >>= \inc ->
        case inc of
            Nothing -> do
                m <- readTVar s
                mapM_ ((`putTMVar` Nothing) . snd) $ M.toList m
                return $ do
                    $(logDebug) "closed incoming channel"
                    unless (M.null m) $
                        $(logError) "some requests did not get responses"
                    return ()
            Just (Left e) -> do
                writeTBMChan o (MsgResponse e)
                return $ do
                    $(logError) "replied to sender with error"
                    processIncoming
            Just (Right (MsgRequest req)) ->
                case qM of
                    Just q -> do
                        writeTBMChan q req
                        return $ do
                            $(logDebug) "received request"
                            processIncoming
                    Nothing ->
                        case req of
                            Request v m _ d -> do
                                let e = ResponseError v (errorMethod m) d
                                writeTBMChan o (MsgResponse e)
                                return $ do
                                    $(logError) $ T.pack $ unwords
                                        [ "rejected incoming request id:"
                                        , fromId d
                                        ]
                                    processIncoming
                            Notif{} -> return $ do
                                $(logError) $ "rejected incoming notification"
                                processIncoming
            Just (Right (MsgResponse res)) -> do
                let hasId = case res of
                                Response{}      -> True
                                ResponseError{} -> True
                                OrphanError{}   -> False
                if hasId
                    then do
                        let x = getResId res
                        m <- readTVar s
                        let pM = x `M.lookup` m
                        case pM of
                            Nothing -> do
                                let v = getResVer res
                                    e = errorId x
                                    err = OrphanError v e
                                writeTBMChan o $ MsgResponse err
                                return $ do
                                    $(logError) $ T.pack $ unwords
                                        [ "got response with unknown id:"
                                        , fromId x
                                        ]
                                    processIncoming
                            Just p -> do
                                writeTVar s $ M.delete x m
                                putTMVar p $ Just res
                                return $ do
                                    $(logDebug) $ T.pack $ unwords
                                        [ "received response id:"
                                        , fromId x
                                        ]
                                    processIncoming
                    else return $ do
                        $(logError) $ T.pack $ unwords
                            [ "ignoring orhpan error:"
                            , fromError (getError res)
                            ]
                        processIncoming

-- | Returns Nothing if did not receive response, could not parse it, or
-- request was a notification. Just Left contains the error object returned
-- by server if any. Just Right means response was received just right.
sendRequest :: (MonadLoggerIO m, ToJSON q, ToRequest q, FromResponse r)
            => q -> JsonRpcT m (Maybe (Either ErrorObj r))
sendRequest q = do
    v <- reader rpcVer
    l <- reader lastId
    s <- reader sentReqs
    o <- reader outCh
    if requestIsNotif q
        then do
            $(logDebug) "sending notification"
            liftIO . atomically $ do
                let req = buildRequest v q undefined
                writeTBMChan o $ MsgRequest req
            $(logDebug) "notification sent"
            return Nothing
        else do
            $(logDebug) "sending request"
            p <- liftIO . atomically $ do
                p <- newEmptyTMVar 
                i <- succ <$> readTVar l
                m <- readTVar s
                let req = buildRequest v q i
                writeTVar s $ M.insert i p m
                writeTBMChan o $ MsgRequest req 
                writeTVar l i
                return p
            $(logDebug) "request sent, awaiting for response"
            liftIO . atomically $ takeTMVar p >>= \rM -> case rM of
                Nothing -> return Nothing
                Just y@Response{} ->
                    case fromResponse (requestMethod q) y of
                        Nothing -> return Nothing
                        Just x -> return . Just $ Right x
                Just e@ResponseError{} ->
                    return . Just $ Left $ getError e
                _ -> undefined

-- | Receive requests from remote endpoint. Returns Nothing if incoming
-- channel is closed or has never been opened.
receiveRequest :: MonadLoggerIO m => JsonRpcT m (Maybe Request)
receiveRequest = do
    chM <- reader reqCh
    case chM of
        Just ch -> do
            $(logDebug) "listening for a new request"
            liftIO . atomically $ readTBMChan ch
        Nothing -> do
            $(logError) "ignoring requests from remote endpoint"
            return Nothing

sendResponse :: MonadLoggerIO m => Response -> JsonRpcT m ()
sendResponse r = do
    o <- reader outCh
    liftIO . atomically . writeTBMChan o $ MsgResponse r

-- | Create JSON-RPC session around conduits from transport
-- layer. When context exits session disappears.
runJsonRpcT :: (MonadLoggerIO m, MonadBaseControl IO m)
            => Ver               -- ^ JSON-RPC version
            -> Bool              -- ^ Ignore incoming requests or notifications
            -> Sink Message m () -- ^ Sink to send messages
            -> Source m (Either Response Message)
            -- ^ Incoming messages or error responses to be returned to sender
            -> JsonRpcT m a      -- ^ JSON-RPC action
            -> m a               -- ^ Output of action
runJsonRpcT ver ignore snk src f = do
    qs <- liftIO . atomically $ initSession ver ignore
    let inSnk  = sinkTBMChan (inCh qs) True
        outSrc = sourceTBMChan (outCh qs)
    withAsync (src $$ inSnk) $ const $
        withAsync (outSrc $$ snk) $ const $
            withAsync (runReaderT processIncoming qs) $ const $
                runReaderT f qs


cr :: Monad m => Conduit ByteString m ByteString
cr = CL.map (`B8.snoc` '\n')

--
-- Transports
--

-- | TCP client transport for JSON-RPC.
jsonRpcTcpClient
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => Ver            -- ^ JSON-RPC version
    -> Bool           -- ^ Ignore incoming requests or notifications
    -> ClientSettings -- ^ Connection settings
    -> JsonRpcT m a   -- ^ JSON-RPC action
    -> m a            -- ^ Output of action
jsonRpcTcpClient ver ignore cs f = runGeneralTCPClient cs $ \ad ->
    runJsonRpcT ver ignore
        (encodeConduit =$ cr =$ appSink ad)
        (appSource ad $= decodeConduit ver) f

-- | TCP server transport for JSON-RPC.
jsonRpcTcpServer
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => Ver             -- ^ JSON-RPC version
    -> Bool            -- ^ Ignore incoming requests or notifications
    -> ServerSettings  -- ^ Connection settings
    -> JsonRpcT m ()   -- ^ Action to perform on connecting client thread
    -> m a
jsonRpcTcpServer ver ignore ss f = runGeneralTCPServer ss $ \cl ->
    runJsonRpcT ver ignore
        (encodeConduit =$ cr =$ appSink cl)
        (appSource cl $= decodeConduit ver) f
