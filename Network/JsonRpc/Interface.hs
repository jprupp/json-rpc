{-# LANGUAGE OverloadedStrings #-}
-- | Interface for JSON-RPC.
-- Types: qo = outgoing request,
--        ri = incoming response,
--        ni = incoming notification,
--        qi = incoming request,
--        ro = outgoing response,
--        no = outgoing notification.
module Network.JsonRpc.Interface
( Respond
, JsonRpcT
, IncomingMsg
, runJsonRpcT
, sendRequest
, sendNotif
, receive
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

-- | Incoming notifications or errors.
type IncomingMsg = Either ErrorObj

-- | Responder for incoming requests
type Respond qi ro = qi -> IO (Either ErrorObj ro)

-- | Conduits for sending and receiving JSON-RPC messages.
data AppChans qo ri ni qi ro no =
    AppChans { inCh    :: TBMChan (IncomingMsg ni)
             , outCh   :: TBMChan (OutgoingMsg qo ri ro no)
             , rpcVer  :: Ver
             }

type JsonRpcT qo ri ni qi ro no = ReaderT (AppChans qo ri ni qi ro no)

-- | Map of ids to corresponding requests and promises of responses.
type SentRequests qo ri = HashMap Id (Request qo, TMVar (Either ErrorObj ri))

-- | Outgoing messages.  Pack a request with a promise for corresponding
-- response.
data OutgoingMsg qo ri ro no
    = OutgoingReq !(Request qo) !(TMVar (Either ErrorObj ri))
    | OutgoingRes !(Response ro)
    | OutgoingNotif !(Notif no)
    | OutgoingError !ErrorMsg

instance (ToJSON qo, ToJSON ro, ToJSON no) => ToJSON (OutgoingMsg qo ri ro no)
  where
    toJSON (OutgoingReq q _) = toJSON q
    toJSON (OutgoingRes   r) = toJSON r
    toJSON (OutgoingNotif n) = toJSON n
    toJSON (OutgoingError e) = toJSON e

-- | JSON-RPC session mutable data.
data Session qo ri = Session
    { lastId        :: TVar Id                     -- ^ Last generated id
    , sentRequests  :: TVar (SentRequests qo ri)   -- ^ Map of ids to promises
    }

-- | Initialize JSON-RPC session.
initSession :: STM (Session qo ri)
initSession = Session <$> newTVar (IdInt 0) <*> newTVar M.empty

-- | Conduit that serializes JSON documents for sending to the network.
encodeConduit :: (ToJSON a, Monad m) => Conduit a m ByteString
encodeConduit = CL.map (L8.toStrict . encode)

-- | Conduit for outgoing JSON-RPC messages.
-- Adds an id to requests whose id is ‘IdNull’.
-- Tracks all sent requests to match corresponding responses.
outConduit :: MonadIO m
           => Session qo ri
           -> Conduit (OutgoingMsg qo ri ro no) m (OutgoingMsg qo ri ro no)
outConduit qs = await >>= \nqM -> case nqM of
    Nothing -> return ()
    Just (OutgoingReq q p) -> do
        msg <- liftIO . atomically $ addReq q p
        yield msg
        outConduit qs
    Just msg -> yield msg >> outConduit qs
  where
    addReq q p = case getReqId q of
        IdNull -> do
            i <- readTVar $ lastId qs
            h <- readTVar $ sentRequests qs
            let i'  = succ i
                q' = q { getReqId = i' }
                h'  = M.insert i' (q', p) h
            writeTVar (lastId qs) i'
            writeTVar (sentRequests qs) h'
            return (OutgoingReq q' p)
        i -> do
            h <- readTVar (sentRequests qs)
            writeTVar (sentRequests qs) (M.insert i (q, p) h)
            return (OutgoingReq q p)

-- | Conduit to decode incoming JSON-RPC messages.
inConduit :: (FromRequest qi, FromResponse ri, FromNotif ni, MonadIO m)
          => Ver                          -- ^ JSON-RPC version
          -> Session qo ri                -- ^ JSON-RPC session
          -> Respond qi ro
          -> TBMChan (OutgoingMsg qo ri ro no)
          -> Conduit ByteString m (IncomingMsg ni)
          -- ^ Decoded incoming messages
inConduit ver qs respond out = evalStateT loop Nothing where
    loop = lift await >>= maybe flush process where
        flush = get >>= \kM -> maybe (return ()) (\k -> handle (k B.empty)) kM
        process = runParser >=> handle
        runParser ck = maybe (parse json' ck) ($ ck) <$> get <* put Nothing

        handle (Fail {}) = do
            let res = OutgoingError $ ErrorMsg ver (errorParse Null) IdNull
            liftIO . atomically $ writeTBMChan out res
            loop
        handle (Partial k) = put (Just k) >> loop
        handle (Done rest v) = do
            msgM <- liftIO . atomically $ decodeSTM v
            case msgM of
                Nothing -> return ()
                Just (MsgRequest (Request w _ q i)) -> liftIO $ do
                    rE <- respond q
                    let res = case rE of
                            Left e -> OutgoingError $ ErrorMsg w e i
                            Right r -> OutgoingRes $ Response w r i
                    atomically $ writeTBMChan out res
                Just (MsgError e) ->
                    lift . yield . Left $ getErrObj e
                Just (MsgNotif n) ->
                    lift . yield . Right $ getNotifParams n
                _ -> undefined
            if B.null rest then loop else process rest

    decodeSTM v = do
        h <- readTVar (sentRequests qs)
        case parseEither (topParse h) v of
            Left s -> do
                writeTBMChan out $ OutgoingError $
                    ErrorMsg ver (errorParse $ toJSON s) IdNull
                return Nothing
            Right x -> case x of
                Right (MsgResponse rs) -> do
                    p <- promiseSTM h (getResId rs)
                    putTMVar p . Right $ getResult rs
                    return Nothing
                Right e@(MsgError m) -> case getErrId m of
                    IdNull -> return $ Just e
                    i -> do p <- promiseSTM h i
                            putTMVar p (Left $ getErrObj m)
                            return Nothing
                Right n@(MsgNotif   _) -> return $ Just n
                Right q@(MsgRequest _) -> return $ Just q
                Left e -> writeTBMChan out (OutgoingError e) >> return Nothing
      where
        promiseSTM h i = do
            let (_, p) = fromJust $ i `M.lookup` h
            writeTVar (sentRequests qs) (M.delete i h)
            return p

        topParse h w  =  parseReq w
                     <|> parseNot w
                     <|> parseRes h w
                     <|> return (Left $ ErrorMsg ver (errorInvalid w) IdNull)

        parseReq w = flip fmap (parseRequest w) $ \rqE -> case rqE of
            Left   e -> Left e
            Right rq -> Right $ MsgRequest rq

        parseNot w = flip fmap (parseNotif w) $ \rnE -> case rnE of
            Left   e -> Left e
            Right rn -> Right $ MsgNotif rn

        parseRes h = withObject "response" $ \o -> do
            r <- o .:? "result" .!= Null
            e <- o .:? "error"  .!= Null
            when (r == Null && e == Null) mzero
            i <- o .:? "id" .!= IdNull
            j <- o .:? "jsonrpc"
            let ver' = if j == Just ("2.0" :: Text) then V2 else V1
            case i of
                IdNull -> Right . MsgError <$> parseJSON (Object o)
                _ -> case i `M.lookup` h of
                    Nothing -> return . Left $ ErrorMsg ver' (errorId i) i
                    Just (rq, _) -> do
                        rsE <- parseResponse rq (Object o)
                        return $ case rsE of
                            Left  er -> Right $ MsgError    er
                            Right rs -> Right $ MsgResponse rs

sendRequest :: (ToJSON qo, ToRequest qo, FromResponse ri, MonadIO m)
            => qo
            -> JsonRpcT qo ri ni qi ro no m (STM (Either ErrorObj ri))
sendRequest q = do
    o <- reader outCh
    v <- reader rpcVer
    let req = buildRequest v q
    p <- liftIO . atomically $ do
        p <- newEmptyTMVar 
        writeTBMChan o $ OutgoingReq req p
        return p
    return $ takeTMVar p

sendNotif :: (ToJSON no, ToNotif no, Monad m)
          => no
          -> JsonRpcT qo ri ni qi ro no m (STM ())
sendNotif n = do
    o <- reader outCh
    v <- reader rpcVer
    let notif = buildNotif v n
    return $ writeTBMChan o (OutgoingNotif notif)

-- | Receive requests or notifications from peer.
-- Returns Nothing if incoming channel is closed and empty.
receive :: Monad m
        => JsonRpcT qo ri ni qi ro no m (STM (Maybe (IncomingMsg ni)))
receive = liftM readTBMChan $ reader inCh

-- | Will create JSON-RPC conduits around 'ByteString' conduits from
-- a transport layer.
runJsonRpcT :: ( FromRequest qi, FromNotif ni, FromResponse ri
               , ToJSON qo, ToJSON no, ToJSON ro )
            => Ver                             -- ^ JSON-RPC version
            -> Sink ByteString IO ()           -- ^ Sink to send messages
            -> Source IO ByteString            -- ^ Source of incoming messages
            -> Respond qi ro                   -- ^ Respond to incoming requests
            -> JsonRpcT qo ri ni qi ro no IO a -- ^ JSON-RPC action
            -> IO a                            -- ^ Output of action
runJsonRpcT ver snk src respond f = do
    (qs, ac) <- liftIO . atomically $ do
        qs <- initSession
        ac <- AppChans <$> newTBMChan 128 <*> newTBMChan 128 <*> return ver
        return (qs, ac)
    let inSnk  = sinkTBMChan (inCh ac) True
        outSrc = sourceTBMChan (outCh ac)
    withAsync (inThread qs inSnk $ outCh ac) $ const $
        withAsync (outThread qs outSrc) $ const $ runReaderT f ac
  where
    inThread qs i o = src $= inConduit ver qs respond o $$ i
    outThread qs o = o $= outConduit qs $= encodeConduit $$ snk

-- | JSON-RPC-over-TCP client.
jsonRpcTcpClient
    :: ( FromRequest qi, FromNotif ni, FromResponse ri
       , ToJSON qo, ToJSON no, ToJSON ro )
    => Ver                             -- ^ JSON-RPC version
    -> ClientSettings                  -- ^ Connection settings
    -> Respond qi ro                   -- ^ Respond to incoming requests
    -> JsonRpcT qo ri ni qi ro no IO a -- ^ JSON-RPC action
    -> IO a                            -- ^ Output of action
jsonRpcTcpClient ver cs respond f = runTCPClient cs $ \ad ->
    runJsonRpcT ver (cr =$ appSink ad) (appSource ad $= ln) respond f
  where
    cr = CL.map (`B8.snoc` '\n')
    ln = await >>= \bsM -> case bsM of
        Nothing -> return ()
        Just bs -> let (l, ls) = B8.break (=='\n') bs in case ls of
            "" -> await >>= \bsM' -> case bsM' of
                Nothing  -> unless (B8.null l) $ yield l
                Just bs' -> leftover (bs `B8.append` bs') >> ln
            _  -> case l of
                "" -> leftover (B8.tail ls) >> ln
                _  -> leftover (B8.tail ls) >> yield l >> ln

-- | JSON-RPC-over-TCP server.
jsonRpcTcpServer
    :: ( FromRequest qi, FromNotif ni, FromResponse ri
       , ToJSON qo, ToJSON no, ToJSON ro )
    => Ver                        -- ^ JSON-RPC version
    -> ServerSettings             -- ^ Connection settings
    -> Respond qi ro                   -- ^ Respond to incoming requests
    -> JsonRpcT qo ri ni qi ro no IO ()
    -- ^ JSON-RPC action to perform on connecting client thread
    -> IO ()
jsonRpcTcpServer ver ss r f = runTCPServer ss $ \cl ->
    runJsonRpcT ver (CL.map (`B8.snoc` '\n') =$ appSink cl) (appSource cl) r f

-- | Dummy server when not expecting client to send anything but requests.
dummySrv :: (MonadIO m) => JsonRpcT qo ri () qi ro no m ()
dummySrv = forever $ receive >>= liftIO . atomically
