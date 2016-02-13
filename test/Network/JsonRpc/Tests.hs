{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Network.JsonRpc.Tests (tests) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.TMChan
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.Either
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Word
import System.IO
import Network.JsonRpc
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests =
    [ testGroup "Messages"
        [ testProperty "Check request fields"
            (reqFields :: Request -> Bool)
        , testProperty "Encode/decode request"
            (testEncodeDecode :: Request -> Bool)
        , testProperty "Check response fields"
            (resFields :: Response -> Bool)
        , testProperty "Encode/decode response"
            (testEncodeDecode :: Response -> Bool)
        , testProperty "Encode/decode message"
            (testEncodeDecode :: Message -> Bool)
        ]
    , testGroup "Conduits"
        [ testProperty "Encoding" testEncodeConduit
        , testProperty "Decoding" testDecodeConduit
        , testProperty "Decode Garbage" testDecodeGarbageConduit
        ]
    , testGroup "Functions"
        [ testProperty "Processing incoming messages" testProcessIncoming
        , testProperty "Processing incoming responses" testIncomingResponse
        , testProperty "Receiving incoming requests" testReceiveRequest
        , testProperty "Receiving incoming batch requests"
            testReceiveBatchRequest
        , testProperty "Sending responses" testSendResponse
        , testProperty "Sending batch responses" testSendBatchResponse
        , testProperty "Sending request batch" testSendBatchRequest
        ]
    , testGroup "Network"
        [ testProperty "Client/server talk" clientTest
        , testProperty "Notifications" notifTest
        ]
    ]

data ReqList = ReqList { getReqList :: [Req] } deriving (Show, Eq)

instance Arbitrary ReqList where
    arbitrary = resize 3 $ ReqList <$> arbitrary

data Req = Req { getReq :: Request } deriving (Show, Eq)

instance Arbitrary Req where
    arbitrary = do
        rq <- Request <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        return $ Req rq

data ReqNot = ReqNotReq { getReqNotParams :: Value }
            | ReqNotNot { getReqNotParams :: Value }
            deriving (Show, Eq)

instance ToJSON ReqNot where
    toJSON = toJSON . getReqNotParams

instance Arbitrary ReqNot where
    arbitrary = oneof [ ReqNotReq <$> arbitrary , ReqNotNot <$> arbitrary ]

instance ToRequest ReqNot where
    requestMethod ReqNotReq{} = "request"
    requestMethod ReqNotNot{} = "notification"
    requestIsNotif ReqNotReq{} = False
    requestIsNotif ReqNotNot{} = True

data Struct = Struct Value deriving (Show, Eq)

instance Arbitrary Struct where
    arbitrary = resize 10 $ Struct <$> oneof [lsn, objn] where
        nonull = oneof
            [ toJSON <$> (arbitrary :: Gen String)
            , toJSON <$> (arbitrary :: Gen Int)
            , toJSON <$> (arbitrary :: Gen Double)
            , toJSON <$> (arbitrary :: Gen Bool)
            ]
        val = oneof [ nonull, return Null ]
        ls   = toJSON <$> listOf val
        obj  = toJSON . M.fromList <$> listOf ps
        ps   = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls]
        lsn  = toJSON <$> listOf (oneof [ls, obj, val])
        objn = toJSON . M.fromList <$> listOf psn
        psn  = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls, obj]

reqFields :: Request -> Bool
reqFields rq = testFields (checkFieldsReq rq) rq

resFields :: Response -> Bool
resFields rs = testFields (checkFieldsRes rs) rs

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = undefined

checkVerId :: Ver -> Maybe Id -> Object -> Parser Bool
checkVerId ver i o = do
    j <- o .:? "jsonrpc"
    guard $ if ver == V2 then j == Just (String "2.0") else isNothing j
    o .:? "id" >>= guard . (==i)
    return True

checkFieldsReqNotif
    :: Ver -> Method -> Value -> Maybe Id -> Object -> Parser Bool
checkFieldsReqNotif ver m v i o = do
    checkVerId ver i o >>= guard
    o .: "method" >>= guard . (==m)
    o .: "params" >>= guard . (==v)
    return True

checkFieldsReq :: Request -> Object -> Parser Bool
checkFieldsReq (Request ver m v i) = checkFieldsReqNotif ver m v (Just i)
checkFieldsReq (Notif   ver m v)   = checkFieldsReqNotif ver m v Nothing

checkFieldsRes :: Response -> Object -> Parser Bool
checkFieldsRes (Response ver v i) o = do
    checkVerId ver (Just i) o >>= guard
    o .: "result" >>= guard . (==v)
    return True
checkFieldsRes (ResponseError ver e i) o = do
    checkVerId ver (Just i) o >>= guard
    o .: "error" >>= guard . (==e)
    return True
checkFieldsRes (OrphanError ver e) o = do
    checkVerId ver Nothing o >>= guard
    o .: "error" >>= guard . (==e)
    return True

testFields :: ToJSON r => (Object -> Parser Bool) -> r -> Bool
testFields ck r = fromMaybe False . parseMaybe f $ toJSON r where
    f = withObject "json" ck

testEncodeDecode :: (Eq r, ToJSON r, FromJSON r) => r -> Bool
testEncodeDecode r = maybe False (==r) $ parseMaybe parseJSON (toJSON r)

testEncodeConduit :: [Message] -> Property
testEncodeConduit msgs = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        bs <- CL.sourceList msgs =$ encodeConduit $$ CL.consume
        return $ map decodeStrict' bs
    assert $ msgs == map fromJust rt

testDecodeConduit :: ([Message], Ver) -> Property
testDecodeConduit (msgs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $
        CL.sourceList (buffer encoded) =$ decodeConduit ver $$ CL.consume
    assert $ rt == map (Right . toJSON) msgs
  where
    encoded = B.concat $ map (L.toStrict . encode) msgs
    buffer bs | B.null bs = []
              | otherwise = B.take 16 bs : buffer (B.drop 16 bs)

testDecodeGarbageConduit :: ([[Word8]], Ver) -> Property
testDecodeGarbageConduit (ss, ver) = monadicIO $ do
    _ <- run $ runNoLoggingT $
        CL.sourceList bss =$ decodeConduit ver $$ CL.consume
    -- Just getting here without crashing is enough
    assert True
  where
    bss = map B.pack ss

testProcessIncoming :: ([Either Response Message], Ver, Bool)
                    -> Property
testProcessIncoming (msgs, ver, ignore) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        expect <- liftIO . atomically $ newTMChan
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (cli qs expect msgs) $ \c -> do
            runReaderT processIncoming qs
            wait c
        validate expect True
    assert rt
  where
    validate expect state = do
        newM <- liftIO . atomically $ readTMChan expect
        case newM of
            Just new -> validate expect $ new && state
            Nothing  -> return state
    cli qs expect [] = liftIO . atomically $ do
        closeTMChan expect
        closeTBMChan $ inCh qs
    cli qs expect (x:xs) = do
        liftIO . atomically $ writeTBMChan (inCh qs) (toJSON <$> x)
        liftIO . atomically $ case x of
            Left e ->
                caseLeftResponse qs e >>= writeTMChan expect
            Right (MsgRequest req@Request{}) ->
                caseSingleRequest req qs >>= writeTMChan expect
            Right (MsgRequest nt@Notif{}) ->
                caseSingleNotif nt qs >>= writeTMChan expect
            Right MsgResponse{} ->
                writeTMChan expect True
            Right (MsgBatch []) ->
                caseEmptyBatch qs >>= writeTMChan expect
            Right (MsgBatch ms) ->
                caseBatch qs ms >>= writeTMChan expect
        cli qs expect xs
    caseLeftResponse qs e = do
        m <- readTBMChan $ outCh qs
        case m of
            Just (MsgResponse err) -> return $ err == e
            _ -> return False
    caseEmptyBatch qs = do
        m <- readTBMChan $ outCh qs
        case m of
            Just (MsgResponse (OrphanError v (ErrorObj _ i _))) ->
                return $ v == rpcVer qs && i == (-32600)
            _ -> return False
    caseBatch qs ms = do
        let isrq (MsgRequest Request{}) = True
            isrq _ = False
            isnotif (MsgRequest Notif{}) = True
            isnotif _ = False
        if any isrq ms
            then
                if ignore
                    then do
                        m <- readTBMChan $ outCh qs
                        case m of
                            Just MsgBatch{} -> return True
                            _ -> return False
                    else do
                        m <- readTBMChan $ fromJust $ reqCh qs
                        case m of
                            Just BatchRequest{} -> return True
                            _ -> return False
            else
                if any isnotif ms
                    then
                        if ignore
                            then return True
                            else do
                                m <- readTBMChan $ fromJust $ reqCh qs
                                case m of
                                    Just BatchRequest{} -> return True
                                    _ -> return False
                    else return True
    caseSingleRequest req qs =
        if ignore
            then do
                m <- readTBMChan $ outCh qs
                case m of
                    Just (MsgResponse (ResponseError _ e@ErrorObj{} _)) ->
                        return $ getErrCode e == (-32601)
                    _ -> return False
            else do
                m <- readTBMChan (fromJust $ reqCh qs)
                return $ m == Just (SingleRequest req)
    caseSingleNotif nt qs =
        if ignore
            then return True
            else do
                m <- readTBMChan (fromJust $ reqCh qs)
                return $ m == Just (SingleRequest nt)

testIncomingResponse :: ([ReqList], Ver, Bool) -> Property
testIncomingResponse (reqss', ver, ignore) = nodup ==> monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        expect <- liftIO . atomically $ newTMChan
        qs <- liftIO . atomically $ do
            qs <- initSession ver ignore
            s <- sent
            writeTVar (sentReqs qs) s
            return qs
        withAsync (cli qs expect msgs) $ \c -> do
            runReaderT processIncoming qs
            wait c
        valid <- validate expect responses True
        mapempty <- liftIO . atomically $ M.null <$> readTVar (sentReqs qs)
        return $ valid && mapempty
    assert rt
  where
    reqss = map getReqList reqss'
    reqs = let f [q] = SingleRequest (getReq q)
               f qs  = BatchRequest (map getReq qs)
           in map f reqss
    flatreqs = let f (BatchRequest bt) = bt
                   f (SingleRequest rt) = [rt]
               in concatMap f reqs
    nodup = length (nubBy ((==) `on` getReqId) flatreqs) == length flatreqs
    respond (Request v _ p i) = Response v p i
    respond _ = undefined
    responses = map respond flatreqs
    msgs = map MsgResponse responses
    sent = do
        promises <- forM flatreqs $
            \Request{getReqId = i} -> (,) i <$> newEmptyTMVar
        return $ M.fromList promises
    validate _ [] state = return state
    validate expect (x:xs) state = do
        newM <- liftIO . atomically $ readTMChan expect
        case newM of
            Just new -> validate expect xs (new == x && state)
            Nothing  -> return state
    cli qs expect [] = liftIO . atomically $ do
        closeTMChan expect
        closeTBMChan $ inCh qs
    cli qs expect (x:xs) =
        case x of
            MsgResponse res -> do
                p <- getpromise qs res
                liftIO . atomically $ writeTBMChan (inCh qs) (Right $ toJSON x)
                liftIO . atomically $ do
                    rE <- readTMVar p
                    F.forM_ rE $ writeTMChan expect
                cli qs expect xs
            MsgBatch resps -> do
                ps <- mapM (getpromise qs . getMsgResponse) resps
                liftIO . atomically $ writeTBMChan (inCh qs) (Right $ toJSON x)
                forM_ ps $ \p -> liftIO . atomically $ do
                    rE <- readTMVar p
                    F.forM_ rE $ writeTMChan expect
            _ -> undefined
    getpromise qs res = do
        let i = getResId res
        liftIO . atomically $ do
            snt <- readTVar (sentReqs qs)
            return . fromJust $ M.lookup i snt

testSendBatchRequest :: ([(ReqNot, Either ErrorObj Value)], Ver, Bool)
                     -> Property
testSendBatchRequest (reqnotres, ver, ignore) = nonull ==> monadicIO $ do
    rs <- run $ runNoLoggingT $ do
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (cli qs) $ \c -> do
            rs <- runReaderT (sendBatchRequest $ map fst reqnotres) qs
            wait c
            return (rs :: [Maybe (Either ErrorObj Value)])
    assert $ length rs == length reqnotres
  where
    nonull = not $ null reqnotres
    resmap = M.fromList $ map (first toJSON) reqnotres
    respond (Request v _ _ i) (Left  y) = Just $ ResponseError v y i
    respond (Request v _ _ i) (Right y) = Just $ Response      v y i
    respond _ _ = undefined
    fulfill req sent = F.forM_ (getReqId req `M.lookup` sent) $ \p ->
        putTMVar p $ respond req $ fromJust $ getReqParams req `M.lookup` resmap
    cli qs = liftIO . atomically $ do
        msg <- readTBMChan (outCh qs)
        sent <- readTVar (sentReqs qs)
        case msg of
            Just (MsgRequest req@Request{}) -> fulfill req sent
            Just (MsgBatch bt) ->
                forM_ bt $ \m ->
                    case m of
                        MsgRequest req@Request{} -> fulfill req sent
                        _ -> return ()
            _ -> return ()

testReceiveRequest :: ([BatchRequest], Ver, Bool) -> Property
testReceiveRequest (reqs, ver, ignore) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (send qs) $ \c -> do
            res <- runReaderT receive qs
            wait c
            return res
    assert $ and rt
  where
    send qs = F.forM_ (reqCh qs) $ \qch -> do
        forM_ reqs $ liftIO . atomically . writeTBMChan qch
        liftIO . atomically $ closeTBMChan qch
    receive = forM reqs $ \q -> do
        t <- receiveRequest
        if ignore
            then return $ isNothing t
            else
                case q of
                    SingleRequest{} ->
                        return $ Just q == fmap SingleRequest t
                    BatchRequest{} -> do
                        ch <- reader outCh
                        m <- liftIO . atomically $ readTBMChan ch
                        case m of
                            Just (MsgResponse OrphanError{}) -> 
                                return $ isNothing t
                            _ -> return $ isNothing t


testReceiveBatchRequest :: ([BatchRequest], Ver, Bool) -> Property
testReceiveBatchRequest (reqs, ver, ignore) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (send qs) $ \c -> do
            rt <- runReaderT receive qs
            wait c
            return rt
    if ignore
        then assert $ all isNothing rt
        else assert $ all isJust rt && map fromJust rt == reqs
  where
    send qs = F.forM_ (reqCh qs) $ \qch -> do
        forM_ reqs $ liftIO . atomically . writeTBMChan qch
        liftIO . atomically $ closeTBMChan qch
    receive = forM reqs $ const receiveBatchRequest

testSendResponse :: ([Response], Ver, Bool) -> Property
testSendResponse (responses, ver, ignore) = monadicIO $ do
    ex <- run $ runNoLoggingT $ do
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (receive qs) $ \c -> do
            runReaderT send qs
            wait c
    assert $ length ex == length responses
    assert $ all isJust ex && map fromJust ex == map MsgResponse responses
  where
    send = forM_ responses sendResponse
    receive qs = forM responses $ const $ liftIO . atomically $
        readTBMChan (outCh qs)

testSendBatchResponse :: ([BatchResponse], Ver, Bool) -> Property
testSendBatchResponse (responses, ver, ignore) = monadicIO $ do
    ex <- run $ runNoLoggingT $ do
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (receive qs) $ \c -> do
            runReaderT send qs
            wait c
    assert $ length ex == length responses
    assert $ all isJust ex && map fromJust ex == map msg responses
  where
    msg (BatchResponse bt) = MsgBatch $ map MsgResponse bt
    msg (SingleResponse r) = MsgResponse r
    send = forM_ responses sendBatchResponse
    receive qs = forM responses $ const $ liftIO . atomically $
        readTBMChan (outCh qs)

createChans :: MonadIO m
            => m ((TBMChan a, TBMChan b), (Sink a m (), Source m b))
createChans = do
    (bso, bsi) <- liftIO . atomically $ (,) <$> newTBMChan 16 <*> newTBMChan 16
    let (snk, src) = (sinkTBMChan bso False, sourceTBMChan bsi)
    return ((bso, bsi), (snk, src))

clientTest :: ([Value], Ver) -> Property
clientTest (qs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        ((bso, bsi), (snk, src)) <- createChans
        let csnk = sinkTBMChan bsi False :: Sink B.ByteString (NoLoggingT IO) ()
            csrc = sourceTBMChan bso :: Source (NoLoggingT IO) B.ByteString
        withAsync (server snk src) $ const $ cli csnk csrc
    assert $ length rt == length qs
    assert $ null rt || all correct rt
    assert $ qs == results rt
  where
    respond q = return $ Right (q :: Value)
    server snk src = runJsonRpcT ver False snk src (srv respond)
    cli snk src = runJsonRpcT ver True snk src . forM qs $ sendRequest
    results = map $ fromRight . fromJust
    correct (Just (Right _)) = True
    correct _ = False

notifTest :: ([Request], Ver) -> Property
notifTest (qs, ver) = monadicIO $ do
    nt <- run $ runNoLoggingT $ do
        ((bso, bsi), (snk, src)) <- createChans
        let csnk = sinkTBMChan bsi False
            csrc = sourceTBMChan bso
        (sig, notifs) <- liftIO . atomically $
            (,) <$> newEmptyTMVar <*> newTVar []
        withAsync (server snk src sig notifs) $ const $ cli sig csnk csrc
        liftIO . atomically $ readTVar notifs
    assert $ length nt == length ntfs
    assert $ reverse nt == ntfs
  where
    respond q = return $ Right (q :: Value)
    server snk src sig notifs =
        runJsonRpcT ver False snk src (process sig notifs)
    process sig notifs = do
        qM <- receiveRequest
        case qM of
            Nothing -> return ()
            Just q -> do
                case q of
                    Notif{} -> liftIO . atomically $
                        readTVar notifs >>= writeTVar notifs . (q:)
                    Request{ getReqParams = String "disconnect" } ->
                        liftIO . atomically $ putTMVar sig ()
                    Request{} -> return ()
                rM <- buildResponse respond q
                case rM of
                    Nothing -> process sig notifs
                    Just  r -> sendResponse r >> process sig notifs
    reqs = map MsgRequest qs
    cli sig snk src = runJsonRpcT ver True snk src $ do
        forM_ reqs sendMessage
        _ <- sendRequest $ String "disconnect"
            :: JsonRpcT (NoLoggingT IO) (Maybe (Either ErrorObj Value))
        liftIO . atomically $ takeTMVar sig
    ntfs = flip filter qs $ \q -> case q of Notif{} -> True; _ -> False

srv :: (MonadLoggerIO m, FromRequest q, ToJSON r)
    => Respond q (JsonRpcT m) r -> JsonRpcT m ()
srv respond = do
    qM <- receiveRequest
    case qM of
        Nothing -> return ()
        Just q -> do
            rM <- buildResponse respond q
            case rM of
                Nothing -> srv respond
                Just r -> sendResponse r >> srv respond
