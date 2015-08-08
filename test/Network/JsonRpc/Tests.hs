{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Network.JsonRpc.Tests (tests) where

import Control.Applicative
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
import Network.JsonRpc
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests =
    [ testGroup "JSON-RPC Requests"
        [ testProperty "Check fields"
            (reqFields :: Request -> Bool)
        , testProperty "Encode/decode"
            (testEncodeDecode :: Request -> Bool)
        ]
    , testGroup "JSON-RPC Responses"
        [ testProperty "Check fields"
            (resFields :: Response -> Bool)
        , testProperty "Encode/decode"
            (testEncodeDecode :: Response -> Bool)
        ]
    , testGroup "Conduits"
        [ testProperty "Encoding" testEncodeConduit
        , testProperty "Decoding" testDecodeConduit
        , testProperty "Decode Garbage" testDecodeGarbageConduit
        , testProperty "Decode Invalid" testDecodeInvalidConduit
        ]
    , testGroup "JSON-RPC monad"
        [ testProperty "Processing incoming messages" testProcessIncoming
        , testProperty "Processing incoming responses" testIncomingResponse
        , testProperty "Receiving incoming requests" testReceiveRequest
        , testProperty "Sending responses" testSendResponse
        ]
    , testGroup "Network"
        [ testProperty "Test server" serverTest
        , testProperty "Test client" clientTest
        , testProperty "Test notifications" notifTest
        , testProperty "Test send request" testSendRequest
        ]
    ]

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

data Struct = Struct { getValue :: Value } deriving (Show, Eq)

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
    assert $ rt == map Right msgs
  where
    encoded = B.concat $ map (L.toStrict . encode) msgs
    buffer bs | B.null bs = []
              | otherwise = B.take 16 bs : buffer (B.drop 16 bs)

testDecodeGarbageConduit :: ([[Word8]], Ver) -> Property
testDecodeGarbageConduit (ss, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $
        CL.sourceList (map B.pack ss) =$ decodeConduit ver $$ CL.consume
    assert $ all isLeft rt

testDecodeInvalidConduit :: ([Struct], Ver) -> Property
testDecodeInvalidConduit (structs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $
        CL.sourceList (buffer encoded) =$ decodeConduit ver $$ CL.consume
    assert $ all invalid rt
  where
    encoded = B.concat $ map (L.toStrict . encode) msgs
    buffer bs | B.null bs = []
              | otherwise = B.take 16 bs : buffer (B.drop 16 bs)
    msgs = map getValue structs
    invalid (Left OrphanError{getError = ErrorObj{getErrCode = (-32600)}}) =
        True
    invalid _ = False

testProcessIncoming :: ([Either Response Message], Ver, Bool) -> Property
testProcessIncoming (msgs, ver, ignore) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        expect <- liftIO . atomically $ newTMChan
        qs <- liftIO . atomically $ initSession ver ignore
        withAsync (cli qs expect msgs) $ const $
            runReaderT processIncoming qs
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
        liftIO . atomically $ writeTBMChan (inCh qs) x
        liftIO . atomically $ case x of
            Left e -> do
                m <- readTBMChan $ outCh qs
                case m of
                    Just (MsgResponse err) ->
                        if err == e
                            then writeTMChan expect True
                            else writeTMChan expect False
                    _ -> writeTMChan expect False
            Right (MsgRequest req@Request{}) ->
                if ignore
                    then do
                        m <- readTBMChan $ outCh qs
                        case m of
                            Just (MsgResponse (ResponseError _ e _)) ->
                                case e of
                                    ErrorObj{} ->
                                        if getErrCode e == (-32601)
                                            then writeTMChan expect True
                                            else writeTMChan expect False
                                    _ -> writeTMChan expect False
                            _ -> writeTMChan expect False
                    else do
                        m <- readTBMChan (fromJust $ reqCh qs)
                        if m == Just req
                            then writeTMChan expect True
                            else writeTMChan expect False
            Right (MsgRequest nt@Notif{}) ->
                if ignore
                    then writeTMChan expect True
                    else do
                        m <- readTBMChan (fromJust $ reqCh qs)
                        if m == Just nt
                            then writeTMChan expect True
                            else writeTMChan expect False
            Right (MsgResponse OrphanError{}) ->
                writeTMChan expect True
            Right MsgResponse{} -> do
                m <- readTBMChan $ outCh qs
                case m of
                    Just (MsgResponse (OrphanError _ e@ErrorObj{})) ->
                        if getErrCode e == (-32000)
                            then writeTMChan expect True
                            else writeTMChan expect False
                    _ -> writeTMChan expect False
        cli qs expect xs

testIncomingResponse :: ([Req], Ver, Bool) -> Property
testIncomingResponse (reqss, ver, ignore) = nodup ==> monadicIO $ do
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
    reqs = map getReq reqss
    nodup = length (nubBy ((==) `on` getReqId) reqs) == length reqs
    respond (Request v _ p i) = Response v p i
    respond _ = undefined
    responses = map respond reqs
    msgs = map MsgResponse responses
    sent = do
        promises <- forM reqs $
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
    cli qs expect (x:xs) = do
        let i = getResId $ getMsgResponse x
        p <- liftIO . atomically $ do
            snt <- readTVar (sentReqs qs)
            return . fromJust $ M.lookup i snt
        liftIO . atomically $ writeTBMChan (inCh qs) (Right x)
        liftIO . atomically $ do
            rE <- readTMVar p
            F.forM_ rE $ writeTMChan expect
        cli qs expect xs

testSendRequest :: ([(ReqNot, Either ErrorObj Value)], Ver, Bool) -> Property
testSendRequest (reqnotres, ver, ignore) = monadicIO $ do
    (ex, rs) <- run $ runNoLoggingT $ do
        expect <- liftIO . atomically $ newTMChan
        qs <- liftIO . atomically $ initSession ver ignore
        rs <- withAsync (cli qs expect reqnotres) $ \c -> do
            rs <- runReaderT (mapM (sendRequest . fst) reqnotres) qs
            wait c
            return (rs :: [Maybe (Either ErrorObj Value)])
        ex <- liftIO . atomically $ getexpect expect []
        return (ex, rs)
    assert $ length rs == length reqnotres
    assert $ and $ zipWith matchresult reqnotres rs
    assert $ and $ zipWith matchnotif (map fst reqnotres) ex
  where
    matchnotif q r =
        case q of
            ReqNotReq v ->
                case r of
                    Just (MsgRequest Request{getReqParams = v'}) -> v == v'
                    _ -> False
            ReqNotNot v ->
                case r of
                    Just (MsgRequest Notif{getReqParams = v'}) -> v == v'
                    _ -> False
    matchresult (q, e) r =
        case q of
            ReqNotReq{} -> Just e == r
            _ -> isNothing r
    getexpect expect acc = do
        valM <- readTMChan expect
        case valM of
            Nothing -> return $ reverse acc
            Just val -> getexpect expect (val:acc)
    respond (Request v _ _ i) (Left  y) = Just $ ResponseError v y i
    respond (Request v _ _ i) (Right y) = Just $ Response      v y i
    respond _ _ = undefined
    cli _ expect [] = liftIO . atomically $ closeTMChan expect
    cli qs expect ((x,y):xs) = join . liftIO . atomically $ do
        msg <- readTBMChan (outCh qs)
        writeTMChan expect msg
        unless (requestIsNotif x) $
            case msg of
                Just (MsgRequest req@Request{}) -> do
                    sent <- readTVar (sentReqs qs)
                    F.forM_ (getReqId req `M.lookup` sent) $ \p ->
                        putTMVar p $ respond req y
                _ -> return ()
        return $ cli qs expect xs

testReceiveRequest :: ([Request], Ver, Bool) -> Property
testReceiveRequest (reqs, ver, ignore) = monadicIO $ do
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
    receive = forM reqs $ const receiveRequest

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

reqFields :: Request -> Bool
reqFields rq = testFields (checkFieldsReq rq) rq

resFields :: Response -> Bool
resFields rs = testFields (checkFieldsRes rs) rs

createChans :: MonadIO m
            => m ((TBMChan a, TBMChan b), (Sink a m (), Source m b))
createChans = do
    (bso, bsi) <- liftIO . atomically $ (,) <$> newTBMChan 16 <*> newTBMChan 16
    let (snk, src) = (sinkTBMChan bso False, sourceTBMChan bsi)
    return ((bso, bsi), (snk, src))

serverTest :: ([Request], Ver) -> Property
serverTest (reqs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        ((bso, bsi), (snk, src)) <- createChans
        withAsync (server snk src) $ const $
            withAsync (sender bsi) $ const $ receiver bso []
    assert $ length rt == length nonotif
    assert $ null rt || all isJust rt
    assert $ params == reverse (results rt)
  where
    respond q = return $ Right (q :: Value)
    server snk src = runJsonRpcT ver False
        (encodeConduit =$ snk) (src =$ decodeConduit ver) (srv respond)
    sender bsi = forM_ reqs $ liftIO . atomically .
        writeTBMChan bsi . L.toStrict . encode . MsgRequest
    receiver bso xs =
        if length xs == length nonotif
            then return xs
            else liftIO (atomically $ readTBMChan bso) >>= \b -> case b of
                Just x -> do
                    let res = decodeStrict' x :: Maybe Response
                    receiver bso (res:xs)
                Nothing -> undefined
    params = map getReqParams nonotif
    results = map $ getResult . fromJust
    nonotif = flip filter reqs $ \q -> case q of Request{} -> True
                                                 Notif{}   -> False

clientTest :: ([Value], Ver) -> Property
clientTest (qs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        ((bso, bsi), (snk, src)) <- createChans
        let csnk = sinkTBMChan bsi False
            csrc = sourceTBMChan bso
        withAsync (server snk src) $ const $ cli
            (CL.map Right =$ csnk)
            (csrc $= CL.map Right)
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
        withAsync (server snk src sig notifs) $ const $ cli sig
            (CL.map Right =$ csnk)
            (csrc $= CL.map Right)
        liftIO . atomically $ readTVar notifs
    assert $ length nt == length ntfs
    assert $ reverse nt == ntfs
  where
    respond q = return $ Right (q :: Value)
    server snk src sig notifs =
        runJsonRpcT ver False snk src $ process sig notifs
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


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = undefined
