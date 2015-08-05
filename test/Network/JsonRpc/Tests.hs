{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Network.JsonRpc.Tests (tests) where

-- import Control.Concurrent
-- import Control.Concurrent.Async
-- import Control.Concurrent.STM
-- import Control.Exception hiding (assert)
import Control.Monad
import Data.Aeson.Types hiding (Error)
-- import Data.Conduit
-- import qualified Data.Conduit.List as CL
-- import Data.List
-- import Data.Conduit.Network
-- import Data.Conduit.TMChan
-- import qualified Data.HashMap.Strict as M
import Data.Maybe
-- import Data.Text (Text)
import Network.JsonRpc
import Network.JsonRpc.Arbitrary()
-- import Test.QuickCheck
-- import Test.QuickCheck.Monadic
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
    , testGroup "JSON-RPC Notifications"
        [ testProperty "Check fields"
            (notifFields :: Notif -> Bool)
        , testProperty "Encode/decode"
            (testEncodeDecode :: Notif -> Bool)
        ]
    , testGroup "JSON-RPC Responses"
        [ testProperty "Check fields"
            (resFields :: Response -> Bool)
        , testProperty "Encode/decode"
            (testEncodeDecode :: Response -> Bool)
        ]
    , testGroup "JSON-RPC Errors"
        [ testProperty "Check fields"
            (errFields :: Error -> Bool)
        , testProperty "Encode/decode"
            (testEncodeDecode :: Error -> Bool)
        ]
    -- , testGroup "JSON-RPC Conduits"
    --     [ testProperty "Outgoing conduit"
    --         (newMsgConduit :: [Message Value Value Value] -> Property)
    --     , testProperty "Decode requests"
    --         (decodeReqConduit :: ([Request Value], Ver) -> Property)
    --     , testProperty "Decode responses"
    --         (decodeResConduit :: ([ReqRes Value Value], Ver) -> Property)
    --     , testProperty "Bad responses"
    --         (decodeErrConduit :: ([ReqRes Value Value], Ver) -> Property)
    --     , testProperty "Sending messages" sendMsgNet
    --     , testProperty "Two-way communication" twoWayNet
    --     , testProperty "Real network communication" realNet
    --     ]
    ]

checkVerId :: Ver -> Id -> Object -> Parser Bool
checkVerId ver i o = do
    j <- o .:? "jsonrpc"
    guard $ if ver == V2 then j == Just (String "2.0") else isNothing j
    o .:? "id" .!= IdNull >>= guard . (==i)
    return True

checkFieldsReqNotif :: Ver -> Method -> Value -> Id -> Object -> Parser Bool
checkFieldsReqNotif ver m v i o = do
    checkVerId ver i o >>= guard
    o .: "method" >>= guard . (==m)
    o .: "params" >>= guard . (==v)
    return True

checkFieldsReq :: Request -> Object -> Parser Bool
checkFieldsReq (Request ver m v i) = checkFieldsReqNotif ver m v i

checkFieldsNotif :: Notif -> Object -> Parser Bool
checkFieldsNotif (Notif ver m v) = checkFieldsReqNotif ver m v IdNull

checkFieldsRes :: Response -> Object -> Parser Bool
checkFieldsRes (Response ver v i) o = do
    checkVerId ver i o >>= guard
    o .: "result" >>= guard . (==v)
    return True

checkFieldsErr :: Error -> Object -> Parser Bool
checkFieldsErr (Error ver e i) o = do
    checkVerId ver i o >>= guard
    o .: "error" >>= guard . (==e)
    return True

testFields :: ToJSON r => (Object -> Parser Bool) -> r -> Bool
testFields check r = fromMaybe False . parseMaybe f $ toJSON r where
    f = withObject "json" check

testEncodeDecode :: (Eq r, ToJSON r, FromJSON r) => r -> Bool
testEncodeDecode r = maybe False (==r) $ parseMaybe parseJSON (toJSON r)

reqFields :: Request -> Bool
reqFields rq = testFields (checkFieldsReq rq) rq

notifFields :: Notif -> Bool
notifFields nt = testFields (checkFieldsNotif nt) nt

resFields :: Response -> Bool
resFields rs = testFields (checkFieldsRes rs) rs

errFields :: Error -> Bool
errFields er = testFields (checkFieldsErr er) er

-- --
-- -- Conduit
-- --
-- 
-- newMsgConduit :: ( ToRequest q, ToJSON q, ToNotif n, ToJSON n
--                  , ToJSON r, FromResponse r )
--               => [Message q n r] -> Property
-- newMsgConduit (snds) = monadicIO $ do
--     msgs <- run $ do
--         qs <- atomically initSession
--         CL.sourceList snds' $= msgConduit False qs $$ CL.consume
--     assert $ length msgs == length snds'
--     assert $ length (filter rqs msgs) == length (filter rqs snds')
--     assert $ map idn (filter rqs msgs) == take (length (filter rqs msgs)) [1..]
--   where
--     rqs (MsgRequest _) = True
--     rqs _ = False
--     idn (MsgRequest rq) = getIdInt $ getReqId rq
--     idn _ = error "Unexpected request"
--     snds' = flip map snds $ \m -> case m of
--         (MsgRequest rq) -> MsgRequest $ rq { getReqId = IdNull }
--         _ -> m
-- 
-- decodeReqConduit :: forall q. (ToRequest q, FromRequest q, Eq q, ToJSON q)
--                  => ([Request q], Ver) -> Property
-- decodeReqConduit (vs, ver) = monadicIO $ do
--     inmsgs <- run $ do
--         qs  <- atomically initSession
--         qs' <- atomically initSession
--         CL.sourceList vs
--             $= CL.map f
--             $= msgConduit False qs
--             $= encodeConduit
--             $= decodeConduit ver False qs'
--             $$ CL.consume
--     assert $ not (any unexpected inmsgs)
--     assert $ all (uncurry match) (zip vs inmsgs)
--   where
--     unexpected :: IncomingMsg () q () () -> Bool
--     unexpected (IncomingMsg (MsgRequest _) Nothing) = False
--     unexpected _ = True
--     match rq (IncomingMsg (MsgRequest rq') _) =
--         rq { getReqId = getReqId rq' } == rq'
--     match _ _ = False
--     f rq = MsgRequest $ rq { getReqId = IdNull } :: Message q () ()
-- 
-- decodeResConduit :: forall q r.
--                     ( ToRequest q, FromRequest q, Eq q, ToJSON q, ToJSON r
--                     , FromResponse r, Eq r )
--                  => ([ReqRes q r], Ver) -> Property
-- decodeResConduit (rr, ver) = monadicIO $ do
--     inmsgs <- run $ do
--         qs  <- atomically initSession
--         qs' <- atomically initSession
--         CL.sourceList vs
--             $= CL.map f
--             $= msgConduit False qs
--             $= encodeConduit
--             $= decodeConduit ver False qs'
--             $= CL.map respond
--             $= encodeConduit
--             $= decodeConduit ver False qs
--             $$ CL.consume
--     assert $ not (any unexpected inmsgs)
--     assert $ all (uncurry match) (zip vs inmsgs)
--   where
--     unexpected :: IncomingMsg q () () r -> Bool
--     unexpected (IncomingMsg (MsgResponse _) (Just _)) = False
--     unexpected _ = True
-- 
--     match rq (IncomingMsg (MsgResponse rs) (Just rq')) =
--         rq { getReqId = getReqId rq' } == rq'
--             && rs == g rq'
--     match _ _ = False
-- 
--     respond :: IncomingMsg () q () () -> Response r
--     respond (IncomingMsg (MsgRequest rq) Nothing) = g rq
--     respond _ = undefined
-- 
--     f rq = MsgRequest $ rq { getReqId = IdNull } :: Message q () ()
--     vs = map (\(ReqRes rq _) -> rq) rr
-- 
--     g rq = let (ReqRes _ rs) = fromJust $ find h rr
--                h (ReqRes rq' _) = getReqParams rq == getReqParams rq'
--            in  rs { getResId = getReqId rq }
-- 
-- decodeErrConduit :: forall q r.
--                     ( ToRequest q, FromRequest q, Eq q, ToJSON q, ToJSON r
--                     , FromResponse r, Eq r, Show r, Show q )
--                  => ([ReqRes q r], Ver) -> Property
-- decodeErrConduit (vs, ver) = monadicIO $ do
--     inmsgs <- run $ do
--         qs  <- atomically initSession
--         qs' <- atomically initSession
--         CL.sourceList vs
--             $= CL.map f
--             $= msgConduit False qs
--             $= encodeConduit
--             $= decodeConduit ver False qs'
--             $= CL.map respond
--             $= encodeConduit
--             $= decodeConduit ver False qs
--             $$ CL.consume
--     assert $ not (any unexpected inmsgs)
--     assert $ all (uncurry match) (zip vs inmsgs)
--   where
--     unexpected :: IncomingMsg q () () r -> Bool
--     unexpected (IncomingMsg (MsgError _) (Just _)) = False
--     -- unexpected _ = True
--     unexpected i = error $ show i
-- 
--     match (ReqRes rq _) (IncomingMsg (MsgError _) (Just rq')) =
--         rq' { getReqId = getReqId rq } == rq
--     match _ _ = False
-- 
--     respond :: IncomingMsg () q () () -> ErrorObj
--     respond (IncomingMsg (MsgRequest (Request ver' _ _ i)) Nothing) =
--         ErrorObj ver' "test" (getIdInt i) Null i
--     respond _ = undefined
-- 
--     f (ReqRes rq _) = MsgRequest $ rq { getReqId = IdNull } :: Message q () ()
-- 
-- type ClientAppConduits = AppConduits Value Value Value () () () IO
-- type ServerAppConduits = AppConduits () () () Value Value Value IO
-- 
-- sendMsgNet :: ([Message Value Value Value], Ver) -> Property
-- sendMsgNet (rs, ver) = monadicIO $ do
--     rt <- run $ do
--         mv <- newEmptyMVar
--         to <- atomically $ newTBMChan 128
--         ti <- atomically $ newTBMChan 128
--         let tiSink   = sinkTBMChan ti True
--             toSource = sourceTBMChan to
--             toSink   = sinkTBMChan to True
--             tiSource = sourceTBMChan ti
--         withAsync (srv tiSink toSource mv) $ \_ ->
--             runConduits ver False toSink tiSource (cliApp mv)
--     assert $ length rt == length rs
--     assert $ all (uncurry match) (zip rs rt)
--   where
--     srv tiSink toSource mv = runConduits ver False tiSink toSource (srvApp mv)
-- 
--     srvApp :: MVar [IncomingMsg () Value Value Value]
--            -> ServerAppConduits -> IO ()
--     srvApp mv (src, snk) =
--         (CL.sourceNull $$ snk) >> (src $$ CL.consume) >>= putMVar mv
-- 
--     cliApp :: MVar [IncomingMsg () Value Value Value]
--            -> ClientAppConduits -> IO [IncomingMsg () Value Value Value]
--     cliApp mv (src, snk) =
--         (CL.sourceList rs $$ snk) >> (src $$ CL.sinkNull) >> readMVar mv
-- 
--     match (MsgRequest rq) (IncomingMsg (MsgRequest rq') Nothing) =
--         rq == rq'
--     match (MsgNotif rn) (IncomingMsg (MsgNotif rn') Nothing) =
--         rn == rn'
--     match (MsgResponse _) (IncomingError e) =
--         getErrMsg e == "Id not recognized"
--     match (MsgError e) (IncomingMsg (MsgError e') Nothing) =
--         getErrMsg e == getErrMsg e'
--     match (MsgError _) (IncomingError e) =
--         getErrMsg e == "Id not recognized"
--     match _ _ = False
-- 
-- type TwoWayAppConduits = AppConduits Value Value Value Value Value Value IO
-- 
-- twoWayNet :: ([Message Value Value Value], Ver) -> Property
-- twoWayNet (rr, ver) = monadicIO $ do
--     rt <- run $ do
--         to <- atomically $ newTBMChan 128
--         ti <- atomically $ newTBMChan 128
--         let tiSink   = sinkTBMChan ti True
--             toSource = sourceTBMChan to
--             toSink   = sinkTBMChan to True
--             tiSource = sourceTBMChan ti
--         withAsync (srv tiSink toSource) $ \_ ->
--             runConduits ver False toSink tiSource cliApp
--     assert $ length rt == length rs
--     assert $ all (uncurry match) (zip rs rt)
--   where
--     rs = map f rr where
--         f (MsgRequest rq) = MsgRequest $ rq { getReqId = IdNull }
--         f m = m
-- 
--     cliApp :: TwoWayAppConduits -> IO [IncomingMsg Value Value Value Value]
--     cliApp (src, snk) = (CL.sourceList rs $$ snk) >> (src $$ CL.consume)
-- 
--     srv tiSink toSource = runConduits ver False tiSink toSource srvApp
-- 
--     srvApp :: TwoWayAppConduits -> IO ()
--     srvApp (src, snk) = src $= CL.map respond $$ snk
-- 
--     respond (IncomingError e) =
--         MsgError e
--     respond (IncomingMsg (MsgRequest (Request ver' _ p i)) _) =
--         MsgResponse (Response ver' p i)
--     respond (IncomingMsg (MsgNotif rn) _) =
--         MsgNotif rn
--     respond (IncomingMsg (MsgError e) _) =
--         MsgNotif (Notif (getErrVer e) "error" (toJSON e))
--     respond _ = undefined
-- 
--     match (MsgRequest (Request ver' m p _))
--         ( IncomingMsg (MsgResponse (Response ver'' p' _))
--                       (Just (Request ver''' m' p'' _)) ) =
--         p == p' && p == p'' && m == m' && ver' == ver'' && ver'' == ver'''
--     match (MsgNotif (Notif ver' _ p))
--         (IncomingMsg (MsgNotif (Notif ver'' _ p')) Nothing) =
--         p == p' && ver' == ver''
--     match (MsgResponse (Response ver' _ _))
--         (IncomingMsg (MsgError e) Nothing) =
--         ver' == getErrVer e && getErrMsg e == "Id not recognized"
--     match (MsgError e@(ErrorObj _ _ _ _ IdNull))
--         (IncomingMsg (MsgNotif (Notif _ "error" e')) Nothing) =
--         toJSON e == e'
--     match (MsgError _)
--         (IncomingMsg (MsgError e) Nothing) =
--         getErrMsg e == "Id not recognized"
--     match _ _ = False
-- 
-- realNet :: ([Request Value], Ver) -> Property
-- realNet (rr, ver) = monadicIO $ do
--     rs <- run $ withAsync (tcpServer ver ss srvApp) $ const cli
--     assert $ length rs == length rr
--     assert $
--         map (getReqParams . fromJust . matchingReq) rs == map getReqParams rr
--   where
--     ss = serverSettings 58493 "127.0.0.1"
--     cs = clientSettings 58493 "127.0.0.1"
-- 
--     cli = do
--         cE <- try $ tcpClient ver True cs cliApp
--         either (const cli) return
--             (cE :: Either SomeException [IncomingMsg Value () () Value])
-- 
--     srvApp :: AppConduits () () Value Value () () IO -> IO ()
--     srvApp (src, snk) = src $= CL.map respond $$ snk
-- 
--     cliApp :: AppConduits Value () () () () Value IO
--            -> IO [IncomingMsg Value () () Value]
--     cliApp (src, snk) = do
--         CL.sourceList (map f rr) $$ snk
--         src $$ CL.consume
--       where
--         f rq = MsgRequest (rq { getReqId = IdNull })
-- 
--     respond (IncomingMsg (MsgRequest (Request ver' _ p i)) _) =
--         MsgResponse (Response ver' p i)
--     respond _ = undefined
