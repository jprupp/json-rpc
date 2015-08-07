{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Network.JsonRpc.Tests (tests) where

import Control.Applicative
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.TMChan
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Network.JsonRpc
import Network.JsonRpc.Arbitrary()
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
    , testGroup "Network"
        [ testProperty "Test server" serverTest
        , testProperty "Test client" clientTest
        ]
    ]

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

reqFields :: Request -> Bool
reqFields rq = testFields (checkFieldsReq rq) rq

resFields :: Response -> Bool
resFields rs = testFields (checkFieldsRes rs) rs

serverTest :: ([Request], Ver) -> Property
serverTest (reqs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        (bso, bsi) <- liftIO . atomically $
            (,) <$> newTBMChan 16 <*> newTBMChan 16
        let snk = sinkTBMChan bso False
            src = sourceTBMChan bsi
        withAsync (server snk src) $ const $ withAsync (sender bsi) $ const $
            receiver bso []
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
                                                 Notif{} -> False

clientTest :: ([Value], Ver) -> Property
clientTest (qs, ver) = monadicIO $ do
    rt <- run $ runNoLoggingT $ do
        (bso, bsi) <- liftIO . atomically $
            (,) <$> newTBMChan 16 <*> newTBMChan 16
        let snk = sinkTBMChan bso False
            src = sourceTBMChan bsi
            csnk = sinkTBMChan bsi False
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
