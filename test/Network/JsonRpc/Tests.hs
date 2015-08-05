{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Network.JsonRpc.Tests (tests) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as L
import Data.Conduit.TMChan
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types
import Data.Either
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
            (errFields :: RpcError -> Bool)
        , testProperty "Encode/decode"
            (testEncodeDecode :: RpcError -> Bool)
        ]
    , testGroup "Network"
        [ testProperty "Test server" serverTest
        , testProperty "Test client" clientTest
        ]
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

checkFieldsErr :: RpcError -> Object -> Parser Bool
checkFieldsErr (RpcError ver e i) o = do
    checkVerId ver i o >>= guard
    o .: "error" >>= guard . (==e)
    return True

testFields :: ToJSON r => (Object -> Parser Bool) -> r -> Bool
testFields ck r = fromMaybe False . parseMaybe f $ toJSON r where
    f = withObject "json" ck

testEncodeDecode :: (Eq r, ToJSON r, FromJSON r) => r -> Bool
testEncodeDecode r = maybe False (==r) $ parseMaybe parseJSON (toJSON r)

reqFields :: Request -> Bool
reqFields rq = testFields (checkFieldsReq rq) rq

notifFields :: Notif -> Bool
notifFields nt = testFields (checkFieldsNotif nt) nt

resFields :: Response -> Bool
resFields rs = testFields (checkFieldsRes rs) rs

errFields :: RpcError -> Bool
errFields er = testFields (checkFieldsErr er) er

serverTest :: ([Request], Ver) -> Property
serverTest (reqs, ver) = monadicIO $ do
    rt <- run $ do
        (bso, bsi) <- atomically $ (,) <$> newTBMChan 16 <*> newTBMChan 16
        let snk = sinkTBMChan bso False
            src = sourceTBMChan bsi
        withAsync (srv snk src) $ const $
            withAsync (sender bsi) $ const $ receiver bso []
    assert $ length rt == length reqs
    assert $ null rt || all isJust rt
    assert $ params == reverse (results rt)
  where
    r q = return $ Right (q :: Value)
    srv snk src = runJsonRpcT ver r snk src dummySrv
    sender bsi = forM_ reqs $ atomically .
        writeTBMChan bsi . L.toStrict . encode . MsgRequest
    receiver bso xs = if length xs == length reqs
        then return xs
        else liftIO (atomically $ readTBMChan bso) >>= \b -> case b of
            Just x -> do
                let res = decodeStrict' x :: Maybe Response
                receiver bso (res:xs)
            Nothing -> undefined
    params = map getReqParams reqs
    results = map $ getResult . fromJust

clientTest :: ([Value], Ver) -> Property
clientTest (qs, ver) = monadicIO $ do
    rt <- run $ do
        (bso, bsi) <- atomically $ (,) <$> newTBMChan 16 <*> newTBMChan 16
        let snk = sinkTBMChan bso False
            src = sourceTBMChan bsi
            csnk = sinkTBMChan bsi False
            csrc = sourceTBMChan bso
        withAsync (srv snk src) $ const $ cli csnk csrc
    assert $ length rt == length qs
    assert $ null rt || all correct rt
    assert $ qs == results rt
  where
    r q = return $ Right (q :: Value)
    srv snk src = runJsonRpcT ver r snk src dummySrv
    cli snk src = runJsonRpcT ver r snk src $
        forM qs $ sendRequest >=> liftIO . atomically
    results = map fromJust . rights
    correct (Right (Just _)) = True
    correct _ = False
