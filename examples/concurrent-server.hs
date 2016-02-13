{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import qualified Data.Foldable as F
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.JsonRpc

data Req = TimeReq | Ping deriving (Show, Eq)

instance FromRequest Req where
    parseParams "time" = Just $ const $ return TimeReq
    parseParams "ping" = Just $ const $ return Ping
    parseParams _      = Nothing

instance ToRequest Req where
    requestMethod TimeReq = "time"
    requestMethod Ping    = "ping"
    requestIsNotif        = const False

instance ToJSON Req where
    toJSON = const emptyArray

data Res = Time { getTime :: UTCTime } | Pong deriving (Show, Eq)

instance FromResponse Res where
    parseResult "time" = Just $ withText "time" $ \t ->
        case parseTimeM True defaultTimeLocale "%c" $ T.unpack t of
            Just t' -> return $ Time t'
            Nothing -> mzero
    parseResult "ping" = Just $ const $ return Pong
    parseResult _ = Nothing

instance ToJSON Res where
    toJSON (Time t) = toJSON $ formatTime defaultTimeLocale "%c" t
    toJSON Pong     = emptyArray

respond :: MonadLoggerIO m => Respond Req m Res
respond TimeReq = liftM (Right . Time) $ liftIO getCurrentTime
respond Ping    = return $ Right Pong

main :: IO ()
main = runStderrLoggingT $ do
    let ss = serverSettings 31337 "::1"
    jsonRpcTcpServer V2 False ss $ withAsync pinger $ const srv

srv :: MonadLoggerIO m => JsonRpcT m ()
srv = do
    $(logDebug) "listening for new request"
    qM <- receiveBatchRequest
    case qM of
        Nothing -> do
            $(logDebug) "closed request channel, exting"
            return ()
        Just (SingleRequest q) -> do
            $(logDebug) "got request"
            rM <- buildResponse respond q
            F.forM_ rM sendResponse
            srv
        Just (BatchRequest qs) -> do
            $(logDebug) "got request batch"
            rs <- catMaybes `liftM` forM qs (buildResponse respond)
            sendBatchResponse $ BatchResponse rs
            srv

pinger :: MonadLoggerIO m => JsonRpcT m ()
pinger = do
    $(logDebug) "ping client"
    p <- sendRequest Ping
    $(logDebug) $ T.pack $
        "received " ++ show (p :: Maybe (Either ErrorObj Res))
    liftIO $ threadDelay 1500000
    pinger
