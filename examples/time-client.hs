{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
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

handleResponse :: Maybe (Either ErrorObj Res) -> Res
handleResponse t =
    case t of
        Nothing -> error "could not receive or parse response"
        Just (Left e) -> error $ fromError e
        Just (Right r) -> r

req :: MonadLoggerIO m => JsonRpcT m Res
req = do
    tEM <- sendRequest TimeReq
    $(logDebug) "sending time request"
    return $ handleResponse tEM

reqBatch :: MonadLoggerIO m => JsonRpcT m [Res]
reqBatch = do
    $(logDebug) "sending pings"
    tEMs <- sendBatchRequest $ replicate 2 Ping
    return $ map handleResponse tEMs

main :: IO ()
main = runStderrLoggingT $
    jsonRpcTcpClient V2 True (clientSettings 31337 "::1") $ do
        $(logDebug) "sending two time requests one second apart"
        replicateM_ 2 $ do
            req >>= $(logDebug) . T.pack . ("response: "++) . show
            liftIO (threadDelay 1000000)
        $(logDebug) "sending two pings in a batch"
        reqBatch >>= $(logDebug) . T.pack . ("response: "++) . show
