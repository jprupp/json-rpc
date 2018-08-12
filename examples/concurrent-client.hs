{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Monad
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types     hiding (Error)
import           Data.Conduit.Network
import qualified Data.Foldable        as F
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
import           Network.JSONRPC
import           UnliftIO
import           UnliftIO.Concurrent

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
        Nothing        -> error "could not receive or parse response"
        Just (Left e)  -> error $ fromError e
        Just (Right r) -> r

req :: MonadLoggerIO m => JSONRPCT m Res
req = do
    tEM <- sendRequest TimeReq
    $(logDebug) "sending time request"
    return $ handleResponse tEM

reqBatch :: MonadLoggerIO m => JSONRPCT m [Res]
reqBatch = do
    $(logDebug) "sending pings"
    tEMs <- sendBatchRequest $ replicate 2 Ping
    return $ map handleResponse tEMs

respond :: MonadLoggerIO m => Respond Req m Res
respond TimeReq = (Right . Time) <$> liftIO getCurrentTime
respond Ping    = return $ Right Pong

main :: IO ()
main = runStderrLoggingT $
    jsonrpcTCPClient V2 False (clientSettings 31337 "::1") $
        withAsync responder $ const $ do
            $(logDebug) "sending four time requests one second apart"
            replicateM_ 4 $ do
                req >>= $(logDebug) . T.pack . ("response: "++) . show
                liftIO (threadDelay 1000000)
            $(logDebug) "sending two pings in a batch"
            reqBatch >>= $(logDebug) . T.pack . ("response: "++) . show

responder :: MonadLoggerIO m => JSONRPCT m ()
responder = do
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
            responder
        Just (BatchRequest qs) -> do
            $(logDebug) "got request batch"
            rs <- catMaybes `liftM` forM qs (buildResponse respond)
            sendBatchResponse $ BatchResponse rs
            responder
