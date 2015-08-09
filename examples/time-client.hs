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
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime } deriving Show

instance ToRequest TimeReq where
    requestMethod TimeReq = "time"
    requestIsNotif = const False

instance ToJSON TimeReq where
    toJSON TimeReq = emptyArray

instance FromResponse TimeRes where
    parseResult "time" = Just $ withText "time" $ \t -> case f t of
        Just t' -> return $ TimeRes t'
        Nothing -> mzero
      where
        f t = parseTime defaultTimeLocale "%c" (T.unpack t)
    parseResult _ = Nothing

req :: MonadLoggerIO m => JsonRpcT m UTCTime
req = do
    $(logDebug) "sending time request"
    ts <- sendRequest TimeReq
    case ts of
        Nothing -> error "could not parse response"
        Just (Left e) -> error $ fromError e
        Just (Right (TimeRes r)) -> return r

reqBatch :: MonadLoggerIO m => JsonRpcT m [UTCTime]
reqBatch = do
    $(logDebug) "sending time requests"
    ts <- sendBatchRequest $ replicate 4 TimeReq
    forM ts $ \t ->
        case t of
            Nothing -> error "could not receive or parse response"
            Just (Left e) -> error $ fromError e
            Just (Right (TimeRes r)) -> return r


main :: IO ()
main = runStderrLoggingT $
    jsonRpcTcpClient V2 True (clientSettings 31337 "::1") $ do
        $(logDebug) "sending four time requests one second apart"
        replicateM_ 4 $ do
            req >>= $(logDebug) . T.pack . ("response: "++) . show
            liftIO (threadDelay 1000000)
        $(logDebug) "sending four time requests in a batch"
        reqBatch >>= $(logDebug) . T.pack . ("response: "++) . show
