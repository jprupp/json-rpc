{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance FromRequest TimeReq where
    parseParams "time" = Just $ const $ return TimeReq 
    parseParams _      = Nothing

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t

respond :: MonadLoggerIO m => Respond TimeReq m TimeRes
respond TimeReq = do
    t <- liftIO getCurrentTime
    return . Right $ TimeRes t

main :: IO ()
main = runStderrLoggingT $ do
    let ss = serverSettings 31337 "::1"
    jsonRpcTcpServer V2 False ss srv

srv :: MonadLoggerIO m => JsonRpcT m ()
srv = do
    $(logDebug) "listening for new request"
    qM <- receiveRequest
    case qM of
        Nothing -> do
            $(logDebug) "closed request channel, exting"
            return ()
        Just q -> do
            $(logDebug) "got request"
            rM <- buildResponse respond q
            case rM of
                Nothing -> do
                    $(logDebug) "no response for this request"
                    srv
                Just r -> do
                    $(logDebug) "sending response"
                    sendResponse r >> srv
