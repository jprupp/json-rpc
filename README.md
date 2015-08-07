json-rpc
========

Fully-featured JSON-RPC 2.0 library for Haskell programs.

This JSON-RPC library is fully-compatible with JSON-RPC 2.0 and 1.0. It
provides an interface that combines a JSON-RPC client and server. It can
set and keep track of request ids to parse responses.  There is support
for sending and receiving notifications. You may use any underlying
transport.  Basic TCP client and server provided.

A JSON-RPC application using this interface is considered to be
peer-to-peer, as it can send and receive all types of JSON-RPC message
independent of whether it originated the connection.

[Hackage documentation](http://hackage.haskell.org/package/json-rpc)


Server Example
--------------

This JSON-RPC server returns the current time.

``` haskell
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
```

Client Example
--------------

Corresponding TCP client to get time from server.

``` haskell
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

req :: MonadLoggerIO m => JsonRpcT m (Either String UTCTime)
req = do
    $(logDebug) "sending time request"
    ts <- sendRequest TimeReq
    $(logDebug) "received response"
    case ts of
        Nothing -> return $ Left "could not parse response"
        Just (Left e) -> return . Left $ fromError e
        Just (Right (TimeRes r)) -> return $ Right r

main :: IO ()
main = runStderrLoggingT $
    jsonRpcTcpClient V2 True (clientSettings 31337 "::1") $ do
        $(logDebug) "sending four time requests one second apart"
        replicateM_ 4 $ do
            req >>= liftIO . print
            liftIO (threadDelay 1000000)
```
