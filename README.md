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

[Documentation](http://hackage.haskell.org/package/json-rpc)

[Examples](https://github.com/xenog/json-rpc/tree/master/examples)


Time Server Example
===================

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson.Types
import Data.Conduit.Network
import qualified Data.Foldable as F
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

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
        case parseTime defaultTimeLocale "%c" $ T.unpack t of
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
    jsonRpcTcpServer V2 False ss srv

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
```


Time Client Example
===================

```haskell
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
        case parseTime defaultTimeLocale "%c" $ T.unpack t of
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
```
