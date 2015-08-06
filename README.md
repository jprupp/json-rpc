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
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import Data.Time.Clock
import Data.Time.Format
import Network.JSONRPC
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance FromRequest TimeReq where
    parseParams "time" = Just $ const $ return TimeReq 
    parseParams _ = Nothing

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t

respond :: (Functor m, MonadLoggerIO m) => Respond TimeReq m TimeRes
respond TimeReq = Right . TimeRes <$> liftIO getCurrentTime

main :: IO ()
main = runStderrLoggingT $
    jsonRpcTcpServer V2 (serverSettings 31337 "::1") respond dummySrv
```

Client Example
--------------

Corresponding TCP client to get time from server.

``` haskell
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
import Network.JSONRPC
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance ToRequest TimeReq where
    requestMethod TimeReq = "time"

instance ToJSON TimeReq where
    toJSON TimeReq = emptyArray

instance FromResponse TimeRes where
    parseResult "time" = Just $ withText "time" $ \t -> case f t of
        Just t' -> return $ TimeRes t'
        Nothing -> mzero
      where
        f t = parseTime defaultTimeLocale "%c" (T.unpack t)
    parseResult _ = Nothing

req :: MonadLoggerIO m => JSONRPCT m UTCTime
req = sendRequest TimeReq >>= \ts -> case ts of
    Left e -> error $ fromError e
    Right (Just (TimeRes r)) -> return r
    _ -> error "Could not parse response"

main :: IO ()
main = runStderrLoggingT $
    jsonRpcTcpClient V2 (clientSettings 31337 "::1") dummyRespond .
        replicateM_ 4 $ req >>= liftIO . print >> liftIO (threadDelay 1000000)
```
