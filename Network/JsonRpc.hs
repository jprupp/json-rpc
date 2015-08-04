module Network.JsonRpc
( -- * Introduction
  -- $introduction

  -- ** Server Example
  -- $server

  -- ** Client Example
  -- $client

  module Network.JsonRpc.Interface
, module Network.JsonRpc.Data
) where

import Network.JsonRpc.Interface
import Network.JsonRpc.Data

-- $introduction
--
-- This JSON-RPC library is fully-compatible with JSON-RPC 2.0 and
-- partially-compatible with JSON-RPC 1.0. It provides an interface that
-- combines a JSON-RPC client and server. It can set and keep track of request
-- ids to parse responses.  There is support for sending and receiving
-- notifications. You may use any underlying transport.  Basic TCP client and
-- server provided.
--
-- The recommended interface to this library is provided as conduits that
-- encode outgoing messages, and decode incoming messages.  Incoming messages
-- are delivered as an 'IncomingMsg' data structure, while outgoing messages
-- are sent in a 'Message' data structure.  The former packs responses and
-- errors with their corresponding request, and has a separate constructor for
-- decoding errors.
--
-- A JSON-RPC application using this interface is considered to be
-- peer-to-peer, as it can send and receive all types of JSON-RPC message
-- independent of whether it originated the connection.
--
-- Type classes 'ToRequest', 'ToNotif' are for data that can be converted into
-- JSON-RPC requests and notifications respectively. An instance of aeson's
-- 'ToJSON' class is also required to serialize these data structures.  Make
-- sure that they serialize as a structured JSON value (array or object) that
-- can go into the params field of the JSON-RPC object.  Type classes
-- 'FromRequest', 'FromNotif' and 'FromResult' are for deserializing JSON-RPC
-- messages.
--
-- Errors are deserialized to the 'ErrorObj' data type. Only a string is
-- supported as contents inside a JSON-RPC 1.0 error. JSON-RPC 2.0 errors also
-- have a code, and possibly additional data as an aeson 'Value'.


-- $server
--
-- This server returns the current time.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Data.Aeson.Types
-- >import Data.Conduit
-- >import qualified Data.Conduit.List as CL
-- >import Data.Conduit.Network
-- >import Data.Time.Clock
-- >import Data.Time.Format
-- >import Network.JsonRpc
-- >import System.Locale
-- >
-- >data TimeReq = TimeReq
-- >data TimeRes = TimeRes UTCTime
-- >
-- >instance FromRequest TimeReq where
-- >    paramsParser "time" = Just $ const $ return TimeReq 
-- >    paramsParser _ = Nothing
-- >
-- >instance ToJSON TimeRes where
-- >    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t
-- >
-- >srv :: AppConduits () () TimeRes TimeReq () () IO -> IO ()
-- >srv (src, snk) = src $= CL.mapM respond $$ snk
-- >
-- >respond :: IncomingMsg () TimeReq () ()
-- >        -> IO (Message () () TimeRes)
-- >respond (IncomingMsg (MsgRequest (Request ver _ TimeReq i)) Nothing) = do    
-- >    t <- getCurrentTime
-- >    return $ MsgResponse (Response ver (TimeRes t) i)
-- >
-- >respond (IncomingError e) = return $ MsgError e
-- >respond (IncomingMsg (MsgError e) _) = return $ MsgError $ e
-- >respond _ = undefined
-- >
-- >main :: IO ()
-- >main = tcpServer V2 (serverSettings 31337 "127.0.0.1") srv


-- $client
--
-- Corresponding TCP client to get time from server.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Data.Aeson.Types hiding (Error)
-- >import Data.Conduit
-- >import qualified Data.Conduit.List as CL
-- >import Data.Conduit.Network
-- >import qualified Data.Text as T
-- >import Data.Time.Clock
-- >import Data.Time.Format
-- >import Network.JsonRpc
-- >import System.Locale
-- >
-- >data TimeReq = TimeReq
-- >data TimeRes = TimeRes UTCTime
-- >
-- >instance ToRequest TimeReq where
-- >    requestMethod TimeReq = "time"
-- >
-- >instance ToJSON TimeReq where
-- >    toJSON TimeReq = emptyArray
-- >
-- >instance FromResponse TimeRes where
-- >    parseResult "time" = withText "time" $ \t -> case f t of
-- >        Nothing -> fail "Could not parse time"
-- >        Just t' -> return $ TimeRes t'
-- >      where
-- >        f t = parseTime defaultTimeLocale "%c" (T.unpack t)
-- >
-- >cli :: AppConduits TimeReq () () () () TimeRes IO
-- >    -> IO UTCTime
-- >cli (src, snk) = do
-- >    CL.sourceList [MsgRequest $ buildRequest V2 TimeReq] $$ snk
-- >    ts <- src $$ CL.consume
-- >    case ts of
-- >        [] -> error "No response received"
-- >        [IncomingError (ErrorObj _ m _ _ _)] -> error $ "Unknown: " ++ m
-- >        [IncomingMsg (MsgError (ErrorObj _ m _ _ _)) _] -> error m
-- >        [IncomingMsg (MsgResponse (Response _ (TimeRes t) _)) _] -> return t
-- >        _ -> undefined
-- >
-- >main :: IO ()
-- >main = tcpClient V2 True (clientSettings 31337 "127.0.0.1") cli >>= print
