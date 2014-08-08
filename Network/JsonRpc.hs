module Network.JsonRpc
( -- * Introduction
  -- $introduction

  -- ** Server Example
  -- $server

  -- ** Client Example
  -- $client

  module Network.JsonRpc.Conduit
, module Network.JsonRpc.Data
) where

import Network.JsonRpc.Conduit
import Network.JsonRpc.Data

-- $introduction
--
-- This JSON-RPC package is fully-compatible with JSON-RPC 2.0 and
-- partially-compatible with JSON-RPC 1.0. It provides an interface that
-- combines a JSON-RPC client and server. It can automatically set ids on
-- outgoing requests, and match incoming responses to their corresponding
-- request for parsing the result field. It has support for sending and
-- receiving notifications. You may use any underlying transport, and the
-- library provides ready-to-use basic TCP client and server.
--
-- The highest-level interface to this module is provided in the form of
-- conduits that encode outgoing messages, and decode incoming messages.
-- Incoming messages are delivered as an 'IncomingMsg' data structure, while
-- outgoing messages are sent in a 'Message' data structure.  The main
-- difference between both is that the former packs responses and errors with
-- their corresponding request, or can contain an error to be returned to
-- sender.
--
-- A JSON-RPC application using this interface is considered to be
-- peer-to-peer, as it can send and receive all types of message, including
-- requests, notifications, responses and errors, independent of whether it
-- originated the connection.
--
-- Type classes 'ToRequest', 'ToNotif' are for data that can be converted into
-- JSON-RPC requests and notifications respectively. An instance of aeson's
-- 'ToJSON' class is also required to serialize these data structures into the
-- params field. Make sure to serialize as a structured JSON value (array or
-- object).  Type classes 'FromRequest', 'FromNotif' and 'FromResult' are for
-- deserializing JSON-RPC messages of the corresponding type. 
--
-- Errors are deserialized to the 'Error' data type. Only a string is supported
-- as contents inside a JSON-RPC 1.0 error. JSON-RPC 2.0 errors contain an
-- 'ErrorObj' with message, code, and additional data as an aeson 'Value'.


-- $server
--
-- This server returns the current time.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Data.Aeson.Types hiding (Error)
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
-- >main = tcpServer False (serverSettings 31337 "127.0.0.1") srv
-- >
-- >srv :: AppConduits () () TimeRes TimeReq () () IO -> IO ()
-- >srv (src, snk) = src $= CL.mapM respond $$ snk
-- >
-- >-- JSON-RPC 2.0
-- >respond :: IncomingMsg () TimeReq () ()
-- >        -> IO (Message () () TimeRes)
-- >respond (IncomingMsg (MsgRequest (Request _ TimeReq i)) Nothing) = do    
-- >    t <- getCurrentTime
-- >    return $ MsgResponse (Response (TimeRes t) i)
-- >
-- >-- JSON-RPC 1.0
-- >respond (IncomingMsg (MsgRequest (Request1 _ TimeReq i)) Nothing) = do    
-- >    t <- getCurrentTime
-- >    return $ MsgResponse (Response1 (TimeRes t) i)
-- >
-- >respond (IncomingError e) = return $ MsgError e
-- >respond _ = undefined


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
-- >    -> IO String
-- >cli (src, snk) = do
-- >    CL.sourceList [MsgRequest $ buildRequest TimeReq] $$ snk
-- >    ts <- src $$ CL.consume
-- >    case ts of
-- >        [] ->
-- >            error "No response received"
-- >        [IncomingError (Error (ErrorObj m _ _) _)] ->
-- >            error $ "Client error: " ++ m
-- >        [IncomingMsg (MsgError (Error (ErrorObj m _ _) _)) _] ->
-- >            error $ "Server error: " ++ m
-- >        [IncomingMsg (MsgResponse (Response (TimeRes t) _)) _] ->
-- >            return $ formatTime defaultTimeLocale "%c" t
-- >        _ -> undefined
-- >
-- >main :: IO ()
-- >main = do
-- >    t <- tcpClient False True (clientSettings 31337 "127.0.0.1") cli
-- >    putStrLn t
