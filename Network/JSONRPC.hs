module Network.JSONRPC
( -- * Introduction
  -- $introduction

  module Network.JSONRPC.Interface
, module Network.JSONRPC.Data
) where

import Network.JSONRPC.Interface
import Network.JSONRPC.Data

-- $introduction
--
-- This JSON-RPC library is fully-compatible with JSON-RPC 2.0 and 1.0. It
-- provides an interface that combines a JSON-RPC client and server. It can
-- set and keep track of request ids to parse responses.  There is support
-- for sending and receiving notifications. You may use any underlying
-- transport.  Basic TCP client and server provided.
--
-- A JSON-RPC application using this interface is considered to be
-- peer-to-peer, as it can send and receive all types of JSON-RPC message
-- independent of whether it originated the connection.
