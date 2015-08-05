{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Implementation of basic JSON-RPC data types.
module Network.JsonRpc.Data
( -- * Requests
  Request(..)
  -- ** Parsing
, FromRequest(..)
, fromRequest
  -- ** Encoding
, ToRequest(..)
, buildRequest

  -- * Responses
, Response(..)
  -- ** Parsing
, FromResponse(..)
, fromResponse
  -- ** Encoding
, Respond
, buildResponse

  -- * Notifications
, Notif(..)
  -- ** Parsing
, FromNotif(..)
, fromNotif
  -- ** Encoding
, ToNotif(..)
, buildNotif

  -- * Errors
, RpcError(..)
, ErrorObj(..)
, fromError
  -- ** Error Messages
, errorParse
, errorInvalid
, errorParams
, errorMethod
, errorId

  -- * Others
, Message(..)
, Method
, Id(..)
, Ver(..)

) where

import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Control.DeepSeq
import Control.Monad
import Data.Aeson (encode)
import Data.Aeson.Types
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import GHC.Generics (Generic)



--
-- Requests
--

data Request = Request { getReqVer      :: !Ver
                       , getReqMethod   :: !Method
                       , getReqParams   :: !Value
                       , getReqId       :: !Id
                       } deriving (Eq, Show)

instance NFData Request where
    rnf (Request v m p i) = rnf v `seq` rnf m `seq` rnf p `seq` rnf i

instance ToJSON Request where
    toJSON (Request V2 m p i) = object $ case p of
        Null -> [jr2, "method" .= m, "id" .= i]
        _    -> [jr2, "method" .= m, "id" .= i, "params" .= p]
    toJSON (Request V1 m p i) = object $ case p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= i]
        _    -> ["method" .= m, "params" .= p, "id" .= i]

class FromRequest q where
    -- | Parser for params Value in JSON-RPC request.
    parseParams :: Method -> Maybe (Value -> Parser q)

fromRequest :: FromRequest q => Request -> Maybe q
fromRequest (Request _ m p _) = parseParams m >>= flip parseMaybe p

instance FromRequest Value where
    parseParams = const $ Just return

instance FromRequest () where
    parseParams = const . Just . const $ return ()

instance FromJSON Request where
    parseJSON = withObject "request" $ \o -> do
        (v, i, m, p) <- parseVerIdMethParams o
        guard $ i /= IdNull
        return $ Request v m p i

class ToRequest q where
    -- | Method associated with request data to build a request object.
    requestMethod :: q -> Method

instance ToRequest Value where
    requestMethod = const "json"

instance ToRequest () where
    requestMethod = const "json"

buildRequest :: (ToJSON q, ToRequest q)
             => Ver             -- ^ JSON-RPC version
             -> q               -- ^ Request data
             -> Id
             -> Request
buildRequest ver q = Request ver (requestMethod q) (toJSON q)



--
-- Responses
--

data Response = Response { getResVer :: !Ver
                         , getResult :: !Value
                         , getResId  :: !Id
                         } deriving (Eq, Show)

instance NFData Response where
    rnf (Response v r i) = rnf v `seq` rnf r `seq` rnf i

instance ToJSON Response where
    toJSON (Response V1 r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (Response V2 r i) = object
        [jr2, "id" .= i, "result" .= r]

class FromResponse r where
    -- | Parser for result Value in JSON-RPC response.
    -- Method corresponds to request to which this response answers.
    parseResult :: Method -> Maybe (Value -> Parser r)

fromResponse :: FromResponse r => Method -> Response -> Maybe r
fromResponse m (Response _ r _) = parseResult m >>= flip parseMaybe r

instance FromResponse Value where
    parseResult = const $ Just return

instance FromResponse () where
    parseResult = const . Just . const $ return ()

instance FromJSON Response where
    parseJSON = withObject "response" $ \o -> do
        i <- o .: "id"
        guard $ i /= IdNull
        r <- o .: "result"
        guard $ r /= Null
        v <- parseVer o
        return $ Response v r i

buildResponse :: (Monad m, FromRequest q, ToJSON r)
              => Respond q m r
              -> Request
              -> m (Either RpcError Response)
buildResponse f req@(Request v _ p i) = case fromRequest req of
    Nothing -> return . Left $ RpcError v (errorInvalid p) i
    Just q -> do
        rE <- f q
        return $ either (\e -> Left $ RpcError v e i)
                        (\r -> Right $ Response v (toJSON r) i) rE

type Respond q m r = q -> m (Either ErrorObj r)


--
-- Notifications
--

data Notif = Notif  { getNotifVer    :: !Ver
                    , getNotifMethod :: !Method
                    , getNotifParams :: !Value
                    } deriving (Eq, Show)

instance NFData Notif where
    rnf (Notif v m n) = rnf v `seq` rnf m `seq` rnf n

instance ToJSON Notif where
    toJSON (Notif V2 m p) = object $ case p of
        Null -> [jr2, "method" .= m]
        _    -> [jr2, "method" .= m, "params" .= p]
    toJSON (Notif V1 m p) = object $ case p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= Null]
        _    -> ["method" .= m, "params" .= p, "id" .= Null]

class FromNotif n where
    -- | Parser for notification params Value.
    parseNotif :: Method -> Maybe (Value -> Parser n)

fromNotif :: FromNotif n => Notif -> Maybe n
fromNotif (Notif _ m n) = parseNotif m >>= flip parseMaybe n

instance FromNotif Value where
    parseNotif = const $ Just return

instance FromNotif () where
    parseNotif = const . Just . const $ return ()

instance FromJSON Notif where
    parseJSON = withObject "notification" $ \o -> do
        (v, i, m, p) <- parseVerIdMethParams o
        guard $ i == IdNull
        return $ Notif v m p

class ToNotif n where
    notifMethod :: n -> Method

instance ToNotif Value where
    notifMethod = const "json"

instance ToNotif () where
    notifMethod = const "json"

buildNotif :: (ToJSON n, ToNotif n)
           => Ver
           -> n
           -> Notif
buildNotif ver n = Notif ver (notifMethod n) (toJSON n)



--
-- Errors
--

-- Error object from JSON-RPC 2.0. ErrorVal for backwards compatibility.
data ErrorObj = ErrorObj  { getErrMsg  :: !String
                          , getErrCode :: !Int
                          , getErrData :: !Value
                          }
              | ErrorVal  { getErrData :: !Value }
              deriving (Show, Eq)

instance NFData ErrorObj where
    rnf (ErrorObj m c d) = rnf m `seq` rnf c `seq` rnf d
    rnf (ErrorVal v) = rnf v

instance FromJSON ErrorObj where
    parseJSON Null = mzero
    parseJSON v@(Object o) = p1 <|> p2 where
        p1 = do
            m <- o .: "message"
            c <- o .: "code"
            d <- o .:? "data" .!= Null
            return $ ErrorObj m c d
        p2 = return $ ErrorVal v
    parseJSON v = return $ ErrorVal v

instance ToJSON ErrorObj where
    toJSON (ErrorObj s i d) = object $ ["message" .= s, "code" .= i]
        ++ if d == Null then [] else ["data" .= d]
    toJSON (ErrorVal v) = v

fromError :: ErrorObj -> String
fromError (ErrorObj m _ _) = m
fromError (ErrorVal v) = T.unpack $ decodeUtf8 $ L.toStrict $ encode v

data RpcError = RpcError { getErrVer  :: !Ver
                         , getErrObj  :: !ErrorObj
                         , getErrId   :: !Id
                         } deriving (Eq, Show)

instance NFData RpcError where
    rnf (RpcError v o i) = rnf v `seq` rnf o `seq` rnf i

instance FromJSON RpcError where
    parseJSON = withObject "error" $ \o -> do
        v <- parseVer o
        e <- o .: "error"
        i <- o .:? "id" .!= IdNull
        return $ RpcError v e i

instance ToJSON RpcError where
    toJSON (RpcError V1 o i) =
        object ["id" .= i, "result" .= Null, "error" .= o]
    toJSON (RpcError V2 o i) =
        object ["id" .= i, "error" .= o, jr2]

-- | Parse error.
errorParse :: Value -> ErrorObj
errorParse = ErrorObj "Parse error" (-32700)

-- | Invalid request.
errorInvalid :: Value -> ErrorObj
errorInvalid = ErrorObj "Invalid request" (-32600)

-- | Invalid params.
errorParams :: Value -> ErrorObj
errorParams = ErrorObj "Invalid params" (-32602)

-- | Method not found.
errorMethod :: Method -> ErrorObj
errorMethod = ErrorObj "Method not found" (-32601) . toJSON

-- | Id not recognized.
errorId :: Id -> ErrorObj
errorId = ErrorObj "Id not recognized" (-32000) . toJSON


--
-- Messages
--

data Message
    = MsgRequest   { getMsgRequest  :: !Request  }
    | MsgResponse  { getMsgResponse :: !Response }
    | MsgNotif     { getMsgNotif    :: !Notif    }
    | MsgError     { getMsgError    :: !RpcError }
    deriving (Eq, Show)

instance NFData Message where
    rnf (MsgRequest  q) = rnf q
    rnf (MsgResponse r) = rnf r
    rnf (MsgNotif    n) = rnf n
    rnf (MsgError    e) = rnf e

instance ToJSON Message where
    toJSON (MsgRequest  q) = toJSON q
    toJSON (MsgResponse r) = toJSON r
    toJSON (MsgNotif    n) = toJSON n
    toJSON (MsgError    e) = toJSON e

instance FromJSON Message where
    parseJSON v = (MsgRequest  <$> parseJSON v)
              <|> (MsgResponse <$> parseJSON v)
              <|> (MsgNotif    <$> parseJSON v)
              <|> (MsgError    <$> parseJSON v)

--
-- Types
--

type Method = Text

data Id = IdInt { getIdInt :: !Int  }
        | IdTxt { getIdTxt :: !Text }
        | IdNull
    deriving (Eq, Show, Read, Generic)

instance Hashable Id

instance NFData Id where
    rnf (IdInt i) = rnf i
    rnf (IdTxt t) = rnf t
    rnf _ = ()

instance Enum Id where
    toEnum = IdInt
    fromEnum (IdInt i) = i
    fromEnum _ = error "Can't enumerate non-integral ids"

instance FromJSON Id where
    parseJSON s@(String _) = IdTxt <$> parseJSON s
    parseJSON n@(Number _) = IdInt <$> parseJSON n
    parseJSON Null = return IdNull
    parseJSON _ = mzero

instance ToJSON Id where
    toJSON (IdTxt s) = toJSON s
    toJSON (IdInt n) = toJSON n
    toJSON IdNull = Null

data Ver = V1 -- ^ JSON-RPC 1.0
         | V2 -- ^ JSON-RPC 2.0
         deriving (Eq, Show, Read)

instance NFData Ver where
    rnf v = v `seq` ()



--
-- Helpers
--

jr2 :: Pair
jr2 = "jsonrpc" .= ("2.0" :: Text)

parseVer :: Object -> Parser Ver
parseVer o = do
    j <- o .:? "jsonrpc"
    return $ if j == Just ("2.0" :: Text) then V2 else V1

parseVerIdMethParams :: Object -> Parser (Ver, Id, Method, Value)
parseVerIdMethParams o = do
    v <- parseVer o
    i <- o .:? "id" .!= IdNull
    m <- o .: "method"
    p <- o .:? "params" .!= Null
    return (v, i, m, p)
