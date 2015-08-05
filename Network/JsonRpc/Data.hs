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
, dummyRespond

  -- * Notifications
, Notif(..)
  -- ** Parsing
, FromNotif(..)
, fromNotif
  -- ** Encoding
, ToNotif(..)
, buildNotif

  -- * Errors
, ErrorObj(..)
, Error(..)
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
import Data.Aeson.Types hiding (Error)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import GHC.Generics (Generic)



--
-- Requests
--

-- Request data type.
data Request = Request { getReqVer      :: !Ver       -- Version
                       , getReqMethod   :: !Method    -- Method
                       , getReqParams   :: !Value     -- Params
                       , getReqId       :: !Id        -- Id
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

-- | Class for data that can be received in JSON-RPC requests.
class FromRequest q where
    -- | Parser for params field.
    parseParams :: Method -> Maybe (Value -> Parser q)

fromRequest :: FromRequest q => Request -> Maybe q
fromRequest (Request _ m p _) = parseParams m >>= flip parseMaybe p

instance FromRequest Value where
    parseParams = const $ Just return

instance FromRequest () where
    parseParams = const Nothing

instance FromJSON Request where
    parseJSON = withObject "request" $ \o -> do
        (v, i, m, p) <- parseVerIdMethParams o
        guard $ i /= IdNull
        return $ Request v m p i

-- | Class for data that can be sent as JSON-RPC requests.
-- Define a method name for each request.
class ToRequest q where
    requestMethod :: q -> Method

instance ToRequest Value where
    requestMethod _ = ""

instance ToRequest () where
    requestMethod _ = undefined

-- Build JSON-RPC request.
buildRequest :: (ToJSON q, ToRequest q)
             => Ver             -- ^ JSON-RPC version
             -> q               -- ^ Request data
             -> Id
             -> Request
buildRequest ver q = Request ver (requestMethod q) (toJSON q)



--
-- Responses
--

-- | JSON-RPC response data type
data Response = Response { getResVer :: !Ver    -- ^ Version
                         , getResult :: !Value  -- ^ Result
                         , getResId  :: !Id     -- ^ Id
                         } deriving (Eq, Show)

instance NFData Response where
    rnf (Response v r i) = rnf v `seq` rnf r `seq` rnf i

instance ToJSON Response where
    toJSON (Response V1 r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (Response V2 r i) = object
        [jr2, "id" .= i, "result" .= r]

-- | Class for data that can be received inside JSON-RPC responses.
class FromResponse r where
    -- | Parse result field from JSON-RPC response.
    parseResult :: Method -> Maybe (Value -> Parser r)

fromResponse :: FromResponse r => Method -> Response -> Maybe r
fromResponse m (Response _ r _) = parseResult m >>= flip parseMaybe r

type Respond q m r = q -> m (Either ErrorObj r)

dummyRespond :: Monad m => Respond () m ()
dummyRespond = const . return $ Right () 

instance FromResponse Value where
    parseResult = const $ Just return

instance FromResponse () where
    parseResult = const Nothing

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
              -> m (Either Error Response)
buildResponse f req@(Request v _ p i) = case fromRequest req of
    Nothing -> return . Left $ Error v (errorInvalid v p) i
    Just q -> do
        rE <- f q
        return $ either (\e -> Left $ Error v e i)
                        (\r -> Right $ Response v (toJSON r) i) rE


--
-- Notifications
--

-- | Class for JSON-RPC notifications.
data Notif = Notif  { getNotifVer    :: !Ver          -- ^ Version
                    , getNotifMethod :: !Method       -- ^ Method
                    , getNotifParams :: !Value        -- ^ Params
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

-- | Class for data that can be received in JSON-RPC notifications.
class FromNotif n where
    -- | Parser for notification params field
    parseNotif :: Method -> Maybe (Value -> Parser n)

fromNotif :: FromNotif n => Notif -> Maybe n
fromNotif (Notif _ m n) = parseNotif m >>= flip parseMaybe n

instance FromNotif Value where
    parseNotif _ = Just return

instance FromNotif () where
    parseNotif _ = Nothing

instance FromJSON Notif where
    -- | Parse notifications.
    parseJSON = withObject "notification" $ \o -> do
        (v, i, m, p) <- parseVerIdMethParams o
        guard $ i == IdNull
        return $ Notif v m p

class ToNotif n where
    notifMethod :: n -> Method

instance ToNotif Value where
    notifMethod _ = ""

instance ToNotif () where
    notifMethod _ = undefined

-- | Build notifications.
buildNotif :: (ToJSON n, ToNotif n)
           => Ver           -- ^ Version
           -> n             -- ^ Notification data
           -> Notif
buildNotif ver n = Notif ver (notifMethod n) (toJSON n)



--
-- Errors
--

-- | JSON-RPC errors.
data ErrorObj = ErrorObj  { getErrMsg  :: !String    -- ^ Message
                          , getErrCode :: !Int       -- ^ Error code (2.0)
                          , getErrData :: !Value     -- ^ Error data (2.0)
                          }
              | ErrorStr  { getErrMsg  :: !String }  -- ^ Message
              | ErrorVal  { getErrData :: !Value  }  -- ^ Error data
              deriving (Show, Eq)

data Error = Error { getErrVer  :: !Ver              -- ^ Version
                   , getErrObj  :: !ErrorObj         -- ^ Object
                   , getErrId   :: !Id               -- ^ Error id
                   } deriving (Eq, Show)

instance NFData ErrorObj where
    rnf (ErrorObj m c d) = rnf m `seq` rnf c `seq` rnf d
    rnf (ErrorStr s) = rnf s
    rnf (ErrorVal v) = rnf v

instance NFData Error where
    rnf (Error v o i) = rnf v `seq` rnf o `seq` rnf i

instance FromJSON ErrorObj where
    parseJSON (Object o) = p1 <|> p2 where
        p1 = do
            m <- o .: "message"
            c <- o .: "code"
            d <- o .:? "data" .!= Null
            return $ ErrorObj m c d
        p2 = return $ ErrorVal (Object o)
    parseJSON (String t) = return $ ErrorStr (T.unpack t)
    parseJSON Null = mzero
    parseJSON v = return $ ErrorVal v

instance FromJSON Error where
    parseJSON = withObject "error" $ \o -> do
        v <- parseVer o
        e <- o .: "error"
        i <- o .:? "id" .!= IdNull
        return $ Error v e i

fromError :: ErrorObj -> String
fromError (ErrorObj m _ _) = m
fromError (ErrorStr s) = s
fromError (ErrorVal v) = T.unpack $ decodeUtf8 $ L.toStrict $ encode v

instance ToJSON ErrorObj where
    toJSON (ErrorObj s i d) = object $ ["message" .= s, "code" .= i]
        ++ if d == Null then [] else ["data" .= d]
    toJSON (ErrorStr s) = toJSON s
    toJSON (ErrorVal v) = v

instance ToJSON Error where
    toJSON (Error V1 o i) =
        object ["id" .= i, "result" .= Null, "error" .= o]
    toJSON (Error V2 o i) =
        object ["id" .= i, "error" .= o]

-- | Parse error.
errorParse :: Ver -> Value -> ErrorObj
errorParse V2 = ErrorObj "Parse error" (-32700)
errorParse V1 = const $ ErrorStr "Parse error"

-- | Invalid request.
errorInvalid :: Ver -> Value -> ErrorObj
errorInvalid V2 = ErrorObj "Invalid request" (-32600)
errorInvalid V1 = const $ ErrorStr "Invalid request"

-- | Invalid params.
errorParams :: Ver -> Value -> ErrorObj
errorParams V2 = ErrorObj "Invalid params" (-32602)
errorParams V1 = const $ ErrorStr "Invalid params"

-- | Method not found.
errorMethod :: Ver -> Method -> ErrorObj
errorMethod V2 = ErrorObj "Method not found" (-32601) . toJSON
errorMethod V1 = const $ ErrorStr "Method not found"

-- | Id not recognized.
errorId :: Ver -> Id -> ErrorObj
errorId V2 = ErrorObj "Id not recognized" (-32000) . toJSON
errorId V1 = const $ ErrorStr "Id not recognized"



--
-- Messages
--

-- | Class for any JSON-RPC message.
data Message
    = MsgRequest   { getMsgRequest  :: !Request  }
    | MsgResponse  { getMsgResponse :: !Response }
    | MsgNotif     { getMsgNotif    :: !Notif    }
    | MsgError     { getMsgError    :: !Error }
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

-- | JSON-RPC methods in requests and notifications.
type Method = Text

-- | JSON-RPC message id.
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

-- | JSON-RPC version
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
