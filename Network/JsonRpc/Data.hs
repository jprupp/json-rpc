{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Implementation of basic JSON-RPC data types.
module Network.JsonRpc.Data
( -- * Requests
  Request(..)
  -- ** Parsing
, FromRequest(..)
, parseRequest
  -- ** Encoding
, ToRequest(..)
, buildRequest

  -- * Responses
, Response(..)
  -- ** Parsing
, FromResponse(..)
, parseResponse

  -- * Notifications
, Notif(..)
  -- ** Parsing
, FromNotif(..)
, parseNotif
  -- ** Encoding
, ToNotif(..)
, buildNotif

  -- * Errors
, ErrorObj(..)
, ErrorMsg(..)
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

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (when, guard, mzero)
import Data.Aeson.Types
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)



--
-- Requests
--

-- Request data type.
data Request q = Request { getReqVer      :: !Ver       -- Version
                         , getReqMethod   :: !Method    -- Method
                         , getReqParams   :: !q         -- Params
                         , getReqId       :: !Id        -- Id
                         } deriving (Eq, Show, Read)

instance NFData q => NFData (Request q) where
    rnf (Request v m q i) = rnf v `seq` rnf m `seq` rnf q `seq` rnf i

instance ToJSON q => ToJSON (Request q) where
    toJSON (Request V2 m p i) = object $ case toJSON p of
        Null -> [jr2, "method" .= m, "id" .= i]
        v    -> [jr2, "method" .= m, "id" .= i, "params" .= v]
    toJSON (Request V1 m p i) = object $ case toJSON p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= i]
        v    -> ["method" .= m, "params" .= v, "id" .= i]

-- | Class for data that can be received in JSON-RPC requests.
class FromRequest q where
    -- | Parser for params field.
    paramsParser :: Method -> Maybe (Value -> Parser q)

instance FromRequest Value where
    paramsParser _ = Just return

instance FromRequest () where
    paramsParser _ = Nothing

-- | Parse JSON-RPC request.
parseRequest :: FromRequest q => Value -> Parser (Either ErrorMsg (Request q))
parseRequest = withObject "request" $ \o -> do
    j <- o .:? "jsonrpc"
    i <- o .:  "id"
    when (i == IdNull) $ fail "Request must have non-null id"
    m <- o .:  "method"
    p <- o .:? "params" .!= Null
    let ver = if j == Just ("2.0" :: Text) then V2 else V1
    case paramsParser m of
        Nothing -> return $ Left $ ErrorMsg ver (errorMethod m) i
        Just x  -> parseIt ver m i x p
               <|> return (Left $ ErrorMsg ver (errorParams p) i)
  where
    parseIt ver m i x p = (\y -> Right (Request ver m y i)) <$> x p

-- | Class for data that can be sent as JSON-RPC requests.
-- Define a method name for each request.
class ToRequest q where
    requestMethod :: q -> Method

instance ToRequest Value where
    requestMethod _ = ""

instance ToRequest () where
    requestMethod _ = undefined

-- Build JSON-RPC request.
buildRequest :: ToRequest q
             => Ver             -- ^ JSON-RPC version
             -> q               -- ^ Request data
             -> Request q
buildRequest ver q = Request ver (requestMethod q) q IdNull



--
-- Responses
--

-- | JSON-RPC response data type
data Response r = Response  { getResVer :: !Ver             -- ^ Version
                            , getResult :: !r               -- ^ Result
                            , getResId  :: !Id              -- ^ Id
                            } deriving (Eq, Show, Read)

instance NFData r => NFData (Response r) where
    rnf (Response v r i) = rnf v `seq` rnf r `seq` rnf i

instance ToJSON r => ToJSON (Response r) where
    toJSON (Response V1 r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (Response V2 r i) = object
        [jr2, "id" .= i, "result" .= r]

-- | Class for data that can be received inside JSON-RPC responses.
class FromResponse r where
    -- | Parse result field from JSON-RPC response.
    parseResult :: Method -> Value -> Parser r

instance FromResponse Value where
    parseResult _ = return

instance FromResponse () where
    parseResult _ _ = return ()

-- | Parse JSON-RPC response.
parseResponse :: FromResponse r
              => Request q -> Value -> Parser (Either ErrorMsg (Response r))
parseResponse rq = withObject "response" $ \o -> do
    let m  = getReqMethod rq
        qi = getReqId rq
    j <- o .:? "jsonrpc"
    i <- o .: "id"
    when (i == IdNull) $ fail "Response must have non-null id"
    when (qi /= i) $ fail "Response id mismatch"
    let ver = if j == Just ("2.0" :: Text) then V2 else V1
    (Right <$> parseRes ver i m o) <|> (Left <$> parseJSON (Object o))
  where
    parseRes ver i m o = do
        v <- o .: "result"
        guard $ v /= Null
        r <- parseResult m v
        return $ Response ver r i



--
-- Notifications
--

-- | Class for JSON-RPC notifications.
data Notif n = Notif  { getNotifVer    :: !Ver          -- ^ Version
                      , getNotifMethod :: !Method       -- ^ Method
                      , getNotifParams :: !n            -- ^ Params
                      } deriving (Eq, Show, Read)

instance NFData n => NFData (Notif n) where
    rnf (Notif v m n) = rnf v `seq` rnf m `seq` rnf n

instance ToJSON n => ToJSON (Notif n) where
    toJSON (Notif V2 m p) = object $ case toJSON p of
        Null -> [jr2, "method" .= m]
        v    -> [jr2, "method" .= m, "params" .= v]
    toJSON (Notif V1 m p) = object $ case toJSON p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= Null]
        v    -> ["method" .= m, "params" .=          v, "id" .= Null]

-- | Class for data that can be received in JSON-RPC notifications.
class FromNotif n where
    -- | Parser for notification params field
    notifParamsParser :: Method -> Maybe (Value -> Parser n)

instance FromNotif Value where
    notifParamsParser _ = Just return

instance FromNotif () where
    notifParamsParser _ = Nothing

-- | Parse notifications.
parseNotif :: FromNotif n => Value -> Parser (Either ErrorMsg (Notif n))
parseNotif = withObject "notification" $ \o -> do
    j <- o .:? "jsonrpc"
    i <- o .:? "id" .!= IdNull
    m <- o .:  "method"
    p <- o .:? "params" .!= Null
    guard $ i == IdNull
    let ver = if j == Just ("2.0" :: Text) then V2 else V1
    case notifParamsParser m of
        Nothing -> return $ Left $ ErrorMsg ver (errorMethod m) IdNull
        Just x  -> f ver m x p
               <|> return (Left $ ErrorMsg ver (errorParams p) i)
  where
    f ver m x p = (Right . Notif ver m) <$> x p

class ToNotif n where
    notifMethod :: n -> Method

instance ToNotif Value where
    notifMethod _ = ""

instance ToNotif () where
    notifMethod _ = undefined

-- | Build notifications.
buildNotif :: ToNotif n
           => Ver           -- ^ Version
           -> n             -- ^ Notification data
           -> Notif n
buildNotif ver n = Notif ver (notifMethod n) n



--
-- Errors
--

-- | JSON-RPC errors.
data ErrorObj = ErrorObj  { getErrMsg  :: !String        -- ^ Message
                          , getErrCode :: !Int           -- ^ Error code (2.0)
                          , getErrData :: !Value         -- ^ Error data (2.0)
                          } deriving (Show, Eq)

data ErrorMsg = ErrorMsg { getErrVer  :: !Ver           -- ^ Version
                         , getErrObj  :: !ErrorObj      -- ^ Object
                         , getErrId   :: !Id            -- ^ Error id
                         } deriving (Eq, Show)

instance NFData ErrorObj where
    rnf (ErrorObj m c d) = rnf m `seq` rnf c `seq` rnf d

instance NFData ErrorMsg where
    rnf (ErrorMsg v o i) = rnf v `seq` rnf o `seq` rnf i

instance FromJSON ErrorMsg where
    parseJSON = withObject "error" $ \o -> do
        i <- o .:? "id" .!= IdNull
        j <- o .:? "jsonrpc"
        let ver = if j == Just ("2.0" :: Text) then V2 else V1
        case ver of
            V2 ->  (\p -> ErrorMsg V2 p i) <$> (e2 =<< o .: "error")
            V1 -> ((\p -> ErrorMsg V1 p i) <$> (e1 =<< o .: "error"))
              <|> ((\p -> ErrorMsg V1 p i) <$> (e1 =<< o .: "result"))
                  -- Buggy V1 servers sometimes put errors in result field
      where
        e1 v = case v of
            String t -> return $ ErrorObj (T.unpack t) 0 Null
            _        -> return $ ErrorObj "" 0 v
        e2 v = case v of
            Object b -> ErrorObj <$> b .: "message" <*> b .:  "code"
                                 <*> b .:? "data" .!= Null
            _ -> fail "JSON-RPC 2.0 error must be a JSON object"

instance ToJSON ErrorMsg where
    toJSON (ErrorMsg V2 o i) = object [jr2, "id" .= i, "error" .= o'] where
        o' = object $ ["code" .= getErrCode o, "message" .= getErrMsg o] ++
            if getErrData o == Null then [] else ["data" .= getErrData o]
    toJSON (ErrorMsg V1 o i) =
        object ["id" .= i, "error" .= getErrMsg o, "result" .= Null]

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

-- | Class for any JSON-RPC message.
data Message q r n
    = MsgRequest   { getMsgRequest  :: !(Request  q) }
    | MsgResponse  { getMsgResponse :: !(Response r) }
    | MsgNotif     { getMsgNotif    :: !(Notif    n) }
    | MsgError     { getMsgError    :: !ErrorMsg     }
    deriving (Eq, Show)

instance (NFData q, NFData n, NFData r) => NFData (Message q r n) where
    rnf (MsgRequest  q) = rnf q
    rnf (MsgResponse r) = rnf r
    rnf (MsgNotif    n) = rnf n
    rnf (MsgError    e) = rnf e

instance (ToJSON q, ToJSON n, ToJSON r) => ToJSON (Message q n r) where
    toJSON (MsgRequest  rq) = toJSON rq
    toJSON (MsgResponse rs) = toJSON rs
    toJSON (MsgNotif    rn) = toJSON rn
    toJSON (MsgError     e) = toJSON e

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

instance NFData Ver



--
-- Helpers
--

jr2 :: Pair
jr2 = "jsonrpc" .= ("2.0" :: Text)

