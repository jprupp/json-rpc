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
import GHC.Generics (Generic)



--
-- Requests
--

data Request q = Request { getReqVer      :: !Ver
                         , getReqMethod   :: !Method
                         , getReqParams   :: !q
                         , getReqId       :: !Id
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

class FromRequest q where
    -- | Parser for params field.
    paramsParser :: Method -> Maybe (Value -> Parser q)

instance FromRequest Value where
    paramsParser _ = Just return

instance FromRequest () where
    paramsParser _ = Nothing

-- Parse request message.
parseRequest :: FromRequest q => Value -> Parser (Either ErrorObj (Request q))
parseRequest = withObject "request" $ \o -> do
    j <- o .:? "jsonrpc"
    i <- o .:  "id"
    when (i == IdNull) $ fail "Request must have non-null id"
    m <- o .:  "method"
    p <- o .:? "params" .!= Null
    let ver = if j == Just ("2.0" :: Text) then V2 else V1
    case paramsParser m of
        Nothing -> return (Left (errorMethod ver m i))
        Just x -> parseIt ver m i x p <|> return (Left (errorParams ver p i))
  where
    parseIt ver m i x p = (\y -> Right (Request ver m y i)) <$> x p

-- For data that can be sent in JSON-RPC request messages.
-- Define a method for each possible request.
class ToRequest q where
    requestMethod :: q -> Method

instance ToRequest Value where
    requestMethod _ = ""

instance ToRequest () where
    requestMethod _ = undefined

buildRequest :: ToRequest q => Ver -> q -> Request q
buildRequest ver q = Request ver (requestMethod q) q IdNull



--
-- Responses
--

data Response r = Response  { getResVer :: !Ver
                            , getResult :: !r
                            , getResId  :: !Id
                            } deriving (Eq, Show, Read)

instance NFData r => NFData (Response r) where
    rnf (Response v r i) = rnf v `seq` rnf r `seq` rnf i

instance ToJSON r => ToJSON (Response r) where
    toJSON (Response V1 r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (Response V2 r i) = object
        [jr2, "id" .= i, "result" .= r]

class FromResponse r where
    -- | Parse result field from JSON-RPC response.
    parseResult :: Method -> Value -> Parser r

instance FromResponse Value where
    parseResult _ = return

instance FromResponse () where
    parseResult _ _ = return ()

-- | Parse JSON-RPC response message.
parseResponse :: FromResponse r
              => Request q -> Value -> Parser (Either ErrorObj (Response r))
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

data Notif n = Notif  { getNotifVer    :: !Ver
                      , getNotifMethod :: !Method
                      , getNotifParams :: !n
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

class FromNotif n where
    -- | Parser for notification params field
    notifParamsParser :: Method -> Maybe (Value -> Parser n)

instance FromNotif Value where
    notifParamsParser _ = Just return

instance FromNotif () where
    notifParamsParser _ = Nothing

-- | Parse notification messages.
parseNotif :: FromNotif n => Value -> Parser (Either ErrorObj (Notif n))
parseNotif = withObject "notification" $ \o -> do
    j <- o .:? "jsonrpc"
    i <- o .:? "id" .!= IdNull
    m <- o .:  "method"
    p <- o .:? "params" .!= Null
    guard $ i == IdNull
    let ver = if j == Just ("2.0" :: Text) then V2 else V1
    case notifParamsParser m of
        Nothing -> return (Left $ errorMethod ver m IdNull)
        Just x -> f ver m x p <|> return (Left (errorParams ver p i))
  where
    f ver m x p = (Right . Notif ver m) <$> x p

class ToNotif n where
    notifMethod :: n -> Method

instance ToNotif Value where
    notifMethod _ = ""

instance ToNotif () where
    notifMethod _ = undefined

buildNotif :: ToNotif n => Ver -> n -> Notif n
buildNotif ver n = Notif ver (notifMethod n) n



--
-- Errors
--

data ErrorObj = ErrorObj { getErrVer  :: !Ver
                         , getErrMsg  :: !String
                         , getErrCode :: !Int
                         , getErrData :: !Value
                         , getErrId   :: !Id
                         } deriving (Eq, Show)

instance NFData ErrorObj where
    rnf (ErrorObj v m c d i) =
        rnf v `seq` rnf m `seq` rnf c `seq` rnf d `seq` rnf i

instance FromJSON ErrorObj where
    parseJSON = withObject "error" $ \o -> do
        i <- o .:? "id" .!= IdNull
        j <- o .:? "jsonrpc"
        let ver = if j == Just ("2.0" :: Text) then V2 else V1
        case ver of
            V2 -> o .: "error" >>= \e -> case e of
                (Object b) -> ErrorObj V2 <$> b .: "message"
                                          <*> b .: "code"
                                          <*> b .:? "data" .!= Null
                                          <*> return i
                _ -> fail "JSON-RPC 2.0 error must be a JSON object"
            V1 -> o .: "error" >>= \e -> return $ ErrorObj V1 e 0 Null i

instance ToJSON ErrorObj where
    toJSON (ErrorObj V2 m c d i) = object [jr2, "id" .= i, "error" .= o] where
        o = case d of
            Null -> object ["code" .= c, "message" .= m]
            _    -> object ["code" .= c, "message" .= m, "data" .= d]
    toJSON (ErrorObj V1 m _ _ i) =
        object ["id" .= i, "error" .= m, "result" .= Null]

-- | Parse error.
errorParse :: Ver -> Value -> ErrorObj
errorParse ver v = ErrorObj ver "Parse error" (-32700) v IdNull

-- | Invalid request.
errorInvalid :: Ver -> Value -> ErrorObj
errorInvalid ver v = ErrorObj ver "Invalid request" (-32600) v IdNull

-- | Invalid params.
errorParams :: Ver -> Value -> Id -> ErrorObj
errorParams ver v i = ErrorObj ver "Invalid params" (-32602) v i

-- | Method not found.
errorMethod :: Ver -> Method -> Id -> ErrorObj
errorMethod ver m i = ErrorObj ver "Method not found" (-32601) (toJSON m) i

-- | Id not recognized.
errorId :: Ver -> Id -> ErrorObj
errorId ver i = ErrorObj ver "Id not recognized" (-32000) (toJSON i) IdNull



--
-- Messages
--

data Message q n r
    = MsgRequest   { getMsgRequest  :: !(Request  q) }
    | MsgNotif     { getMsgNotif    :: !(Notif    n) }
    | MsgResponse  { getMsgResponse :: !(Response r) }
    | MsgError     { getMsgError    :: !ErrorObj     }
    deriving (Eq, Show)

instance (NFData q, NFData n, NFData r) => NFData (Message q n r) where
    rnf (MsgRequest  q) = rnf q
    rnf (MsgNotif    n) = rnf n
    rnf (MsgResponse r) = rnf r
    rnf (MsgError    e) = rnf e

instance (ToJSON q, ToJSON n, ToJSON r) => ToJSON (Message q n r) where
    toJSON (MsgRequest  rq) = toJSON rq
    toJSON (MsgNotif    rn) = toJSON rn
    toJSON (MsgResponse rs) = toJSON rs
    toJSON (MsgError     e) = toJSON e

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
    toEnum i = IdInt i
    fromEnum (IdInt i) = i
    fromEnum _ = error $ "Can't enumerate non-integral ids"

instance FromJSON Id where
    parseJSON s@(String _) = IdTxt <$> parseJSON s
    parseJSON n@(Number _) = IdInt <$> parseJSON n
    parseJSON Null = return $ IdNull
    parseJSON _ = mzero

instance ToJSON Id where
    toJSON (IdTxt s) = toJSON s
    toJSON (IdInt n) = toJSON n
    toJSON IdNull = Null

data Ver = V1 | V2 deriving (Eq, Show, Read)

instance NFData Ver



--
-- Helpers
--

jr2 :: Pair
jr2 = "jsonrpc" .= ("2.0" :: Text)

