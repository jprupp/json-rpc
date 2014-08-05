{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Implementation of basic JSON-RPC data types.
module Network.JsonRpc.Data
( -- * Requests
  Request(..)
  -- ** Parsing
, FromRequest
, paramsParser
, parseRequest
  -- ** Encoding
, ToRequest
, requestMethod
, buildRequest

  -- * Responses
, Response(..)
  -- ** Parsing
, FromResponse
, parseResult
, parseResponse

  -- * Notifications
, Notif(..)
  -- ** Parsing
, FromNotif
, notifParamsParser
, parseNotif
  -- ** Encoding
, ToNotif
, notifMethod
, buildNotif

  -- * Errors
, Error(..)
, ErrorObj(..)
  -- ** Parsing
, parseError
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

) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (when, guard, mzero)
import Data.Aeson (encode)
import Data.Aeson.Types hiding (Error)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)


--
-- Requests
--

data Request q
    = Request1  { getReqMethod   :: !Method
                , getReqParams   :: !q
                , getReqId       :: !Id
                }
    | Request   { getReqMethod   :: !Method
                , getReqParams   :: !q
                , getReqId       :: !Id
                }
    deriving (Eq, Show, Read)

instance NFData q => NFData (Request q) where
    rnf (Request  m q i) = rnf m `seq` rnf q `seq` rnf i
    rnf (Request1 m q i) = rnf m `seq` rnf q `seq` rnf i

instance ToJSON q => ToJSON (Request q) where
    toJSON (Request m p i) = object $ case toJSON p of
        Null -> [jr2, "method" .= m, "id" .= i]
        v -> [jr2, "method" .= m, "params" .= v, "id" .= i]
    toJSON (Request1 m p i) = object $ case toJSON p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= i]
        v -> ["method" .= m, "params" .= v, "id" .= i]

class FromRequest q where
    paramsParser :: Method -> Maybe (Value -> Parser q)

instance FromRequest Value where
    paramsParser _ = Just return

instance FromRequest () where
    paramsParser _ = Nothing

parseRequest :: FromRequest q => Value -> Parser (Either Error (Request q))
parseRequest = withObject "request" $ \o -> do
    rM <- o .:? "jsonrpc"
    i  <- o .:  "id"
    when (i == IdNull) $ fail "Request must have non-null id"
    m  <- o .:  "method"
    p  <- o .:? "params" .!= Null
    let r1 = rM /= Just ("2.0" :: Text)
    case paramsParser m of
        Nothing -> return (Left   $  errorMethod r1 m i)
        Just  x ->        (Right <$> parseIt r1 m i x p)
               <|> return (Left   $  errorParams r1 p i)
  where
    parseIt r1 m i x p = x p >>= \y ->
        return $ if r1 then Request1 m y i else Request  m y i

class ToRequest q where
    requestMethod :: q -> Method

instance ToRequest Value where
    requestMethod _ = ""

instance ToRequest () where
    requestMethod _ = undefined

buildRequest :: ToRequest q => q -> Request q
buildRequest q = Request (requestMethod q) q IdNull

--
-- Responses
--

data Response r
    = Response1 { getResult    :: !r
                , getResId     :: !Id
                }
    | Response  { getResult    :: !r
                , getResId     :: !Id
                }
    deriving (Eq, Show, Read)

instance NFData r => NFData (Response r) where
    rnf (Response1 r i) = rnf r `seq` rnf i
    rnf (Response  r i) = rnf r `seq` rnf i

instance ToJSON r => ToJSON (Response r) where
    toJSON (Response1 r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (Response r i) = object
        [jr2, "id" .= i, "result" .= r]

class FromResponse r where
    parseResult :: Method -> Value -> Parser r

instance FromResponse Value where
    parseResult _ = return

instance FromResponse () where
    parseResult _ _ = return ()

parseResponse :: FromResponse r
            => Request q -> Value -> Parser (Either Error (Response r))
parseResponse rq = withObject "response" $ \o -> do
    let m  = getReqMethod rq
        qi = getReqId rq
    r <- o .:? "jsonrpc"
    i <- o .: "id"
    when (i == IdNull) $ fail "Response must have non-null id"
    when (qi /= i) $ fail "Response id mismatch"
    let r1 = r /= Just ("2.0" :: Text)
    (Right <$> parseRes r1 i m o) <|> (Left <$> parseError i o)
  where
    parseRes r1 i m o = do
        v <- o .:? "result" .!= Null
        guard $ v /= Null
        r <- parseResult m v
        return $ if r1 then Response1 r i else Response r i

--
-- Notifications
--

data Notif n
    = Notif1 { getNotifMethod :: !Method
             , getNotifParams :: !n
             }
    | Notif  { getNotifMethod :: !Method
             , getNotifParams :: !n
             }
    deriving (Eq, Show, Read)

instance NFData n => NFData (Notif n) where
    rnf (Notif  m n) = rnf m `seq` rnf n
    rnf (Notif1 m n) = rnf m `seq` rnf n

instance ToJSON n => ToJSON (Notif n) where
    toJSON (Notif m p) = object $ case toJSON p of
        Null -> [jr2, "method" .= m]
        v -> [jr2, "method" .= m, "params" .= v]
    toJSON (Notif1 m p) = object $ case toJSON p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= Null]
        v -> ["method" .= m, "params" .= v, "id" .= Null]

class FromNotif n where
    -- | Parse params field from JSON-RPC notification for given method.
    notifParamsParser :: Method -> Maybe (Value -> Parser n)

instance FromNotif Value where
    notifParamsParser _ = Just return

instance FromNotif () where
    notifParamsParser _ = Nothing

parseNotif :: FromNotif n => Value -> Parser (Either Error (Notif n))
parseNotif = withObject "notification" $ \o -> do
    r <- o .:? "jsonrpc"
    i <- o .:? "id" .!= IdNull
    m <- o .:  "method"
    p <- o .:? "params" .!= Null
    guard $ i == IdNull
    let r1 = r /= Just ("2.0" :: Text)
    case notifParamsParser m of
        Nothing -> return (Left   $  errorMethod r1 m IdNull)
        Just  x ->        (Right <$> parseIt r1 m x p)
               <|> return (Left   $  errorParams r1 p IdNull)
  where
    parseIt r1 m x p = x p >>= \y ->
        return $ if r1 then Notif1 m y else Notif m y

class ToNotif n where
    notifMethod :: n -> Method

instance ToNotif Value where
    notifMethod _ = ""

instance ToNotif () where
    notifMethod _ = undefined

buildNotif :: ToNotif n => n -> Notif n
buildNotif n = Notif (notifMethod n) n

--
-- Errors
--

data Error
    = Error1   { getErr     :: !String
               , getErrId   :: !Id
               }
    | Error    { getErrObj  :: !ErrorObj
               , getErrId   :: !Id
               }
    deriving (Eq, Show)

instance NFData Error where
    rnf (Error1 e i) = rnf e `seq` rnf i
    rnf (Error  o i) = rnf o `seq` rnf i

data ErrorObj
    = ErrorObj { getErrMsg  :: !String
               , getErrCode :: !Int
               , getErrData :: !Value
               }
    deriving (Eq, Show)

instance NFData ErrorObj where
    rnf (ErrorObj m c d) = rnf m `seq` rnf c `seq` rnf d

instance FromJSON ErrorObj where
    parseJSON = withObject "error" $ \o ->
        ErrorObj <$> o .: "message"
                 <*> o .: "code"
                 <*> o .:? "data" .!= Null

instance ToJSON ErrorObj where
    toJSON (ErrorObj m c d) = case d of
        Null -> object ["code" .= c, "message" .= m]
        _    -> object ["code" .= c, "message" .= m, "data" .= d]

instance ToJSON Error where
    toJSON (Error1 e i) = object
        ["id" .= i, "error" .= e, "result" .= Null]
    toJSON (Error o i) = object
        [jr2, "id" .= i, "error" .= o]

parseError :: Id -> Object -> Parser Error
parseError i o = parseErr <|> parseErr1 <|> parseRes1
  where
    parseErr1 = o .: "error"  >>= \e -> return $ Error1 e i
    parseRes1 = o .: "result" >>= \e -> return $ Error1 e i
    parseErr  = o .: "error"  >>= \e -> return $ Error  e i

errorParse :: Bool      -- ^ RPCv1
           -> String -> Error
errorParse r1 s = if r1
    then Error1 "Parse error" IdNull
    else Error (ErrorObj "Parse error" (-32700) (toJSON s)) IdNull

errorInvalid :: Bool    -- ^ RPCv1
             -> Value -> Error
errorInvalid r1 v = if r1
    then Error1 "Invalid request" IdNull
    else Error (ErrorObj "Invalid request" (-32600) v) IdNull

errorParams :: Bool     -- ^ RPCv1
            -> Value -> Id -> Error
errorParams r1 p i = if r1
    then Error1 "Invalid params" i
    else Error (ErrorObj "Invalid params" (-32602) p) i

errorMethod :: Bool     -- ^ RPCv1
            -> Method -> Id -> Error
errorMethod r1 m i = if r1
    then Error1 ("Method not found: " ++ T.unpack m) i
    else Error (ErrorObj "Method not found" (-32601) (toJSON m)) i

errorId :: Bool    -- ^ RPCv1
        -> Id -> Error
errorId r1 i = if r1
  then
    Error1 ("Id not recognized: " ++ L8.unpack (encode (toJSON i))) IdNull
  else
    Error (ErrorObj "Id not recognized" (-32000) (toJSON i)) IdNull

--
-- Messages
--

data Message q n r
    = MsgRequest   { getMsgRequest  :: !(Request  q) }
    | MsgNotif     { getMsgNotif    :: !(Notif    n) }
    | MsgResponse  { getMsgResponse :: !(Response r) }
    | MsgError     { getMsgError    :: !Error        }
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

--
-- Helpers
--

jr2 :: Pair
jr2 = "jsonrpc" .= ("2.0" :: Text)

