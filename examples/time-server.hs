{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Aeson.Types
import Data.Conduit.Network
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance FromRequest TimeReq where
    paramsParser "time" = Just $ const $ return TimeReq 
    paramsParser _ = Nothing

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t

respond :: TimeReq -> IO (Either ErrorObj TimeRes)
respond = const $ Right . TimeRes <$> getCurrentTime

main :: IO ()
main = jsonRpcTcpServer V2 (serverSettings 31337 "::1") respond
    (dummySrv :: JsonRpcT () () () TimeReq TimeRes () IO ())

