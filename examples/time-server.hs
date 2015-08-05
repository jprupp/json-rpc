{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance FromRequest TimeReq where
    parseParams "time" = Just $ const $ return TimeReq 
    parseParams _ = Nothing

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t

respond :: Respond TimeReq IO TimeRes
respond TimeReq = Right . TimeRes <$> getCurrentTime

main :: IO ()
main = jsonRpcTcpServer V2 (serverSettings 31337 "::1") respond dummySrv

