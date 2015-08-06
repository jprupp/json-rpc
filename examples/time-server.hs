{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import Data.Time.Clock
import Data.Time.Format
import Network.JSONRPC
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance FromRequest TimeReq where
    parseParams "time" = Just $ const $ return TimeReq 
    parseParams _ = Nothing

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t

respond :: (Functor m, MonadLoggerIO m) => Respond TimeReq m TimeRes
respond TimeReq = Right . TimeRes <$> liftIO getCurrentTime

main :: IO ()
main = runStderrLoggingT $
    jsonRPCTCPServer V2 (serverSettings 31337 "::1") respond dummySrv
