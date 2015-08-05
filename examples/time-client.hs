{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types hiding (Error)
import Data.Conduit.Network
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes { timeRes :: UTCTime }

instance ToRequest TimeReq where
    requestMethod TimeReq = "time"

instance ToJSON TimeReq where
    toJSON TimeReq = emptyArray

instance FromResponse TimeRes where
    parseResult "time" = Just $ withText "time" $ \t -> case f t of
        Just t' -> return $ TimeRes t'
        Nothing -> mzero
      where
        f t = parseTime defaultTimeLocale "%c" (T.unpack t)
    parseResult _ = Nothing

req :: JsonRpcT IO UTCTime
req = sendRequest TimeReq >>= \ts -> case ts of
    Left e -> error $ fromError e
    Right (Just (TimeRes r)) -> return r
    _ -> error "Could not parse response"

main :: IO ()
main = jsonRpcTcpClient V2 (clientSettings 31337 "::1") dummyRespond .
    replicateM_ 4 $ req >>= liftIO . print >> liftIO (threadDelay 1000000)
