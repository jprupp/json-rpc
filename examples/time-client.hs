{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Concurrent.STM
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
    parseResult "time" = withText "time" $ \t -> case f t of
        Nothing -> fail "Could not parse time"
        Just t' -> return $ TimeRes t'
      where
        f t = parseTime defaultTimeLocale "%c" (T.unpack t)
    parseResult m = fail $ "Method not found: " ++ T.unpack m

req :: JsonRpcT TimeReq TimeRes () () () () IO UTCTime
req = sendRequest TimeReq >>= liftIO . atomically >>= \tsE -> case tsE of
    Left  e -> error $ getErrMsg e
    Right r -> liftIO (threadDelay 1000000) >> return (timeRes r)

main :: IO ()
main = jsonRpcTcpClient V2 (clientSettings 31337 "::1") undefined $
    replicateM_ 4 $ req >>= liftIO . print
