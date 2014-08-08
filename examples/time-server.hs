{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson.Types
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes UTCTime

instance FromRequest TimeReq where
    paramsParser "time" = Just $ const $ return TimeReq 
    paramsParser _ = Nothing

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t

srv :: AppConduits () () TimeRes TimeReq () () IO -> IO ()
srv (src, snk) = src $= CL.mapM respond $$ snk

respond :: IncomingMsg () TimeReq () ()
        -> IO (Message () () TimeRes)
respond (IncomingMsg (MsgRequest (Request ver _ TimeReq i)) Nothing) = do    
    t <- getCurrentTime
    return $ MsgResponse (Response ver (TimeRes t) i)

respond (IncomingError e) = return $ MsgError e
respond (IncomingMsg (MsgError e) _) = return $ MsgError $ e
respond _ = undefined

main :: IO ()
main = tcpServer V2 (serverSettings 31337 "127.0.0.1") srv

