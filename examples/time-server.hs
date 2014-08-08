{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson.Types hiding (Error)
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

-- JSON-RPC 2.0
respond :: IncomingMsg () TimeReq () ()
        -> IO (Message () () TimeRes)
respond (IncomingMsg (MsgRequest (Request _ TimeReq i)) Nothing) = do    
    t <- getCurrentTime
    return $ MsgResponse (Response (TimeRes t) i)

-- JSON-RPC 1.0
respond (IncomingMsg (MsgRequest (Request1 _ TimeReq i)) Nothing) = do    
    t <- getCurrentTime
    return $ MsgResponse (Response1 (TimeRes t) i)

respond (IncomingError e) = return $ MsgError e
respond _ = undefined

main :: IO ()
main = tcpServer False (serverSettings 31337 "127.0.0.1") srv

