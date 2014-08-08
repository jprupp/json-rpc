{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson.Types hiding (Error)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Network.JsonRpc
import System.Locale

data TimeReq = TimeReq
data TimeRes = TimeRes UTCTime

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

cli :: AppConduits TimeReq () () () () TimeRes IO
    -> IO String
cli (src, snk) = do
    CL.sourceList [MsgRequest $ buildRequest TimeReq] $$ snk
    ts <- src $$ CL.consume
    case ts of
        [] ->
            error "No response received"
        [IncomingError (Error (ErrorObj m _ _) _)] ->
            error $ "Client error: " ++ m
        [IncomingMsg (MsgError (Error (ErrorObj m _ _) _)) _] ->
            error $ "Server error: " ++ m
        [IncomingMsg (MsgResponse (Response (TimeRes t) _)) _] ->
            return $ formatTime defaultTimeLocale "%c" t
        _ -> undefined

main :: IO ()
main = do
    t <- tcpClient False True (clientSettings 31337 "127.0.0.1") cli
    putStrLn t
