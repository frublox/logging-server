{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import Database.SQLite.Simple
import System.IO
import GHC.Generics

import Data.Aeson
import Data.Time.Calendar

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

data LogInfo = LogInfo {
    appName :: String,
    message :: String,
    messageType :: String,
    location :: String,
    uuid :: Integer,
    platform :: String,
    model :: String
} deriving (Show, Generic)

instance FromJSON LogInfo
instance ToJSON LogInfo

type LogAPI = 
    "log" :> ReqBody '[JSON] LogInfo :> Post '[JSON] ()

formatLog :: LogInfo -> String
formatLog logInfo =
    appName logInfo ++ 
    " | " ++ show (uuid logInfo) ++ 
    " | " ++ location logInfo ++
    " | " ++ platform logInfo ++ " - " ++ model logInfo ++ 
    " | " ++ messageType logInfo ++ ": " ++ message logInfo

logToFile :: LogInfo -> IO ()
logToFile logInfo = do
    withFile "log.txt" AppendMode
        (\fileHandle -> do
            let entry = formatLog logInfo
            hPutStrLn fileHandle entry)

doLog :: LogInfo -> EitherT ServantErr IO ()
doLog logInfo = do
    liftIO $ logToFile logInfo

server :: Server LogAPI
server = doLog

logAPI :: Proxy LogAPI
logAPI = Proxy

app :: Application
app = serve logAPI server

main :: IO ()
main = run 8081 app
