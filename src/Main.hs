{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Database.SQLite.Simple
import System.IO
import GHC.Generics

import Data.Aeson
import Data.Time.Calendar

import Network.Wai
import Network.Wai.Middleware.Cors
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

type LogAPI = "log" :> ReqBody '[JSON] LogInfo :> Post '[JSON] ()

main :: IO ()
main = do
    let port = 3000
    putStrLn ("Running logging server on port " ++ show port)
    run port app

-- We're using simpleCors to get around the cross-domain policy
-- Is it safe? Well, um, maybe..
app :: Application
app = simpleCors (serve logAPI server)

logAPI :: Proxy LogAPI
logAPI = Proxy

server :: Server LogAPI
server = doLog

doLog :: LogInfo -> EitherT ServantErr IO ()
doLog logInfo = liftIO (logToFile logInfo)

logToFile :: LogInfo -> IO ()
logToFile logInfo = do
    withFile "log.txt" AppendMode
        (\fileHandle -> do
            let entry = formatLog logInfo
            putStrLn entry
            hPutStrLn fileHandle entry)

formatLog :: LogInfo -> String
formatLog logInfo =
    appName logInfo ++ 
    " | " ++ show (uuid logInfo) ++ 
    " | " ++ location logInfo ++
    " | " ++ platform logInfo ++ " - " ++ model logInfo ++ 
    " | " ++ messageType logInfo ++ ": " ++ message logInfo
