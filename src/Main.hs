{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import Database.SQLite.Simple

import Data.Aeson
import Data.Time.Calendar

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Control.Monad.Trans.Either

type Coord = Double
type Longitude = Coord
type Latitude = Coord

data LogInfo = LogInfo {
	appId :: String,
	message :: String,
	messageType :: String,
	location :: String,
	uuid :: Integer,
	platform :: String,
	model :: String
} deriving (Show, Generic)

instance FromJSON LogInfo

type LogAPI = "log" :> ReqBody '[JSON] LogInfo :> Post '[JSON] LogInfo

doLog :: LogInfo -> EitherT ServantErr IO ()
doLog logInfo = print logInfo

server :: Server LogAPI
server = doLog

logAPI :: Proxy LogAPI
logAPI = Proxy

app :: Application
app = serve logAPI server

main :: IO ()
main = run 8081 app
