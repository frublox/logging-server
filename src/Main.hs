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
import Servant

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as Text 
import qualified Data.Text.IO as Text

import Data.Monoid ((<>))

import Data.LogInfo
import Format.Pretty
import Utils.Text (showText)

-- FormUrlEncoded is used over JSON due to the restrictions on
-- cross-domain requests
type LogAPI = 
    "log" :> ReqBody '[FormUrlEncoded] LogInfo 
          :> Post '[FormUrlEncoded] ()

main :: IO ()
main = do
    let port = 3000
    Text.putStrLn ("Running on port " <> showText port)
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
doLog logInfo = liftIO $ do
    Text.putStrLn (prettify logInfo)
    logToFile logInfo

logToFile :: LogInfo -> IO ()
logToFile logInfo = do
    let entry = prettify logInfo
    Text.writeFile "log.txt" entry
