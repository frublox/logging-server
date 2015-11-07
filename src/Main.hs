{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cors as Wai (simpleCors)
import qualified Network.Wai.Handler.Warp as Warp

import Servant

import Data.LogInfo
import Format.Pretty
import Utils.Text (showText)

-- FormUrlEncoded is used instead of JSON for cross-domain requests
type LogAPI = 
    "log" :> ReqBody '[FormUrlEncoded, JSON] LogInfo 
          :> Post '[FormUrlEncoded, JSON] ()

main :: IO ()
main = do
    port <- read `fmap` (head `fmap` getArgs)
    Text.putStrLn ("Running on port " <> showText port)
    Warp.run port app

app :: Wai.Application
app = Wai.simpleCors (serve logAPI server)

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
