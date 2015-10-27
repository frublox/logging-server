{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Network.Wai
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Servant

import Data.LogInfo
import Format.Pretty
import Utils.Text (showText)

-- FormUrlEncoded is allowed over JSON for cross-domain requests
type LogAPI = 
    "log" :> ReqBody '[FormUrlEncoded, JSON] LogInfo 
          :> Post '[FormUrlEncoded, JSON] ()

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
