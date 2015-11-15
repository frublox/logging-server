{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Spoon

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cors as Wai (simpleCors)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import Servant

import Data.LogInfo
import Format.Pretty
import Utils.Text (showText)

-- FormUrlEncoded is used instead of JSON for cross-domain requests
type LogAPI = 
    "log" :> ReqBody '[FormUrlEncoded, JSON] LogInfo 
          :> Post '[FormUrlEncoded, JSON] ()

defaultPort = 3000

main :: IO ()
main = do
    args <- getArgs
    port <- getPort args

    Text.putStrLn $ Text.concat ["Running on port ", showText port]

    Warp.run port app

getPort :: [String] -> IO Warp.Port
getPort args = 
    case (teaspoon . read . head) args of
        Nothing -> do
            Text.putStrLn "No port specified. Using default port."
            return defaultPort
        Just port -> return port

app :: Wai.Application
app = Wai.simpleCors (serve logAPI server)
    where
        logAPI = Proxy :: Proxy LogAPI
        server = doLog

doLog :: LogInfo -> EitherT ServantErr IO ()
doLog logInfo = liftIO $ do
    Text.putStrLn (prettify logInfo)
    logToFile logInfo

logToFile :: LogInfo -> IO ()
logToFile logInfo = do
    let entry = prettify logInfo
    Text.writeFile "log.txt" entry
