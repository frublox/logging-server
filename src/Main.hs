{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Directory

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import Data.Time.Format

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
type LogAPI 
    = "log" 
        :> ReqBody '[FormUrlEncoded, JSON] LogInfo 
        :> Post '[FormUrlEncoded, JSON] ()

defaultPort :: Warp.Port
defaultPort = 3000

defaultLogDir :: FilePath
defaultLogDir = "logs/"

main :: IO ()
main = do
    port <- getPort

    Text.putStrLn $ Text.concat ["Running on port ", showText port]

    Warp.run port app

getPort :: IO Warp.Port
getPort = do
    args <- getArgs
    case teaspoon $ read (args !! 0) of
        Nothing -> do
            Text.putStrLn "No port specified. Using default port."
            return defaultPort
        Just port -> return port

getLogDir :: IO FilePath
getLogDir = do
    args <- getArgs
    case teaspoon (args !! 1) of
        Nothing -> do
            let msg = concat ["No logging directory specified. Using default: '", defaultLogDir]
            Text.putStrLn (Text.pack msg)
            return defaultLogDir
        Just dir -> return dir

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
    dir <- getLogDir
    createDirectoryIfMissing True dir

    today <- localDay `fmap` (zonedTimeToLocalTime `fmap` getZonedTime)
    let timestamp = show today

    let filepath = concat [dir, "log_", timestamp, ".txt"]
    let entry = prettify logInfo

    Text.writeFile filepath entry
