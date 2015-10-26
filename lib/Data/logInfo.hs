{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.LogInfo (
    LogInfo
) where

import Data.Text (Text)
import qualified Data.Text as Text 
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Monoid ((<>))

import Control.Spoon

import Format.Pretty
import Utils.Text (showText, readText)

import Servant

data MessageType = Info | Error 
    deriving (Show, Read)

data Message = Message {
    messageText :: Text,
    messageType :: MessageType
}

instance Pretty Message where
    prettify (Message msgText msgType) =
        showText msgType <> ": " <> msgText 

data Device = Device {
    uuid :: Integer,
    platform :: Text,
    model :: Text
}

type Latitude = Double
type Longitude = Double

data Location = Coords Latitude Longitude | UnknownLocation

instance Pretty Location where
    prettify (Coords lat long) = 
        "[" <> showText lat <> ", " <> showText long <> "]"
    prettify UnknownLocation = "Unknown Location"

data LogInfo = LogInfo {
    appName :: Text,
    message :: Message,
    location :: Location,
    device :: Device
}

instance Pretty LogInfo where
    prettify (LogInfo name msg loc dev) =
        name <> 
        " | " <> showText (uuid dev) <>
        " | " <> prettify loc <>
        " | " <> platform dev <> " - " <> model dev <>
        " | " <> prettify msg

extract :: Text -> [(Text, Text)] -> Either String Text
extract label inputs = 
    case lookup label inputs of
        Nothing -> Left ("Label " ++ Text.unpack label ++ " was not found.")
        Just value -> Right value

readExtract :: Read a => Text -> [(Text, Text)] -> Either String a
readExtract label inputs = do
    value <- extract label inputs
    maybe (Left ("Couldn't parse " ++ Text.unpack label ++ "."))
          Right (readText value)

instance FromFormUrlEncoded LogInfo where
    fromFormUrlEncoded inputs = do
        name <- extract "appName" inputs

        message <- do
            msgText <- extract "message" inputs
            msgType <- readExtract "messageType" inputs

            return (Message msgText msgType)

        location <- do
            case readExtract "location" inputs of
                Left _ -> return UnknownLocation
                Right [latitude, longitude] -> return (Coords latitude longitude)

        device <- do
            uuid <- readExtract "uuid" inputs
            platform <- extract "platform" inputs
            model <- extract "model" inputs

            return (Device uuid platform model)

        return (LogInfo name message location device)
