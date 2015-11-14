{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.LogInfo (
    LogInfo
) where

import GHC.Generics

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text 
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Monoid ((<>))
import Data.Bifunctor (first)

import Control.Spoon

import Format.Pretty
import Utils.Text (showText, readText)

import Servant

data MessageType 
    = Info 
    | Error
    deriving (Show, Read, Generic)

instance FromJSON MessageType

data Message = Message 
    { messageText :: Text
    , messageType :: MessageType
    } deriving (Generic, Show)

instance FromJSON Message

instance Pretty Message where
    prettify (Message msgText msgType) = Text.concat [showText msgType, ": ", msgText]

data Device = Device
    { uuid :: Integer
    , platform :: Text
    , model :: Text
    } deriving (Generic, Show)

instance FromJSON Device

type Latitude = Double
type Longitude = Double

data Location 
    = Coords Latitude Longitude 
    | UnknownLocation
    deriving (Show)

instance FromJSON Location where
    parseJSON (Object v) = parseLocation `fmap` (v .: "location")

instance Pretty Location where
    prettify (Coords lat long) = Text.concat ["[", showText lat, ", ", showText long, "]"]
    prettify UnknownLocation = "Unknown Location"

-- | Parse text containing coordinates in list format: "[latitude, longitude]"
parseLocation :: Text -> Location
parseLocation text = 
    case readText text of
        Nothing -> UnknownLocation
        Just [lat, long] -> Coords lat long

data LogInfo = LogInfo 
    { appName :: Text
    , message :: Message
    , location :: Location
    , device :: Device
    } deriving (Generic, Show)

instance FromJSON LogInfo

instance Pretty LogInfo where
    prettify (LogInfo name msg loc dev) = 
        Text.concat [
            name,
            " | ", showText (uuid dev),
            " | ", prettify loc,
            " | ", platform dev, " - ", model dev,
            " | ", prettify msg
        ]

instance FromFormUrlEncoded LogInfo where
    fromFormUrlEncoded inputs = 
        -- fromFormUrlEncoded's return type should be Either String LogInfo, but
        -- logInfo is of type Either Text LogInfo.
        -- 
        -- We can fix this by using Bifunctor's `first` function to modify the left side.
        Text.unpack `first` logInfo

        -- Defining external functions is for losers, right?
        where
            logInfo = do
                name <- extract "appName"

                message <- do
                    msgText <- extract "message"
                    msgType <- readExtract "messageType"

                    return (Message msgText msgType)

                location <- parseLocation `fmap` (extract "location")

                device <- do
                    uuid <- readExtract "uuid"
                    platform <- extract "platform"
                    model <- extract "model"

                    return (Device uuid platform model)

                return (LogInfo name message location device)

                where
                    -- Essentially `lookup` wrapped in Either
                    extract :: Text -> Either Text Text
                    extract label = 
                        case lookup label inputs of
                            Nothing -> Left $ Text.concat ["Couldn't find label ", label, "."]
                            Just value -> Right value

                    -- Essentially `readText` applied to `extract label`, wrapped in Either
                    readExtract :: Read a => Text -> Either Text a
                    readExtract label = do
                        x <- extract label
                        case readText x of
                            Nothing -> Left $ Text.concat ["Couldn't parse label ", label, "."]
                            Just value -> Right value

