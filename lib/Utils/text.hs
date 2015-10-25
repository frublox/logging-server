module Utils.Text where

import Data.Text (Text)
import qualified Data.Text as Text 
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

import Control.Spoon

showText :: Show a => a -> Text
showText = Text.pack . show

readText :: Read a => Text -> Maybe a
readText = teaspoon . read . Text.unpack
