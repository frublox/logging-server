module Format.Pretty where

import Data.String (IsString)
import Data.Text (Text)

class Pretty a where
    prettify :: a -> Text
