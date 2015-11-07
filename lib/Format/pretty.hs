module Format.Pretty where

import Data.Text (Text)

-- | Describes a data type that can be represented as human-readable text.
-- |
-- | This typeclass differs from 'Show' in that it is not meant to have an inverse
-- | instance that can be parsed back into the original data type.
class Pretty a where
    prettify :: a -> Text
