module Acronym (abbreviate) where

import Data.Text (Text)
import qualified Data.Text as T

abbreviate :: Text -> Text
abbreviate xs = T.pack $ map T.head $ T.words xs
