module NLP.Helpers where

import qualified Data.Text as T
import Data.Text (Text)

toText :: String -> Text
toText = T.strip . T.pack
