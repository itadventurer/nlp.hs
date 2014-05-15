module NLP.Tokenizer where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Data.Char

tokenize :: Text -> [Text]
tokenize = T.words . T.filter ((||) <$> isLetter <*> isSpace) . T.toLower
