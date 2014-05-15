{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenizer.SimpleTokenizer where

import qualified Data.Text as T
import Data.Text (Text)
import Tokenizer

tokenize :: Tokenizer Text
tokenize = T.words
