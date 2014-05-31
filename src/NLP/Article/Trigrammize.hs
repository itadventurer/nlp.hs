{-# LANGUAGE OverloadedStrings #-}
module NLP.Article.Trigrammize where

import qualified Data.Text as T
import NLP.LanguageModels.NGramm
import NLP.Types
import NLP.Types.Monad
import qualified Data.Map as M
import NLP.Index.Index

handleAllArticles :: NLP ()
handleAllArticles = do
  articles <- getAll
  _ <- mapM handleArticle articles
  return ()

handleArticle :: Article -> NLP ()
handleArticle article@(Article {articleText = txt}) = do
  let txt' = T.words txt
      trigramms = getTrigramms txt'
      trigrammList = M.toList trigramms
  _ <- forkNLP $ mapM_ handleWord $ M.toList $ countOccurrence txt'
  _ <- forkNLP $ mapM_ handleTrigramm trigrammList
  return ()
  where
    handleTrigramm (trigramm,count) = insertOrKey $ ArticleTrigramm article trigramm count
    handleWord (word,count) = insertOrKey $ ArticleWord article (Word word) count
