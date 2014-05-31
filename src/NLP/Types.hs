{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Types where

import Database.Persist.MySQL (Key,SqlBackend,Unique)
import Data.Text (Text)
import NLP.Database.Article
import NLP.Types.Monad
import qualified Database.Persist as DB
import Data.Maybe
import Data.Time.Clock

data Entity a = Entity { entityKey :: Key (DBEntity a)
                       , entityVal :: a
                       }

class (DB.PersistEntityBackend (DBEntity a) ~ SqlBackend, (DB.PersistEntity (DBEntity a))) => DBType a where
  type family DBEntity a
  insertUnique :: a -> NLP (Maybe (Key (DBEntity a)))
  insertUnique synonym = do
    s <- toDBType synonym
    runDB $ DB.insertUnique s
    
  insertOrKey :: a -> NLP (Key (DBEntity a))
  insertOrKey s = do
    s' <- toDBType s
    a <- runDB $ DB.insertBy s'
    return $ either DB.entityKey id a

  fromDBType :: DBEntity a -> NLP a
  toDBType :: a -> NLP (DBEntity a)
  get :: Key (DBEntity a) -> NLP (Maybe a)
  get key = do
    w <- runDB $ DB.get key
    maybe (return Nothing) (fmap Just . fromDBType) w
  getBy :: Unique (DBEntity a) -> NLP (Maybe a)
  getBy unique = do
    v <- runDB $ DB.getBy unique
    maybe (return Nothing) (fmap Just . fromDBType . DB.entityVal) v

  getAll :: NLP [a]
  getAll = do
    v <- runDB $ DB.selectList [] []
    mapM entityToVal v
    where
      entityToVal e = do
        let e' = DB.entityVal e
        fromDBType e'

-- Word
newtype Word = Word Text
               deriving (Eq, Show, Ord)
  
instance DBType Word where
  type DBEntity Word = DBWord
  fromDBType (DBWord w) = return $ Word w
  toDBType (Word w) = return $ DBWord w

-- Synonym
data Synonym = Synonym Word Word

instance Eq Synonym where  
  (Synonym w11 w12) == (Synonym w21 w22) = (w11 == w21 && w12 == w22) ||
                                           (w11 == w22 && w12 == w21)
instance DBType Synonym where
  type DBEntity Synonym = DBSynonym

  fromDBType (DBSynonym k1 k2) = do
    w1 <- get k1
    w2 <- get k2
    return $ Synonym (fromJust w1) (fromJust w2)
  toDBType (Synonym w1 w2) = do
    k1 <- insertOrKey w1
    k2 <- insertOrKey w2
    return $ DBSynonym k1 k2

-- Trigramm

data Trigramm = Trigramm Word Word Word
                deriving (Eq, Show, Ord)

instance DBType Trigramm where
  type DBEntity Trigramm = DBTrigramm

  fromDBType (DBTrigramm k1 k2 k3) = do
    w1 <- get k1
    w2 <- get k2
    w3 <- get k3
    return $ Trigramm (fromJust w1) (fromJust w2) (fromJust w3)
  toDBType (Trigramm w1 w2 w3) = do
    k1 <- insertOrKey w1
    k2 <- insertOrKey w2
    k3 <- insertOrKey w3
    return $ DBTrigramm k1 k2 k3

-- Author
data Author = Author { authorName :: Text
                     , authorEmail :: Text
                     }
              deriving (Eq, Show, Ord)
                       
instance DBType Author where
  type DBEntity Author = DBAuthor

  fromDBType (DBAuthor name email) = return $ Author name email
  toDBType (Author name email) = return $ DBAuthor name email

fromAuthorName :: Text -> Author
fromAuthorName name = Author name ""

-- Article
data Article = Article { articleUrl :: Text
                       , articleTitle :: Text
                       , articleAuthor :: Maybe Author
                       , articleDate :: Maybe UTCTime
                       , articleText :: Text
                       }
               deriving (Eq, Show, Ord)

instance DBType Article where
  type DBEntity Article = DBArticle

  fromDBType (DBArticle url title authorId date text) = do
    author <- maybe (return Nothing) (fmap Just .get) authorId
    return $ Article url title (maybe Nothing id author) date text
  toDBType (Article url title author date text) = do
    authorId <- maybe (return Nothing) (fmap Just . insertOrKey) author
    return $ DBArticle url title authorId date text

existURL :: Text -> NLP Bool
existURL url = do
  cnt <- runDB $ DB.count [DBArticleUrl DB.==. url]
  return $ cnt > 0

data ArticleTrigramm = ArticleTrigramm Article Trigramm Int

instance DBType ArticleTrigramm where
  type DBEntity ArticleTrigramm = DBArticleTrigramm

  fromDBType (DBArticleTrigramm articleKey trigrammKey count) = do
    article <- get articleKey
    trigramm <- get trigrammKey
    return $ ArticleTrigramm (fromJust article) (fromJust trigramm) count
  toDBType (ArticleTrigramm article trigramm count) = do
    articleId <- insertOrKey article
    trigrammId <- insertOrKey trigramm
    return $ DBArticleTrigramm articleId trigrammId count

data ArticleWord = ArticleWord Article Word Int

instance DBType ArticleWord where
  type DBEntity ArticleWord = DBArticleWord

  fromDBType (DBArticleWord articleKey wordKey count) = do
    article <- get articleKey
    word <- get wordKey
    return $ ArticleWord (fromJust article) (fromJust word) count
  toDBType (ArticleWord article word count) = do
    articleId <- insertOrKey article
    wordId <- insertOrKey word
    return $ DBArticleWord articleId wordId count
  
