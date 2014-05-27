module NLP.Database.Helpers where

import NLP.Types
import qualified Database.Persist as DB
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.MySQL

insertOrKey e = do
  a <- runDB $ DB.insertBy e
  return $ either DB.entityKey id a

runDB f = do
  pool <- asks _dbPool
  runStdoutLoggingT $ runResourceT $ runSqlPool f pool
