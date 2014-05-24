module NLP.Types where

import Control.Monad.Reader
import Database.Persist.MySQL
import Control.Concurrent
import Data.Pool
import Control.Monad.Trans.Resource
import Control.Monad.Logger

data NLPEnv = NLPEnv {
    _dbConfig :: ConnectInfo
    , _dbPool :: Pool Connection
  }

type NLP = LoggingT (ReaderT NLPEnv IO)

runNLP h = runReaderT $ runStdoutLoggingT h

withDBPool cnt f = do
  cfg <- asks _dbConfig
  liftIO $ withMySQLPool cfg cnt f

runDB f = do
  pool <- asks _dbPool
  runStdoutLoggingT $ runResourceT $ runSqlPool f pool

forkNLP :: NLP () -> NLP ThreadId
forkNLP h = do
  env <- ask
  liftIO $ forkIO $ void $ runNLP h env

