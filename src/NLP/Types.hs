module NLP.Types where

import Control.Monad.Reader
import Database.Persist.MySQL
import Control.Concurrent
import Data.Pool
import Control.Monad.Logger
import Control.Concurrent.Async (mapConcurrently)

data NLPEnv = NLPEnv {
    _dbConfig :: ConnectInfo
    , _dbPool :: Pool Connection
  }

type NLP = LoggingT (ReaderT NLPEnv IO)

-- runNLP :: ReaderT NLPEnv m a -> NLPEnv -> m a
runNLP h = runReaderT $ runStdoutLoggingT h
--runNLP h = runReaderT  h


-- mapConcurrentlyNLP f list = ReaderT $ \env -> mapConcurrently (runNLP f env) list

mapConcurrentlyNLP :: (a -> NLP b) -> [a] -> NLP [b]
mapConcurrentlyNLP f list = do
  env <- ask
  liftIO $ mapConcurrently (\a -> runNLP (f a) env) list

withDBPool cnt f = do
  cfg <- asks _dbConfig
  liftIO $ withMySQLPool cfg cnt f

forkNLP :: NLP () -> NLP ThreadId
forkNLP h = do
  env <- ask
  liftIO $ forkIO $ void $ runNLP h env

