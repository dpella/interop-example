{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module:      DPella.SQLite
-- Copyright:   (c) DPella AB 2023
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <agustin@dpella.io>, <lobo@dpella.io>
module DPella.SQLite (
  SQLiteT,
  runSQLiteT,
  SQLite,
  runSQLite,
  query,
  queryNamed,
  query_,
  queryWith,
  queryWith_,
  execute,
  executeNamed,
  execute_,
  withTransaction,
  changes,
  lastInsertRowId,
  handleSQLiteError,
  SQLite.SQLError (..),
  SQLite.Error (..),
  SQLite.Query (..),
  SQLite.Only (..),
  SQLite.NamedParam (..),
  SQLite.FromRow (..),
  SQLite.ToRow (..),
  SQLite.FromField (..),
  SQLite.ToField (..),
  SQLite.SQLData (..),
  SQLite.FieldParser,
  SQLite.RowParser,
  SQLite.field,
  SQLite.fieldWith,
  SQLite.sql,
) where

import Control.Monad (when, forM_)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, mask, onException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.Text qualified as Text
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField qualified as SQLite
import Database.SQLite.Simple.FromRow qualified as SQLite
import Database.SQLite.Simple.QQ qualified as SQLite
import Database.SQLite.Simple.ToField qualified as SQLite

import Control.Exception (throwIO)
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQLite
import Database.SQLite.Simple.Function qualified as SQLite

import DPella.RawVal
import DPella.Noise


----------------------------------------

-- * A simple monadic interface to run queries inside SQLite databases

-- ** SQLite Monad

----------------------------------------

-- | The SLiteT monad
newtype SQLiteT m a = SQLiteT (ReaderT SQLite.Connection m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadTrans)

-- | Run an `SQLiteT` computation given a path for the database
runSQLiteT :: (MonadIO m) => FilePath -> SQLiteT m a -> m a
runSQLiteT db (SQLiteT sqlite) = do
  withCustomSQLite db $ \conn -> do
    runReaderT sqlite conn

-- | The non-transformer version
type SQLite = SQLiteT IO

-- | Run an `SQLite` computation given a path for the database
runSQLite :: FilePath -> SQLite a -> IO a
runSQLite = runSQLiteT

-- | Get the internal `SQLite` connection handle
getConnection :: (Monad m) => SQLiteT m SQLite.Connection
getConnection = SQLiteT ask

-- | Running inside a custom SQL environment with:
-- * Foreign-key constraints enabled
-- * Application-defined functions as per `customSQLFunctions`
withCustomSQLite :: (MonadIO m) => FilePath -> (SQLite.Connection -> m a) -> m a
withCustomSQLite db ma = do
  -- Open the connection
  conn <- liftIO $ SQLite.open db
  -- Enable foreign keys
  liftIO $ SQLite.execute_ conn "PRAGMA foreign_keys = ON"
  -- Run the computation in the extended function environment
  a <- withSQLFunctions customSQLNoiseFunctions conn ma
  -- Close the connection
  liftIO $ SQLite.close conn
  -- Return the result
  return a

---------------------------------------

-- ** SQL commands

-- | Run a @SELECT@ query
query :: (SQLite.FromRow res, SQLite.ToRow params, MonadIO m) => SQLite.Query -> params -> SQLiteT m [res]
query q p = do
  conn <- getConnection
  liftIO (SQLite.query conn q p)

-- | Run a @SELECT@ query passing named parameters
queryNamed :: (SQLite.FromRow res, MonadIO m) => SQLite.Query -> [SQLite.NamedParam] -> SQLiteT m [res]
queryNamed q p = do
  conn <- getConnection
  liftIO (SQLite.queryNamed conn q p)

-- | Same as `query` but used when we don't pass any arguments
query_ :: (SQLite.FromRow res, MonadIO m) => SQLite.Query -> SQLiteT m [res]
query_ q = do
  conn <- getConnection
  liftIO (SQLite.query_ conn q)

-- | Same as `query` but passing a specific row parser
queryWith :: (SQLite.ToRow params, MonadIO m) => SQLite.RowParser res -> SQLite.Query -> params -> SQLiteT m [res]
queryWith parser q p = do
  conn <- getConnection
  liftIO (SQLite.queryWith parser conn q p)

-- | Same as `queryWith` but used when we don't pass any arguments
queryWith_ :: (MonadIO m) => SQLite.RowParser res -> SQLite.Query -> SQLiteT m [res]
queryWith_ parser q = do
  conn <- getConnection
  liftIO (SQLite.queryWith_ parser conn q)

-- | Execute an @UPDATE@, @INSERT@ or @CREATE@ command
execute :: (SQLite.ToRow res, MonadIO m) => SQLite.Query -> res -> SQLiteT m Int
execute q r = do
  conn <- getConnection
  liftIO (SQLite.execute conn q r >> SQLite.changes conn)

-- | Execute an @UPDATE@, @INSERT@ or @CREATE@ command passing named parameters
executeNamed :: (MonadIO m) => SQLite.Query -> [SQLite.NamedParam] -> SQLiteT m ()
executeNamed q p = do
  conn <- getConnection
  liftIO (SQLite.executeNamed conn q p)

-- | Same as `execute` but used when we don't pass any argument
execute_ :: (MonadIO m) => SQLite.Query -> SQLiteT m Int
execute_ q = do
  conn <- getConnection
  liftIO (SQLite.execute_ conn q >> SQLite.changes conn)

-- | Execute a command inside an exclusive transaction (no other writes or reads allowed)
withTransaction :: (MonadIO m, MonadMask m) => SQLiteT m a -> SQLiteT m a
withTransaction ma = do
  conn <- getConnection
  let begin = liftIO $ SQLite.execute_ conn "BEGIN EXCLUSIVE TRANSACTION"
  let commit = liftIO $ SQLite.execute_ conn "COMMIT TRANSACTION"
  let rollback = liftIO $ SQLite.execute_ conn "ROLLBACK TRANSACTION"
  mask $ \restore -> do
    begin
    a <- restore ma `onException` rollback
    commit
    return a

-- | Get the last inserted row id
lastInsertRowId :: (MonadIO m) => SQLiteT m Int
lastInsertRowId = do
  conn <- getConnection
  rowid <- liftIO (SQLite.lastInsertRowId conn)
  return (fromIntegral rowid)

-- | Get the number of affected rows after an @INSERT@, @UPDATE@ or @DELETE@
changes :: (MonadIO m) => SQLiteT m Int
changes = do
  conn <- getConnection
  liftIO (SQLite.changes conn)

-- | Handle an exception raised internally by SQLite
handleSQLiteError :: (MonadIO m, MonadCatch m) => SQLiteT m a -> (SQLite.Error -> SQLiteT m a) -> SQLiteT m a
handleSQLiteError sqlite handle = do
  try sqlite >>= \case
    Left e -> handle (SQLite.sqlError e)
    Right a -> return a

-- | Parse SQLite data into an attribute value
fromSQLiteField :: RawType -> SQLite.FieldParser RawVal
fromSQLiteField r_type f = do
  res <- SQLite.fromField @SQLData f
  case (r_type, res) of
    (RBoolTy, SQLInteger n)-> do
      when (n /= 0 && n /= 1) $ do
        fail ("Invalid boolean value " <> show n)
      return (RawBool (n /= 0))
    (RIntTy, SQLInteger n) -> do
      return (RawInt (fromIntegral n))
    (RDoubleTy, SQLFloat d) -> do
      return (RawDouble d)
    (RTextTy, SQLText t) -> do
      return (RawText t)
    (r_ty, sql_data) -> do
      fail ("Unable to marshall " <> show r_ty <> " with " <> show sql_data)

instance SQLite.ToField RawVal where
  toField (RawBool b) = SQLite.toField (SQLite.SQLInteger (if b then 1 else 0))
  toField (RawDouble d) = SQLite.toField (SQLite.SQLFloat d)
  toField (RawInt i) = SQLite.toField (SQLite.SQLInteger (fromIntegral i))
  toField (RawText t) = SQLite.toField (SQLite.SQLText t)



-- | List all available noise functions
customSQLNoiseFunctions :: [SQLFunction]
customSQLNoiseFunctions = [ sqlDpellaSampleRandom ]

-- | Create an `SQLFunction` sampling from a Gaussian distribution, satisfying approximate
-- differential privacy
sqlDpellaSampleRandom :: SQLFunction
sqlDpellaSampleRandom =
  SQLFunction "dpella_sample_random" $ dpellaSampleRandom . sqlite_env_rng


-- | SQL environment
newtype SQLEnv = SQLEnv
  { sqlite_env_rng :: NoiseGen
  }

-- | Initialize an SQL environment
initSQLiteEnv :: IO SQLEnv
initSQLiteEnv = do
  rng <- newNoiseGen
  return
    SQLEnv
      { sqlite_env_rng = rng
      }

----------------------------------------

-- ** Custom SQL functions parameterized by the internal environment

-- | The function na,e
type FunctionName = Text

-- | The function implementation
type FunctionImpl f = SQLEnv -> f

-- | An SQL function
data SQLFunction where
  SQLFunction :: (SQLite.Function f) => FunctionName -> FunctionImpl f -> SQLFunction

-- | Retrieve the name of an `SQLFunction`
getSQLFunctionName :: SQLFunction -> FunctionName
getSQLFunctionName (SQLFunction name _) = name

-- | Run a computation with some custom functions in scope
withSQLFunctions :: (MonadIO m) => [SQLFunction] -> SQLite.Connection -> (SQLite.Connection -> m a) -> m a
withSQLFunctions funs conn ma = do
  env <- liftIO initSQLiteEnv
  -- Register the custom functions
  liftIO $ forM_ funs $ \(SQLFunction name impl) -> do
    res <- SQLite.createFunction conn name (impl env)
    case res of
      Left err -> throwIO (userError (show err))
      Right () -> return ()
  -- Run the computation that uses custom SQL functions
  a <- ma conn
  -- Unregister the custom functions
  liftIO $ forM_ funs $ \(SQLFunction name _) -> do
    SQLite.deleteFunction conn name
  -- Return the result
  return a


