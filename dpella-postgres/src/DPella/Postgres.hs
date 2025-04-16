{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module:      DPella.Postgres
-- Copyright:   (c) DPella AB 2023
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <matti@dpella.io>, <lobo@dpella.io>
module DPella.Postgres (
  PostgresT,
  runPostgresT,
  Postgres,
  runPostgres,
  query,
  query_,
  queryWith,
  queryWith_,
  execute,
  execute_,
  withTransaction,
  handlePostgresError,
  Postgres.SqlError (..),
  Postgres.Query,
  Postgres.Only (..),
  Postgres.conversionError,
  Postgres.Conversion,
  Postgres.FromRow (..),
  Postgres.ToRow,
  Postgres.FromField (..),
  Postgres.ToField (..),
  Postgres.FieldParser,
  Postgres.RowParser,
  Postgres.field,
  Postgres.fieldWith,
  Postgres.sql,
  Postgres.toJSONField,
  Postgres.Null (..),
) where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, mask, onException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans)


import Data.ByteString.Char8 qualified as BS
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromField qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres
import Database.PostgreSQL.Simple.SqlQQ qualified as Postgres
import Database.PostgreSQL.Simple.ToField qualified as Postgres
import Database.PostgreSQL.Simple.Types qualified as Postgres

import DPella.RawVal

----------------------------------------

-- * A simple monadic interface to run queries inside SQLite databases

-- ** SQLite Monad

----------------------------------------

-- | The SLiteT monad
newtype PostgresT m a = PostgresT (ReaderT Postgres.Connection m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadTrans)

-- | Run an `PostgresT` computation given a connection string
runPostgresT :: (MonadIO m) => BS.ByteString -> PostgresT m a -> m a
runPostgresT db (PostgresT act) = do
  withCustomPostgres db $ \conn -> do
    runReaderT act conn

-- | The non-transformer version
type Postgres = PostgresT IO

-- | Run an `Postgres` computation given a path for the database
runPostgres :: BS.ByteString -> Postgres a -> IO a
runPostgres = runPostgresT

-- | Get the internal `SQLite` connection handle
getConnection :: (Monad m) => PostgresT m Postgres.Connection
getConnection = PostgresT ask

-- | Running inside a custom SQL environment with:
-- * Foreign-key constraints enabled
-- * Application-defined functions as per `customSQLFunctions`
withCustomPostgres :: (MonadIO m) => BS.ByteString -> (Postgres.Connection -> m a) -> m a
withCustomPostgres db ma = do
  -- Open the connection
  conn <- liftIO $ Postgres.connectPostgreSQL db
  -- Run the computation in the extended function environment
  a <- ma conn
  -- Close the connection
  liftIO $ Postgres.close conn
  -- Return the result
  return a

-- ---------------------------------------

-- -- ** SQL commands

-- -- | Run a @SELECT@ query
query :: (Postgres.FromRow res, Postgres.ToRow params, MonadIO m) => Postgres.Query -> params -> PostgresT m [res]
query q p = do
  conn <- getConnection
  liftIO (Postgres.query conn q p)

-- -- | Run a @SELECT@ query passing named parameters
-- queryNamed :: (SQLite.FromRow res, MonadIO m) => SQLite.Query -> [SQLite.NamedParam] -> SQLiteT m [res]
-- queryNamed q p = do
--   conn <- getConnection
--   liftIO (SQLite.queryNamed conn q p)

-- -- | Same as `query` but used when we don't pass any arguments
query_ :: (Postgres.FromRow res, MonadIO m) => Postgres.Query -> PostgresT m [res]
query_ q = do
  conn <- getConnection
  liftIO (Postgres.query_ conn q)

-- -- | Same as `query` but passing a specific row parser
queryWith
  :: (Postgres.ToRow params, MonadIO m) => Postgres.RowParser res -> Postgres.Query -> params -> PostgresT m [res]
queryWith parser q p = do
  conn <- getConnection
  liftIO (Postgres.queryWith parser conn q p)

-- -- | Same as `queryWith` but used when we don't pass any arguments
queryWith_ :: (MonadIO m) => Postgres.RowParser res -> Postgres.Query -> PostgresT m [res]
queryWith_ parser q = do
  conn <- getConnection
  liftIO (Postgres.queryWith_ parser conn q)

-- | Execute an @UPDATE@, @INSERT@ or @CREATE@ command
execute :: (Postgres.ToRow res, MonadIO m) => Postgres.Query -> res -> PostgresT m Int
execute q r = do
  conn <- getConnection
  liftIO (fromIntegral <$> Postgres.execute conn q r)

-- | Same as `execute` but used when we don't pass any argument
execute_ :: (MonadIO m) => Postgres.Query -> PostgresT m Int
execute_ q = do
  conn <- getConnection
  liftIO (fromIntegral <$> Postgres.execute_ conn q)

-- | Execute a command inside an exclusive transaction (no other writes or reads allowed)
withTransaction :: (MonadIO m, MonadMask m) => PostgresT m a -> PostgresT m a
withTransaction ma = do
  conn <- getConnection
  let begin = liftIO $ Postgres.begin conn
  let commit = liftIO $ Postgres.commit conn
  let rollback = liftIO $ Postgres.rollback conn
  mask $ \restore -> do
    begin
    a <- restore ma `onException` rollback
    commit
    return a

-- | Handle an exception raised internally by Postgres
handlePostgresError
  :: (MonadIO m, MonadCatch m) => PostgresT m a -> (Postgres.SqlError -> PostgresT m a) -> PostgresT m a
handlePostgresError pgsql handle = do
  try pgsql >>= \case
    Left e -> handle e
    Right a -> return a

-- | Parse a postgres field into an attribute value
fromPostgresField :: RawType -> Postgres.FieldParser RawVal
fromPostgresField r_ty f bs = do
  case r_ty of
    RBoolTy -> RawBool <$> Postgres.fromField @Bool f bs
    RDoubleTy -> RawDouble <$> Postgres.fromField @Double f bs
    RIntTy -> RawInt <$> Postgres.fromField @Int f bs
    RTextTy -> RawText <$> Postgres.fromField @Text f bs

instance Postgres.ToField RawVal where
  toField (RawBool b) = Postgres.toField b
  toField (RawDouble d) = Postgres.toField d
  toField (RawInt i) = Postgres.toField i
  toField (RawText t) = Postgres.toField t
