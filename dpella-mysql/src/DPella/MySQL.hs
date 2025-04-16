{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module:      DPella.MySQL
-- Copyright:   (c) DPella AB 2023
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <matti@dpella.io>, <lobo@dpella.io>
module DPella.MySQL (
  MySQLT,
  runMySQLT,
  MySQL,
  runMySQL,
  query,
  query_,
  -- queryWith,
  -- queryWith_,
  execute,
  execute_,
  -- MySQL.SqlError (..),
  -- MySQL.Query,
  MySQL.Only (..),
  -- MySQL.conversionError,
  -- MySQL.Conversion,
  -- MySQL.QueryResults (..),
  -- MySQL.QueryParams,
  -- MySQL.FromField (..),
  -- MySQL.ToField (..),
  -- MySQL.FieldParser,
  -- MySQL.RowParser,
  -- MySQL.field,
  -- MySQL.fieldWith,
  -- MySQL.sql,
  -- MySQL.toJSONField,
  -- MySQL.Null (..),
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
import Database.MySQL.Simple qualified as MySQL
import Database.MySQL.Simple.QueryResults qualified as MySQL
import Database.MySQL.Simple.QueryParams qualified as MySQL
import Database.MySQL.Simple.Param qualified as MySQL

import DPella.RawVal

----------------------------------------

-- * A simple monadic interface to run queries inside SQLite databases

-- ** SQLite Monad

----------------------------------------

-- | The SLiteT monad
newtype MySQLT m a = MySQLT (ReaderT MySQL.Connection m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadTrans)

-- | Run an `MySQLT` computation given a connection string
runMySQLT :: (MonadIO m) => BS.ByteString -> MySQLT m a -> m a
runMySQLT db (MySQLT act) = do
  withCustomMySQL db $ \conn -> do
    runReaderT act conn

-- | The non-transformer version
type MySQL = MySQLT IO

-- | Run an `MySQL` computation given a path for the database
runMySQL :: BS.ByteString -> MySQL a -> IO a
runMySQL = runMySQLT

-- | Get the internal `SQLite` connection handle
getConnection :: (Monad m) => MySQLT m MySQL.Connection
getConnection = MySQLT ask

-- | Running inside a custom SQL environment with:
withCustomMySQL :: (MonadIO m) => BS.ByteString -> (MySQL.Connection -> m a) -> m a
withCustomMySQL db ma = do
  -- Open the connection
  conn <- liftIO $ MySQL.connect (parseConnectionString $ BS.unpack db)
  -- Run the computation in the extended function environment
  a <- ma conn
  -- Close the connection
  liftIO $ MySQL.close conn
  -- Return the result
  return a

-- ---------------------------------------

-- -- ** SQL commands

-- -- | Run a @SELECT@ query
query :: (MySQL.QueryResults res, MySQL.QueryParams params, MonadIO m) => MySQL.Query -> params -> MySQLT m [res]
query q p = do
  conn <- getConnection
  liftIO (MySQL.query conn q p)

-- -- | Same as `query` but used when we don't pass any arguments
query_ :: (MySQL.QueryResults res, MonadIO m) => MySQL.Query -> MySQLT m [res]
query_ q = do
  conn <- getConnection
  liftIO (MySQL.query_ conn q)

-- | Execute an @UPDATE@, @INSERT@ or @CREATE@ command
execute :: (MySQL.QueryParams res, MonadIO m) => MySQL.Query -> res -> MySQLT m Int
execute q r = do
  conn <- getConnection
  liftIO (fromIntegral <$> MySQL.execute conn q r)

-- | Same as `execute` but used when we don't pass any argument
execute_ :: (MonadIO m) => MySQL.Query -> MySQLT m Int
execute_ q = do
  conn <- getConnection
  liftIO (fromIntegral <$> MySQL.execute_ conn q)

instance MySQL.Param RawVal where
  render (RawBool b)   = MySQL.render b
  render (RawDouble d) = MySQL.render d
  render (RawInt i)    = MySQL.render i
  render (RawText t)   = MySQL.render t

-- | Parse a MySQL connection string in the format:
-- mysql://user:password@host:port/database
parseConnectionString :: String -> MySQL.ConnectInfo
parseConnectionString url = 
  let
    prefix = "mysql://" :: String
    withoutPrefix = drop (length prefix) url
    
    -- Extract user and rest
    (userPart, afterUser) = break (== ':') withoutPrefix
    
    -- Extract password and rest (skip ':')
    (passwordPart, afterPassword) = break (== '@') (drop 1 afterUser)
    
    -- Extract host and rest (skip '@')
    (hostPart, afterHost) = break (== ':') (drop 1 afterPassword)
    
    -- Extract port and database (skip ':')
    (portPart, databasePart) = break (== '/') (drop 1 afterHost)
    
    -- Skip '/' in database
    database = drop 1 databasePart
    
  in
    MySQL.defaultConnectInfo
      { MySQL.connectHost = hostPart
      , MySQL.connectPort = read portPart
      , MySQL.connectUser = userPart
      , MySQL.connectPassword = passwordPart
      , MySQL.connectDatabase = database
      }
