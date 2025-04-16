{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified DPella.Postgres as PG
import qualified DPella.MySQL as MS
import qualified DPella.SQLite as SQL
import Data.String (IsString)


-- Define the data structure for employees
data Employee = Employee
  { empName :: Text
  , empAge :: Int
  , empIsEmployed :: Bool
  }

-- Sample employee data
employees :: [Employee]
employees =
  [ Employee "Alice" 30 True
  , Employee "Bob" 24 False
  , Employee "Charlie" 45 True
  , Employee "Diana" 29 True
  ]


sumQuery :: IsString a => a
sumQuery = "SELECT SUM(CAST(age as FLOAT)) + dpella_sample_random(CAST(18 AS FLOAT),CAST(67 AS FLOAT)) FROM employees"

insertQuery :: IsString a => a
insertQuery = "INSERT INTO employees (name, age, is_employed) VALUES (?, ?, ?)"

createTableQuery :: (Semigroup a, IsString a) => a
createTableQuery = 
  "CREATE TABLE IF NOT EXISTS employees "
  <> "(id SERIAL PRIMARY KEY, "
  <> " name TEXT NOT NULL,"
  <> " age INTEGER NOT NULL,"
  <> " is_employed BOOLEAN NOT NULL)"

-- SQLite Example
runSQLiteExample :: IO ()
runSQLiteExample = SQL.runSQLite ":memory:" $ do
  liftIO $ putStrLn "--- Running SQLite Example ---"
  -- Create table
  _ <- SQL.execute_ createTableQuery
  liftIO $ putStrLn "SQLite table 'employees' created."

  -- Insert data
  forM_ employees $ \emp -> do
    SQL.execute insertQuery
      (empName emp, empAge emp, empIsEmployed emp)
  liftIO $ putStrLn $ "Inserted " <> show (length employees) <> " records into SQLite."

  -- Query sum of ages 4 times, to show randomness
  forM_ [1 :: Int ..4] $ \_ -> do
    [SQL.Only totalAge] :: [SQL.Only Double] <- SQL.query_ sumQuery
    liftIO $ putStrLn $ "Sum of ages (with noise) (SQLite): " <> show totalAge

-- PostgreSQL Example
runPostgresExample :: IO ()
runPostgresExample = do
    let connStr = "postgres://test:test@localhost:5432/test"
    putStrLn "\n--- Running PostgreSQL Example ---"
    putStrLn $ "Connecting to: " <> connStr
    PG.runPostgres (BS.pack connStr) $ do

      -- Drop and Create table
      _ <- PG.execute_ "DROP TABLE IF EXISTS employees;"
      _ <- PG.execute_ createTableQuery
      liftIO $ putStrLn "PostgreSQL table 'employees' created."

      -- Insert data
      forM_ employees $ \emp -> do
        PG.execute insertQuery
          (empName emp, empAge emp, empIsEmployed emp)
      liftIO $ putStrLn $ "Inserted " <> show (length employees) <> " records into PostgreSQL."

       -- Query sum of ages 4 times, to show randomness
      forM_ [1 :: Int ..4] $ \_ -> do
        [PG.Only totalAge] :: [PG.Only Double] <- PG.query_ sumQuery
        liftIO $ putStrLn $ "Sum of ages (PostgreSQL): " <> show totalAge

-- PostgreSQL Example
runMySQLxample :: IO ()
runMySQLxample = do
    let connStr = "mysql://test:test@localhost:3306/test"
    putStrLn "\n--- Running MySQL Example ---"
    putStrLn $ "Connecting to: " <> connStr
    MS.runMySQL (BS.pack connStr) $ do

      -- Drop and Create table
      _ <- MS.execute_ "DROP TABLE IF EXISTS employees;"
      _ <- MS.execute_ createTableQuery
      liftIO $ putStrLn "MySQL table 'employees' created."

      -- Insert data
      forM_ employees $ \emp -> do
        MS.execute insertQuery
          (empName emp, empAge emp, empIsEmployed emp)
      liftIO $ putStrLn $ "Inserted " <> show (length employees) <> " records into MySQL."

       -- Query sum of ages 4 times, to show randomness
      forM_ [1 :: Int ..4] $ \_ -> do
        [MS.Only totalAge] :: [MS.Only Double] <- MS.query_ sumQuery
        liftIO $ putStrLn $ "Sum of ages (MySQL): " <> show totalAge

main :: IO ()
main = do
  runSQLiteExample
  runPostgresExample
  runMySQLxample
