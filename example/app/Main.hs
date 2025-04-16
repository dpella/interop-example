{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified DPella.Postgres as PG
import qualified DPella.SQLite as SQL


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

-- SQLite Example
runSQLiteExample :: IO ()
runSQLiteExample = SQL.runSQLite ":memory:" $ do
  liftIO $ putStrLn "--- Running SQLite Example ---"

  -- Create table
  _ <- SQL.execute_ [SQL.sql|
    CREATE TABLE IF NOT EXISTS employees (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      age INTEGER NOT NULL,
      is_employed BOOLEAN NOT NULL
    )
  |]
  liftIO $ putStrLn "SQLite table 'employees' created."

  -- Insert data
  forM_ employees $ \emp -> do
    SQL.execute
      "INSERT INTO employees (name, age, is_employed) VALUES (?, ?, ?)"
      (empName emp, empAge emp, empIsEmployed emp)
  liftIO $ putStrLn $ "Inserted " <> show (length employees) <> " records into SQLite."

  -- Query sum of ages
  [SQL.Only totalAge] :: [SQL.Only Int] <- SQL.query_ "SELECT SUM(age) FROM employees"
  liftIO $ putStrLn $ "Sum of ages (SQLite): " <> show totalAge

-- PostgreSQL Example
runPostgresExample :: IO ()
runPostgresExample = do
    -- NOTE: Replace with your actual connection string or use environment variables
    let connStr = "host=localhost port=5432 dbname=test user=test password=test"
    putStrLn "\n--- Running PostgreSQL Example ---"
    putStrLn $ "Connecting to: " <> connStr
    PG.runPostgres (BS.pack connStr) $ do

      -- Drop and Create table
      _ <- PG.execute_ "DROP TABLE IF EXISTS employees;"
      _ <- PG.execute_ [PG.sql|
        CREATE TABLE employees (
          id SERIAL PRIMARY KEY,
          name TEXT NOT NULL,
          age INTEGER NOT NULL,
          is_employed BOOLEAN NOT NULL
        )
      |]
      liftIO $ putStrLn "PostgreSQL table 'employees' created."

      -- Insert data
      forM_ employees $ \emp -> do
        PG.execute
          "INSERT INTO employees (name, age, is_employed) VALUES (?, ?, ?)"
          (empName emp, empAge emp, empIsEmployed emp)
      liftIO $ putStrLn $ "Inserted " <> show (length employees) <> " records into PostgreSQL."

      -- Query sum of ages
      [PG.Only totalAge] :: [PG.Only Int] <- PG.query_ "SELECT SUM(age) FROM employees"
      liftIO $ putStrLn $ "Sum of ages (PostgreSQL): " <> show totalAge

main :: IO ()
main = do
  runSQLiteExample
  runPostgresExample
