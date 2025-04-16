# SQL Inter-opearbility Example


This repository shows how we can connect different SQL engines to Haskell, and how can define
custom Haskell functions that can be invoked from each engine.


## Running the example

Set up the environment:

```bash
docker build -t sql-interoperability-example .
```

Run the example:

```bash
docker run -it sqlinteroperability-example
```


## Report

### SQLite

In SQLite, database lives in the same process as the Haskell process.
This makes it easy to use Haskell functions in SQLite queries,
which we simply *register* directly in Haskell:

```haskell
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
```

This creates a SQLite function that can be used in SQL queries.
Each function is invoked with the `SQLiteEnv` that is passed to the function, which
contains the current state of the random number generator.


### PostgreSQL
In postgresql, the database lives in a different process than the Haskell process.
This makes it more difficult to use Haskell functions in PostgreSQL queries.
We need to create a shared library that can be loaded by PostgreSQL, as a PostgreSQL extension.

This is done by defining a `foreign` function in Haskell, as is done in `dpella-ffi`, and
then defining a PostgreSQL extension that loads the shared library.

Since this lives in a separate process, we need to make sure that the Haskell runtime is initialized
before we can use any Haskell functions.

This is done in the `init` function of the PostgreSQL extension, which is called when the extension is loaded,
and deinitialization happens in the `fini` function, which is called when the extension is unloaded.

### MySQL
In MySQL, the database also lives in a different process than the Haskell process.
However, MySQL does not have extensions like PostgreSQL, so we need to use a different approach.
We need to create a library of functions that can be loaded by MySQL, and then use the `CREATE FUNCTION` statement to register the functions in MySQL.

This means that we need to do initialization and deinitialization for each function call.
But since we want to maintain state between function calls, we need to use a different approach.
In this plugin, we simply initialize the Haskell runtime when the function is called *if it has not been initialized yet*,
and then we never deinitialize it.

This is OK in practice, since the Haskell runtime is designed to be initialized once and then used for the lifetime of the process.

