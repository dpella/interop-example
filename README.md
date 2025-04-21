# **Report: Integrating Custom Haskell Functions with SQL Engines (SQLite, PostgreSQL, MySQL)**

## Introduction 

At DPella, we leverage advanced programming language techniques in Haskell,
particularly its powerful type system, to develop robust and 
*correct by construction* applications. However, while Haskell excels in
enforcing complex invariants with type safety, it is not ideally suited for
processing large volumes of data. In contrast, relational databases are
designed for efficient data management and processing. In this repository, we
explore how we can offload data processing tasks to relational databases while
utilizing Haskell's capabilities for injecting noise into the data. 

The primary goal of this repository is to establish effective communication
between the Haskell runtime and the database engine runtime, enabling the
execution of Haskell logic directly within SQL queries. We focus on integrating
custom functions with both embedded databases like SQLite and external
databases such as PostgreSQL and MySQL.

The next figure shows the general idea of the project. 

![General architecture idea](./fig/arch1.gif)

The RDBMS will send data -- often the result of certain data analyses -- when
running SQL queries to DPella's implementation. DPella will *inject noise to
those results to protect the privacy of the individuals contributing with their
data to the dataset being analyzed -- DPella applies Differential Privacy
technology for this. The resulting noisy results and then send back to the
engine for further processing if needed. 

Generally speaking, this report details and compares methods for RDBMS being
able to call functions written in the Haskell. We evaluate three popular SQL
database engines: SQLite, PostgreSQL, and MySQL. The primary goal is to enable
the execution of Haskell logic by SQL queries, such as complex algorithms or
specialized computations like adding Differential Privacy noise. By doing so,
developers can leverage Haskell's strengths (e.g., type safety, functional
purity) within their existing database workflows.

## Motivating example

We will zoom in into the architecture shown above with a concrete example that
we will use in the rest of the report. 

![Two different runtimes](./fig/arch2.gif) 

The example focuses on SQL queries being able to call the Haskell function
`dpellaSampleRandom` (implemented in [Noise.hs](./dpella-base/src/DPella/Noise.hs)) which
generates a random number within a given range, potentially as part of a
Differential Privacy mechanism. This function requires managing state (the
random number generator) across calls. 

To call that function, queries use the SQL function `dpella_sample_random`. The
challenge here is three-fold: (i) to make the RDBMS to connect the SQL function
`dpella_sample_random` with the code in `dpellaSampleRandom`; (ii) passing all
the SQL arguments as `dpellaSampleRandom`'s arguments; and (iii) passing the
result of `dpellaSampleRandom` as the SQL result of `dpella_sample_random`.

## Running example

Set up the environment (builds Docker image with dependencies and extensions):

```bash
docker build -t sql-interoperability-example .
```


We consider a table of employees, where each row contains the name of the
employee, her/his age, and a boolean flag to indicate if she/he is still
employed (see code in [Main.hs](./example/app/Main.hs)). 

The example creates the table of employees, inserts some hard coded records, 
and executes the following query four times -- so that randomness can be seen. 

```SQL 
SELECT SUM(CAST(age as FLOAT)) + dpella_sample_random(CAST(18 AS FLOAT),CAST(67 AS FLOAT)) 
FROM employees
```

This query obtains the sum of all the ages and then adds a random number
between `18` and `67`. The reason to include `AS FLOAT` in the constants above
is connected to data marshalling across the RDBMS and Haskell (explained
later).

Run the example (executes [Main.hs](./example/app/Main.hs) within the Docker container):

```bash
docker run  sql-interoperability-example
```

The following output shows how we run such SQL operations in three different SQL
engines, i.e., SQLite, Postgres, and MySQL. 

```plaintext
--- Running SQLite Example ---
SQLite table 'employees' created.
Inserted 4 records into SQLite.
Sum of ages (with noise) (SQLite): 171.1366419561398
Sum of ages (with noise) (SQLite): 148.75287169458687
Sum of ages (with noise) (SQLite): 160.61467033334398
Sum of ages (with noise) (SQLite): 158.95724915195856

--- Running PostgreSQL Example ---
Connecting to: postgres://test:test@localhost:5432/test
PostgreSQL table 'employees' created.
Inserted 4 records into PostgreSQL.
Sum of ages (PostgreSQL): 166.50913874804027
Sum of ages (PostgreSQL): 190.64541265429347
Sum of ages (PostgreSQL): 190.05762465442217
Sum of ages (PostgreSQL): 183.1843888581907

--- Running MySQL Example ---
Connecting to: mysql://test:test@localhost:3306/test
MySQL table 'employees' created.
Inserted 4 records into MySQL.
Sum of ages (MySQL): 153.09850998336447
Sum of ages (MySQL): 150.8886262387034
Sum of ages (MySQL): 152.52728376912134
Sum of ages (MySQL): 176.15142166159143
```

In what follows the report outline the distinct integration architecture
required for each engine, analyses common components across the Haskell modules
for interoperability. Files
[SQLite.hs](./dpella-sqlite/src/DPella/SQLite.hs),
[Postgres.hs](./dpella-postgres/src/DPella/Postgres.hs), and
[MySQL.hs](./dpella-mysql/src/DPella/MySQL.hs) provide the required
infrastructure (e.g., types, monads) to run the SQL instructions described in
[Main.hs](./example/app/Main.hs), and presents a comparative analysis of the
approaches.

## **3. Overview**

This repository demonstrates how to integrate the custom Haskell function `dpella_sample_random` into SQL queries across SQLite, PostgreSQL, and MySQL. The general approach involves:

1.  Defining the core Haskell function (`dpellaSampleRandom` in `DPella.Noise`) and its state (`NoiseGen`).
2.  Exposing this function to the respective SQL engines using engine-specific mechanisms.
3.  Providing Haskell wrapper modules (`DPella.SQLite`, `DPella.Postgres`, `DPella.MySQL`) to simplify interaction with each database from Haskell application code.

* **SQLite**: Runs within the same process as the Haskell application (`app/Main.hs`). Custom functions are directly registered using the `sqlite-simple` Haskell API within `DPella.SQLite.withSQLFunctions`. This allows seamless invocation from SQL queries executed via `DPella.SQLite.query_`, as seen in `runSQLiteExample`.

* **PostgreSQL**: Runs as a separate process. Integration is achieved by creating a PostgreSQL extension (`dpella-ffi-ext`) as a shared library written in C (`dpella-ffi/pg_extension/dpella-ffi-ext.c`). This C code calls into the Haskell function (`wrappedDpellaSampleRandom` exposed via FFI in `dpella-ffi/src/DPella_FFI.hs`). The Haskell runtime is explicitly initialized (`hs_init`) and finalized (`hs_exit`) within the PostgreSQL extension lifecycle functions (`_PG_init`, `_PG_fini`). The Haskell application (`app/Main.hs`) connects using a connection string and executes queries via `DPella.Postgres.query_`.

* **MySQL**: Also runs as a separate process. Custom functions are dynamically loaded using MySQL's User Defined Function (UDF) mechanism, defined via `CREATE FUNCTION ... SONAME ...` (`dpella-ffi/mysql_plugin/init.sql`). A C shared library (`dpella-ffi/mysql_plugin/dpella_ffi_mysql.c`) acts as a bridge, calling the FFI-exposed Haskell function. The Haskell runtime is lazily initialized (`hs_init`) upon the first function call using a mutex for thread safety and remains active for the lifetime of the MySQL process. The Haskell application (`app/Main.hs`) connects using a connection string and executes queries via `DPella.MySQL.query_`.

Each engine-specific integration is encapsulated in dedicated Haskell modules (`DPella.SQLite`, `DPella.Postgres`, `DPella.MySQL`) and corresponding FFI/C code where applicable.

## **4. Commonalities Across Engines**

Despite differences in integration specifics, several common aspects are observed:

* **Haskell Wrapper Modules:** Each engine has a corresponding Haskell module (`DPella.SQLite`, `DPella.Postgres`, `DPella.MySQL`) providing a monadic interface (`SQLiteT`, `PostgresT`, `MySQLT`) built on `ReaderT Connection m a`. These modules offer standardized functions like `run*`, `query`, `query_`, `execute`, and `execute_` to abstract database interactions.
* **Core Haskell Logic:** All integrations ultimately call the same underlying Haskell function, `dpellaSampleRandom` from `DPella.Noise`, ensuring consistent behavior.
* **Stateful Random Number Generation:** The core `dpellaSampleRandom` function utilizes a shared, stateful random number generator (`NoiseGen` implemented as an `IORef StdGen` in `DPella.Noise`).
    * For SQLite, this state is managed within an `SQLEnv` created per connection context in `withSQLFunctions`.
    * For PostgreSQL and MySQL, state is managed via a global `NOINLINE` `IORef` (`nOISEGEN`) within the FFI module (`DPella_FFI.hs`), made accessible via `unsafePerformIO`. This implies shared state across all connections within the database process where the Haskell runtime is loaded.
* **Use of Foreign Function Interface (FFI):** Both PostgreSQL and MySQL integrations rely on Haskell's FFI (`foreign export ccall` in `DPella_FFI.hs`) to expose `wrappedDpellaSampleRandom` to the C bridge code. SQLite, running in-process, uses the `sqlite-simple` library's `createFunction` API directly without needing explicit FFI exports for this purpose.
* **Custom SQL Function Definition:** Each integration defines a custom SQL function named `dpella_sample_random` that invokes the Haskell implementation. This provides a uniform SQL interface across engines, as used in `sumQuery` within `app/Main.hs`.
    ```haskell
    -- Defined in app/Main.hs
    sumQuery :: IsString a => a
    sumQuery = "SELECT SUM(CAST(age as FLOAT)) + dpella_sample_random(CAST(18 AS FLOAT),CAST(67 AS FLOAT)) FROM employees"
    ```
* **Modular Design:** The project structure separates core Haskell logic (`dpella-base`), FFI bindings (`dpella-ffi`), engine-specific Haskell wrappers (`dpella-sqlite`, `dpella-postgres`, `dpella-mysql`), and the example application (`example`).

## **5. Detailed Report and Comparison**

This repository demonstrates interoperability between Haskell and three SQL engines. Each requires a different approach to integrate the `dpella_sample_random` Haskell function.



### **5.1. SQLite**

```ascii
+-------------------------------------------------+
|             Haskell Application Process         |
| +---------------------+   +-------------------+ |
| |   app/Main.hs       |   | DPella.SQLite.hs  | |  (Runs :memory: DB)
| |---------------------|   |-------------------| |
| | SQL.runSQLite(...)  |   | withSQLFunctions  | |
| |  |                  |   |  |                | |
| |  `-> SQL.query_(..) |<--+  | createFunction | |<----+    (Haskell Func)
| |        | "SELECT..  |   |  `->(impl env)----|------>| dpellaSampleRandom |
| |        | dpella_..."|   +-------------------+ |     +--------------------+
| |        V            |                         |     |   (Uses NoiseGen)  |
| | +-------------------+                         |     +--------------------+
| | |  sqlite-simple    |------------------------>|
| | | (SQLite C Lib)    |<--- Response <----------|
| | +-------------------+                         |
+-------------------------------------------------+
```

* **Explanation:** The Haskell application (`app/Main.hs`) uses the `DPella.SQLite` wrapper. `withSQLFunctions` registers the `dpellaSampleRandom` Haskell function directly with the in-process SQLite library (`sqlite-simple`) using `createFunction`. SQL queries executed via `query_` can then directly call `dpella_sample_random`, which executes the registered Haskell code within the same process.


* **Integration:** Runs in-process with the Haskell application. Functions are registered/unregistered directly using `sqlite-simple`'s API within `DPella.SQLite.withSQLFunctions`. An `SQLEnv` holding the `NoiseGen` is created for the scope of the connection.
* **Invocation Mechanism:** The Haskell function `dpellaSampleRandom` is wrapped and registered with the SQLite connection using `SQLite.createFunction`. The `SQLEnv` containing the stateful `NoiseGen` is passed to the implementation.
    ```haskell
    -- From dpella-sqlite/src/DPella/SQLite.hs

    -- Defines the SQL function structure
    data SQLFunction where
      SQLFunction :: (SQLite.Function f) => FunctionName -> FunctionImpl f -> SQLFunction

    -- Specifies the function name and the Haskell implementation (using the env's RNG)
    sqlDpellaSampleRandom :: SQLFunction
    sqlDpellaSampleRandom =
      SQLFunction "dpella_sample_random" $ dpellaSampleRandom . sqlite_env_rng

    -- Registers the function(s) within the connection scope
    withSQLFunctions :: (MonadIO m) => [SQLFunction] -> SQLite.Connection -> (SQLite.Connection -> m a) -> m a
    withSQLFunctions funs conn ma = do
      env <- liftIO initSQLiteEnv -- Creates environment with NoiseGen
      liftIO $ forM_ funs $ \(SQLFunction name impl) -> do
        -- `impl env` provides the actual Haskell function IO () -> IO SQLData
        res <- SQLite.createFunction conn name (impl env) -- Registers the function
        case res of
          Left err -> throwIO (userError (show err))
          Right () -> return ()
      a <- ma conn -- Computation uses the function
      liftIO $ forM_ funs $ \(SQLFunction name _) -> do
        SQLite.deleteFunction conn name -- Unregisters the function
      return a
    ```
* **Usage (`app/Main.hs`):**
    ```haskell
    runSQLiteExample :: IO ()
    runSQLiteExample = SQL.runSQLite ":memory:" $ do -- Establishes connection and manages functions
      liftIO $ putStrLn "--- Running SQLite Example ---"
      -- ... (create table, insert data) ...
      -- Query sum of ages 4 times, to show randomness
      forM_ [1 :: Int ..4] $ \_ -> do
        [SQL.Only totalAge] :: [SQL.Only Double] <- SQL.query_ sumQuery -- Executes query using the custom function
        liftIO $ putStrLn $ "Sum of ages (with noise) (SQLite): " <> show totalAge
    ```
* **Pros:** Simple integration; easy state management within the Haskell application context.
* **Cons:** Limited scalability and concurrency due to single-process architecture.

### **5.2. PostgreSQL**

```ascii
+---------------------------+      +--------------------------------------+
| Haskell App Process       |      | PostgreSQL Server Process            |
| +-----------------------+ |      | +----------------------------------+ |
| |    app/Main.hs        | |      | | SQL Engine                       | |
| |-----------------------| |      | |----------------------------------| |
| | PG.runPostgres(...)   | |      | | "SELECT...dpella_sample_random"  | |
| |  |                    | |      | |        |                         | |
| |  `-> PG.query_(...) --|------> | |        V                         | |
| +-----------------------+ | CONN | | +------------------------------+ | |
|                           |      | | | dpella-ffi-ext.so (C Ext.)   | | |
|                           |      | | |------------------------------| | |
|                           |      | | | _PG_init() -> hs_init()      | | |
|                           |      | | | pg_dpella_sample_random()    | | |
|                           |      | | |  |                           | | |
|                           |      | | |  `-> dpella_sample_random_hs | | |
|                           |      | | +------|-----------------------+ | |
|                           |      | +--------|-------------------------+ |
|                           |      |          V (FFI Call)                |
|                           |      | +------------------------------+     |
|                           |      | | Haskell RTS (within PG Proc) |     |
|                           |      | |------------------------------|     |
|                           |      | | DPella_FFI.hs                |     |
|                           |      | |  `- wrappedDpellaSampleRandom|     |
|                           |      | |     (Uses global NoiseGen)   |     |
|                           |      | +------------------------------+     |
|                           | <----|- Response                            |
|                           | CONN |                                      |
+---------------------------+      +--------------------------------------+

```

* **Explanation:** The Haskell app connects to the separate PostgreSQL server. An SQL query uses `dpella_sample_random`. PostgreSQL maps this (via `CREATE FUNCTION`) to the C function `pg_dpella_sample_random` in the loaded extension (`dpella-ffi-ext.so`). This C function calls the Haskell function `dpella_sample_random_hs` via the Foreign Function Interface (FFI). The Haskell code runs within the Haskell Runtime System (RTS) initialized (`hs_init`) inside the PostgreSQL process.




* **Integration:** Runs as a separate process. Requires a C extension (`dpella-ffi-ext.c`) using FFI to call Haskell (`DPella_FFI.hs`). The Haskell runtime is explicitly managed via `hs_init`/`hs_exit` in `_PG_init`/`_PG_fini`. The extension is created via SQL (`dpella-ffi-ext--1.0.sql`). State (`NoiseGen`) is global within the FFI module.
* **Invocation Mechanism:** The SQL function `dpella_sample_random` is linked to the C function `pg_dpella_sample_random`. This C function retrieves arguments and calls the Haskell function `dpella_sample_random_hs` (which is the FFI export name for `wrappedDpellaSampleRandom`) via the FFI stub header.
    ```sql
    -- From dpella-ffi/pg_extension/dpella-ffi-ext--1.0.sql
    -- Links SQL function name to the C function in the shared library ('MODULE_PATHNAME')
    CREATE FUNCTION dpella_sample_random(result FLOAT8, param FLOAT8)
    RETURNS FLOAT8
    AS 'MODULE_PATHNAME', 'pg_dpella_sample_random'
    LANGUAGE C IMMUTABLE STRICT;
    ```
    ```c
    // From dpella-ffi/pg_extension/dpella-ffi-ext.c
    #include <DPella_FFI_stub.h> // Provides declaration for dpella_sample_random_hs

    PG_FUNCTION_INFO_V1(pg_dpella_sample_random);
    Datum pg_dpella_sample_random(PG_FUNCTION_ARGS) {
        // Retrieve the two arguments from PostgreSQL function call
        float8 arg1 = PG_GETARG_FLOAT8(0);
        float8 arg2 = PG_GETARG_FLOAT8(1);
        // Call the Haskell function via FFI
        double result = dpella_sample_random_hs(arg1, arg2);
        PG_RETURN_FLOAT8(result);
    }
    ```
    ```haskell
    // From dpella-ffi/src/DPella_FFI.hs
    -- Exports the wrapped Haskell function under the C name "dpella_sample_random_hs"
    foreign export ccall "dpella_sample_random_hs"
      wrappedDpellaSampleRandom :: CDouble -> CDouble -> IO CDouble
    ```
* **Usage (`app/Main.hs`):**
    ```haskell
    runPostgresExample :: IO ()
    runPostgresExample = do
        let connStr = "postgres://test:test@localhost:5432/test" -- Connection String
        putStrLn "\n--- Running PostgreSQL Example ---"
        PG.runPostgres (BS.pack connStr) $ do -- Establishes connection
          -- ... (create table, insert data) ...
          -- Query sum of ages 4 times, to show randomness
          forM_ [1 :: Int ..4] $ \_ -> do
            [PG.Only totalAge] :: [PG.Only Double] <- PG.query_ sumQuery -- Executes query using the custom function
            liftIO $ putStrLn $ "Sum of ages (PostgreSQL): " <> show totalAge
    ```
* **Pros:** Robust and scalable; clear lifecycle management of Haskell runtime via extension hooks.
* **Cons:** Complex setup and deployment (C code, FFI, extension build/install); global state in FFI module might have implications.

### **5.3. MySQL**


```ascii
+---------------------------+      +--------------------------------------+
| Haskell App Process       |      | MySQL Server Process (mysqld)        |
| +-----------------------+ |      | +----------------------------------+ |
| |    app/Main.hs        | |      | | SQL Engine                       | |
| |-----------------------| |      | |----------------------------------| |
| | MS.runMySQL(...)      | |      | | "SELECT...dpella_sample_random"  | |
| |  |                    | |      | |        |                         | |
| |  `-> MS.query_(...) --|------> | |        V                         | |
| +-----------------------+ | CONN | | +------------------------------+ | |
|                           |      | | | libdpella_ffi_mysql.so(UDF)  | | |
|                           |      | | |------------------------------| | |
|                           |      | | | init_shared_state()          | | |
|                           |      | | |  (Lazy hs_init() + Mutex)    | | |
|                           |      | | | dpella_sample_random()       | | |
|                           |      | | |  |                           | | |
|                           |      | | |  `-> dpella_sample_random_hs | | |
|                           |      | | +------|-----------------------+ | |
|                           |      | +--------|-------------------------+ |
|                           |      |          V (FFI Call)                | 
|                           |      | +------------------------------+     |
|                           |      | | Haskell RTS (within MySQL)   |     |
|                           |      | |------------------------------|     |
|                           |      | | DPella_FFI.hs                |     |
|                           |      | |  `- wrappedDpellaSampleRandom|     |
|                           |      | |     (Uses global NoiseGen)   |     |
|                           |      | +------------------------------+     |
|                           | <----|- Response                            |
|                           | CONN |                                      |
+---------------------------+      +--------------------------------------+
```

* **Explanation:** The Haskell app connects to the separate MySQL server. An SQL query uses `dpella_sample_random`. MySQL maps this (via `CREATE FUNCTION ... SONAME`) to the C function `dpella_sample_random` in the loaded shared library (`libdpella_ffi_mysql.so`). This C function calls the Haskell function `dpella_sample_random_hs` via FFI. The Haskell RTS is lazily initialized (thread-safe) on the first call within the MySQL process.


* **Integration:** Runs as a separate process. Uses a dynamically loaded UDF (`dpella_ffi_mysql.c`) registered via `CREATE FUNCTION ... SONAME ...` (`init.sql`). The C UDF calls the Haskell function via FFI (`DPella_FFI.hs`). Haskell runtime is lazily initialized on first call with mutex protection (`init_shared_state`) and persists. State (`NoiseGen`) is global within the FFI module.
* **Invocation Mechanism:** The SQL function `dpella_sample_random` is linked to the shared library `libdpella_ffi_mysql.so`. MySQL calls the C function `dpella_sample_random` within that library for each row. This C function retrieves arguments and calls the Haskell function `dpella_sample_random_hs` via the FFI stub header.
    ```sql
    -- From dpella-ffi/mysql_plugin/init.sql
    -- Links the SQL function name to the C function implementation within the shared library
    CREATE FUNCTION dpella_sample_random RETURNS REAL SONAME "libdpella_ffi_mysql.so";
    ```
    ```c
    // From dpella-ffi/mysql_plugin/dpella_ffi_mysql.c
    #include <DPella_FFI_stub.h> // Provides declaration for dpella_sample_random_hs

    // Main UDF function called per row
    double dpella_sample_random(UDF_INIT *initid, UDF_ARGS *args,
                                        char *is_null, char *error) {
        // Retrieve arguments
        double arg1 = *((double*)args->args[0]);
        double arg2 = *((double*)args->args[1]);

        // Call the Haskell function via FFI
        double result = dpella_sample_random_hs(arg1, arg2);
        *((double*)initid->ptr) = result; // Store result (optional depending on UDF needs)

        return result; // Return result to MySQL
    }
    ```
    ```haskell
    // From dpella-ffi/src/DPella_FFI.hs
    -- Exports the wrapped Haskell function under the C name "dpella_sample_random_hs"
    foreign export ccall "dpella_sample_random_hs"
      wrappedDpellaSampleRandom :: CDouble -> CDouble -> IO CDouble
    ```
* **Usage (`app/Main.hs`):**
    ```haskell
     runMySQLxample :: IO ()
     runMySQLxample = do
         let connStr = "mysql://test:test@localhost:3306/test" -- Connection String
         putStrLn "\n--- Running MySQL Example ---"
         MS.runMySQL (BS.pack connStr) $ do -- Establishes connection
           -- ... (create table, insert data) ...
           -- Query sum of ages 4 times, to show randomness
           forM_ [1 :: Int ..4] $ \_ -> do
             [MS.Only totalAge] :: [MS.Only Double] <- MS.query_ sumQuery -- Executes query using the custom function
             liftIO $ putStrLn $ "Sum of ages (MySQL): " <> show totalAge
    ```
* **Pros:** Flexible dynamic loading; simpler lazy runtime initialization.
* **Cons:** Less structured lifecycle management (no explicit `hs_exit` tied to UDF unload); potential complexity managing global FFI state and concurrency.

## **6. Summary of Approaches**

| Engine      | Integration Method      | Complexity | Scalability | Runtime Management           | State (`NoiseGen`) Scope | Invocation Path                     |
| :---------- | :---------------------- | :--------- | :---------- | :--------------------------- | :----------------------- | :---------------------------------- |
| SQLite      | Direct API registration | Low        | Low         | Simple (App scope)           | Per Connection (`SQLEnv`)  | SQL -> `sqlite-simple` -> Haskell |
| PostgreSQL  | C Extension + FFI       | High       | High        | Explicit (`_PG_init`/`_fini`)  | Global FFI Module        | SQL -> C Extension -> FFI -> Haskell |
| MySQL       | C UDF + FFI             | Medium     | Medium      | Lazy Init (First Call)       | Global FFI Module        | SQL -> C UDF -> FFI -> Haskell    |

## **7. Conclusion**

Integrating Haskell functions like `dpella_sample_random` into SQL databases is achievable for SQLite, PostgreSQL, and MySQL, each with distinct mechanisms and trade-offs. SQLite offers simplicity for embedded or single-process use cases with per-connection state management. PostgreSQL provides a robust, scalable solution via its extension system but requires significant setup complexity. MySQL offers a middle ground with dynamic UDFs and lazy initialization, trading some lifecycle clarity for flexibility. The choice depends on the specific requirements for scalability, deployment complexity, and state management strategy. The use of common wrapper modules simplifies application-level code, while the underlying integration method dictates the operational characteristics.
