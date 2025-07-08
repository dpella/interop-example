# **Interoperability Report: Integrating Custom Haskell Functions into SQL Databases**

This report explores the integration of Haskell functions into SQL database
systems, focusing on three popular engines: SQLite (embedded) and
PostgreSQL and MySQL (standalone). It provides a high-level overview of the
integration process, highlighting the differences between these database types.
Designed for developers and stakeholders, this report aims to explain how the
integration works and varies across database engines, without delving into
technical implementation details.

## **Why Combine Haskell with SQL?**

* **Custom Logic**: Haskell is great for writing precise, reusable
  functions—like adding randomness for privacy.
* **Type Safety**: Haskell ensures your logic is consistent and safe.
* **Efficiency**: SQL is optimized for working with large datasets; combining
  both brings the best of each.

## **General Integration Approach**

1. __Create__ the Haskell function (e.g., `dpellaSampleRandom` for generating
   noise).
2. __Expose__ it in a way the database can call it (this varies by engine).
3. __Use__ the function inside SQL queries like any built-in function.

## **Common Integration Elements**

Regardless of the database engine:

* The function is written in Haskell and compiled.
* A connection is made between SQL and Haskell, either through embedded code
  (SQLite) or an extension/plugin (PostgreSQL, MySQL).
* Haskell's random generator is managed to ensure consistent behavior.
* All databases call the same core function, ensuring consistent results across
  engines.

## **Database-Specific Details**

### **1. SQLite**

**Type**: Embedded (runs inside the Haskell program)

**Integration Method**: Direct registration using a Haskell library (`sqlite-simple`)

**Strengths**:

* Very easy to integrate
* Lightweight and ideal for prototyping or small-scale apps

**Weaknesses**:

* Does not scale well for high-concurrency or networked applications
* Tied closely to the Haskell application process

**Use Case**: Local tools or desktop apps needing custom logic without database
server overhead


### **2. PostgreSQL**

**Type**: Standalone server (runs independently of the Haskell app)

**Integration Method**: Uses a PostgreSQL extension written in C that connects
to Haskell via FFI (Foreign Function Interface)

**Strengths**:

* Very scalable and robust
* Clean, structured lifecycle for the Haskell runtime

**Weaknesses**:

* Setup is complex and requires working with C code and database extensions
* Deployment may require administrative access

**Use Case**: Enterprise-level applications where performance, scalability, and
integration quality matter

### **3. MySQL**

**Type**: Standalone server

**Integration Method**: Loads Haskell via a User-Defined Function (UDF) written
in C

**Strengths**:

* Flexible and easier to load dynamically at runtime
* Good balance of power and ease of use

**Weaknesses**:

* Runtime setup is less structured (lacks explicit shutdown process)
* Requires thread safety and careful handling of shared state

**Use Case**: Web apps or moderate-scale services where flexibility and ease of
setup are desired

## **Comparison Table**

| Feature                | SQLite                  | PostgreSQL                   | MySQL                 |
| ---------------------- | ----------------------- | ---------------------------- | --------------------- |
| **Runs as**            | Embedded process        | External server              | External server       |
| **Integration**        | Direct API registration | C extension + FFI            | C UDF + FFI           |
| **Complexity**         | Low                     | High                         | Medium                |
| **State Management**   | Per connection          | Global (Haskell FFI)         | Global (Haskell FFI)  |
| **Setup Requirements** | Minimal                 | PostgreSQL admin             | MySQL plugin loading  |
| **Lifecycle Control**  | Simple (app scope)      | Explicit (`_PG_init`)        | Lazy on first call    |
| **Best For**           | Lightweight domains     | Large scale, complex queries | Moderate load |


## **Conclusion**

By integrating Haskell functions into SQL databases, developers can unlock
powerful new workflows that combine Haskell's expressiveness with SQL's data
handling. The approach varies by engine:

* **SQLite**: Best for lightweight, embedded apps.
* **PostgreSQL**: Ideal for complex, production-scale systems.
* **MySQL**: Balanced choice with dynamic flexibility.

With this setup, Haskell logic can enhance database queries--enabling advanced
features like Differential Privacy--while remaining scalable and reusable.