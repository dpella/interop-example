# Integrating Custom Haskell Functions into SQL Databases

This repository contains a proof of concept for integrating custom Haskell
functions into SQL databases, specifically focusing on SQLite, PostgreSQL, and
MySQL. The goal is to demonstrate how to create and use Haskell functions within
these database systems, providing a high-level overview of the integration
process.

## Guide to the Repository
This repository is organized into several directories, each serving a specific
purpose, we provide a brief overview of the main components:

- [Interoperability Report](doc/interoperability-report.md): A high-level
  overview of the integration process, focusing on the commonalities and
  differences between the integration into embedded (SQLite) and stand-alone
  (PostgreSQL and MySQL) databases.

- [Technical Report](doc/technical-report.md): A detailed technical report
  covering the implementation details, including workflow descriptions, code
  snippets explanations and examples.

- [Dockerfile](./Dockerfile): A Dockerfile for building a containerized
  environment to run the examples and tests. Refer to the [Technical
  Report](doc/technical-report.md) for detailed instructions on how to build and
  run the Docker container.

- Implementation directories:
  - [dpella-base](./dpella-base): Contains the implementation for SQLite
    integration.
  - [dpella-ffi](./dpella-ffi): Contains the implementation for PostgreSQL and
    MySQL integration using FFI.
  - [dpella-mysql](./dpella-mysql): Contains the implementation for MySQL
    integration using a custom plugin.
  - [dpella-postgresql](./dpella-postgresql): Contains the implementation for
    PostgreSQL integration using a custom extension.
  - [dpella-sqlite](./dpella-sqlite): Contains the implementation for SQLite
    integration.
  - [example](./example): Contains a working application that demonstrates how to
    use the custom Haskell functions in the different database systems.
  - [scripts](./scripts): Contains database-specific initialization scripts and
    configuration files.

## Quick Start

This repository provides a quick start script to help you set up and run the
examples. The script is located in the `scripts` directory and can be run
directly from the command line when you are in the root directory:

```bash
./scripts/quick-setup.sh
```

This script will set up the necessary environment, build the required Docker
image, and run the examples for each database system (i.e., SQLite, PostgreSQL,
and MySQL). Make sure you have [Docker Engine](https://docs.docker.com/engine/)
installed your machine before executing the script.

## Acknowledgments

Funded by the European Union. Views and opinions expressed are however those of
the author(s) only and do not necessarily reflect those of the European Union or
European Commission. Neither the Selected Third Parties Sub-Grant Agreement
European Union nor the granting authority can be held responsible for them.
Funded within the framework of the NGI Sargasso project under grant agreement No
101092887.

![NGI Sargasso](./doc/fig/logo-ngi-sargasso.png)
![European Union](./doc/fig/logo-eu.png)

## License
This project is distributed under the Mozilla License. See the
[LICENSE](LICENSE) for more information.
