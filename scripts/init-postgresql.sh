#!/bin/bash
set -e

# Initialize PostgreSQL database
sudo -u postgres psql <<EOF
CREATE USER test WITH PASSWORD 'test' CREATEDB;
CREATE DATABASE test;
GRANT ALL PRIVILEGES ON DATABASE test TO test;
\c dpella
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "dpella-ffi-ext";
EOF

echo "PostgreSQL has been initialized with dpella database and extension"
