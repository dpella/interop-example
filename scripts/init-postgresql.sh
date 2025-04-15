#!/bin/bash
set -e

# Initialize PostgreSQL database
sudo -u postgres psql <<EOF
CREATE USER dpella WITH PASSWORD 'dpella' CREATEDB;
CREATE DATABASE dpella;
GRANT ALL PRIVILEGES ON DATABASE dpella TO dpella;
\c dpella
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "dpella-ffi-ext";
EOF

echo "PostgreSQL has been initialized with dpella database and extension"
