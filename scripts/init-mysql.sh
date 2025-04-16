#!/bin/bash
set -e


sudo mysql  <<EOF
CREATE DATABASE test;
CREATE USER 'test'@'localhost' IDENTIFIED BY 'test';

GRANT ALL PRIVILEGES ON test.* TO 'test'@'localhost';
FLUSH PRIVILEGES;
USE test;
SOURCE /app/dpella-ffi/mysql_plugin/init.sql;
EXIT
EOF

echo "MySQL has been initialized with test database and extension"
