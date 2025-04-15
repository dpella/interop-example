-- haskell_ext--1.0.sql
-- Extension script for haskell_ext extension

-- Complain if the script is run directly, instead of via CREATE EXTENSION
\echo Use "CREATE EXTENSION dpella-ffi-ext" to load this file. \quit

-- Note: FLOAT8 is the same as double precision, but since the C functions refer to FLOAT8,
-- we use that here
CREATE FUNCTION dpella_sample_random(result FLOAT8, param FLOAT8)
RETURNS FLOAT8
AS 'MODULE_PATHNAME', 'pg_dpella_sample_random'
LANGUAGE C IMMUTABLE STRICT;
