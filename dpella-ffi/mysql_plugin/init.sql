-- dpella-mysql.sql
-- SQL functions for DPella MySQL plugin

-- In MySQL, confusingly, REAL is a synoym for REAL. So we use REAL here.
CREATE FUNCTION dpella_sample_random RETURNS REAL SONAME "libdpella_ffi_mysql.so";

