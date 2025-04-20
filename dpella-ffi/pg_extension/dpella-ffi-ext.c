// dpella-ffi-ext.c
#include "postgres.h"
#include "fmgr.h"
#include <DPella_FFI_stub.h>
#include <stdint.h>        // for consistent integer types (if needed)

// Prototypes 
void _PG_init(void); 
void _PG_fini(void); 

// Declare the SQL-callable wrapper function
PG_MODULE_MAGIC;                 // Required magic to ensure compatibility

PG_FUNCTION_INFO_V1(pg_dpella_sample_random);
Datum pg_dpella_sample_random(PG_FUNCTION_ARGS) {
    // Retrieve the two arguments from PostgreSQL function call
    float8 arg1 = PG_GETARG_FLOAT8(0);
    float8 arg2 = PG_GETARG_FLOAT8(1);
    // Call the Haskell function
    double result = dpella_sample_random_hs(arg1, arg2);
    PG_RETURN_FLOAT8(result);
}


// Initialization function for the extension (called on library load)
void _PG_init(void) {
    // Initialize Haskell runtime system (RTS)
    hs_init(NULL, NULL);
}

// Optional: Cleanup function (note: might not be called by Postgres, as extensions persist)
void _PG_fini(void) {
    hs_exit();
}
