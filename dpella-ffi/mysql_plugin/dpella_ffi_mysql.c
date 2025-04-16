#include <server/mysql.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <DPella_FFI_stub.h> // Generated from Haskell FFI

// Shared state structure
typedef struct {
    int hs_initialized;
    pthread_mutex_t lock;
} SharedState;

// Global shared state
static SharedState shared_state = {0};

// Function to initialize shared state
static void init_shared_state() {
    // Initialize the mutex if not already initialized
    if (!shared_state.hs_initialized) {
        pthread_mutex_init(&shared_state.lock, NULL);
        hs_init(NULL, NULL);
        shared_state.hs_initialized = 1;
    }
}

// Init function
int dpella_sample_random_init(UDF_INIT *initid, UDF_ARGS *args, char *message) {
    init_shared_state();
    pthread_mutex_lock(&shared_state.lock);

    // Check argument count
    if (args->arg_count != 2) {
        strcpy(message, "dpella_sample_random requires two arguments");
        pthread_mutex_unlock(&shared_state.lock);
        return 1;
    }

    // Check argument types
    if (args->arg_type[0] != REAL_RESULT || args->arg_type[1] != REAL_RESULT) {
        strcpy(message, "dpella_sample_random requires REAL arguments");
        pthread_mutex_unlock(&shared_state.lock);
        return 1;
    }

    // Allocate memory for result
    initid->ptr = (char*)malloc(sizeof(double));
    initid->maybe_null = 0;
    return 0; // Success
}

// Deinit function
void dpella_sample_random_deinit(UDF_INIT *initid) {
    // Free allocated memory
    if (initid->ptr) {
        free(initid->ptr);
        initid->ptr = NULL;
    }
    pthread_mutex_unlock(&shared_state.lock);
}

// Main function
double dpella_sample_random(UDF_INIT *initid, UDF_ARGS *args,
                                    char *is_null, char *error) {
    double arg1 = *((double*)args->args[0]);
    double arg2 = *((double*)args->args[1]);

    // Call the Haskell function
    double result = dpella_sample_random_hs(arg1, arg2);
    *((double*)initid->ptr) = result;

    return result;
}
