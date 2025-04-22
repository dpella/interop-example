# Makefile for DPella MySQL Plugin

# Set the path to GHC
USER = "dpella"
GHC_PATH = /home/${USER}/.ghcup/bin

# Variables to adjust
MYSQL_CONFIG ?= mysql_config
PLUGIN_NAME = dpella_ffi_mysql

# Paths
MYSQL_INCLUDE_DIR = $(shell $(MYSQL_CONFIG) --include )
MYSQL_PLUGIN_DIR = $(shell $(MYSQL_CONFIG) --plugindir)
 
GHCVERSION := $(shell ${GHC_PATH}/ghc --numeric-version)
 
GHCDIR := $(shell find $(shell ${GHC_PATH}/ghc --print-libdir) -name HsFFI.h -exec dirname {} \;)
STUBLOC := $(shell realpath $(shell find ../../dist-newstyle/ -wholename '*/build/DPella_FFI_stub.h' -exec dirname {} \;))
LIBLOC := $(shell realpath  $(shell find ../../dist-newstyle -name '*libdpella-ffi.so.1.0.0' -exec dirname {} \;))
GHCLIBDIR := $(shell find $(shell ${GHC_PATH}/ghc --print-libdir) -name 'libHSrts-1.0.2-ghc$(GHCVERSION).so' -exec dirname {} \;)
 
# Compiler and flags
CC = gcc
CCFLAGS = -Wall -fPIC -shared -std=c99
INCLUDES = $(MYSQL_INCLUDE_DIR)  -I$(GHCDIR) -I$(STUBLOC)
LDFLAGS = -L$(LIBLOC) -l:libdpella-ffi.so.1.0.0 \
          -L$(GHCLIBDIR) -lHSrts-1.0.2-ghc$(GHCVERSION) \
          -lffi -lgmp

# Targets
.PHONY: all clean install

all: $(PLUGIN_NAME).so

$(PLUGIN_NAME).so: $(PLUGIN_NAME).c
	$(CC) $(CCFLAGS) $(INCLUDES) -o lib$@ $< $(LDFLAGS)

install: lib$(PLUGIN_NAME).so
	# This should be MYSQL_PLUGIN_DIR, but for some reason it returns a different plugin dir.
	install -m 0755 lib$(PLUGIN_NAME).so /usr/lib/mysql/plugin/

clean:
	rm -f lib$(PLUGIN_NAME).so *.o
