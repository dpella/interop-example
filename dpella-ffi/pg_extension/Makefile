# extension name
EXTENSION = dpella-ffi-ext
# build a single shared library from multiple objects
MODULE_big = dpella-ffi-ext
# object file(s) to compile (our c source)
OBJS = dpella-ffi-ext.o

LIBLOC := $(shell realpath  $(shell find ../../dist-newstyle -name 'libdpella-ffi.so.1.0.0' -exec dirname {} \;))
GHCVERSION := $(shell ghc --numeric-version)
# adjust these paths as needed to find ghc includes and the haskell library
GHCDIR := $(shell find $(shell ghc --print-libdir) -name HsFFI.h -exec dirname {} \;)
STUBLOC := $(shell realpath $(shell find ../../dist-newstyle/ -wholename '*/DPella_FFI_stub.h' -exec dirname {} \;))
GHCLIBDIR := $(shell find $(shell ghc --print-libdir) -name 'libHSrts-1.0.2-ghc$(GHCVERSION).so' -exec dirname {} \;)
PG_CPPFLAGS = -I$(GHCDIR) -I$(STUBLOC)          # include directory for HsFFI.h

SHLIB_LINK = -L$(LIBLOC) -l:libdpella-ffi.so.1.0.0 \
             -L$(GHCLIBDIR) -lHSrts-1.0.2-ghc$(GHCVERSION) \
             -lffi -lgmp

# extension script and control file to install
DATA = dpella-ffi-ext--1.0.sql
PGXS := $(shell pg_config --pgxs)
include $(PGXS)
