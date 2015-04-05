################################################################################
.PHONEY: all install restart

################################################################################
# Set up the default target.
all::

################################################################################
CABAL_FLAGS = --enable-tests -fmaintainer
CABAL_DEP_PROFILING = -O2
CABAL_SANDBOX_DIR = ../../.cabal-sandbox
HLINT_DIRS = src example test

################################################################################
include util/haskell.mk
