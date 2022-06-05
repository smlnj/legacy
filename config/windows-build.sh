#!/bin/sh
#
# Copyright (c) 2014 The Fellowship of SML/NJ
#
# Build script for SML/NJ and related tools on Windows
# using cygwin as a shell.
#

export SMLNJ_WINDOWS_RUNTIME
SMLNJ_WINDOWS_RUNTIME=yes

case `uname -s` in
  CYGWIN_NT*) ;;
  *) echo "Unknown OS; the windows-build.sh script should be run on 32-bit cygwin"
    exit 1 ;;
esac

config/install.sh
