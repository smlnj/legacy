#!/bin/sh
#
# Copyright (c) 2019 The Fellowship of SML/NJ
#
# Pre-installation script for SML/NJ.  The purpose of this script
# is to download and unpackage files in preparation of building on
# Windows.
#

complain() {
    echo "$@"
    exit 1
}

vsay() {
    if [ x${INSTALL_DEBUG} = xtrue ] ; then
	echo "$@"
    elif [ x${INSTALL_QUIETLY} = xtrue ] ; then
	:
    else
	echo "$@"
    fi
}

this=$0

ROOT=`pwd`

#
# set the various directory and file pathname variables
#
CONFIGDIR=$ROOT/config
BASEDIR=$ROOT/base		# where the base source tree is rooted
BOOT_ARCHIVE=boot.x86-win32

#
# Function to make a directory including its ancestors.
#
makedir() {
    if [ x"$1" = x ] ; then
	:
    elif [ -d "$1" ] ; then
	:
    else
	makedirtmp=`dirname "$1"`
	makedir "$makedirtmp"
	if [ x${INSTALL_VERBOSE} = xtrue ] ; then
	    vsay "$this: Making directory $1"
	fi
	if mkdir "$1" ; then
	    :
	else
	    complain "$this: !!! Unable to make directory $1!"
	fi
    fi
}

#
# the release version that we are installing
#
VERSION=`cat "$CONFIGDIR/version"`
vsay $this: Preparing version $VERSION for Windows installation.

#
# create the base source subdirectory
#
makedir "$BASEDIR"

"$CONFIGDIR"/unpack "$ROOT" runtime
"$CONFIGDIR"/unpack "$ROOT" "$BOOT_ARCHIVE"
"$CONFIGDIR"/unpack "$ROOT" smlnj-lib
"$CONFIGDIR"/unpack "$ROOT" system

# source code for the various targets that are part of the
# standard Windows installation.
#
EXTRA_TARGETS="\
  ckit \
  cml \
  doc \
  MLRISC \
  ml-burg \
  ml-lex
  ml-lpt \
  ml-yacc \
  nlffi \
  old-basis \
  pgraph \
  trace-debug-profile \
"

for file in $EXTRA_TARGETS ; do
  "$CONFIGDIR"/unpack "$ROOT" $file
done

#
# need to make sure that the generated files have a newer timestamp
# so that the config\install.bat script works
#
touch smlnj-lib/HTML4/*.l.sml smlnj-lib/HTML4/*.g.sml

#
# remove tar files
#
rm -rf *tgz
