#!/bin/sh
#
# Copyright (c) 2018 The Fellowship of SML/NJ
#
# Script for downloading and unpacking SML/NJ sources without building
# the runtime, sml executable, or libraries.  The main purpose of this script
# is to support installation on Windows.
#
# usage: config/download.sh <arch> <os>
#
#	<arch>	the architecture for the boot files
#
#	<os>	the operating system (win32 or unix)
#

here=`pwd`
path_to_me=`dirname $0`

CONFIGDIR=$here/config

complain() {
    echo "$@"
    exit 1
}

if [ $# -ne 2 ] ; then
  complain "usage: config/download.sh <arch> <os>"
fi
ARCH=$1
OS=$2
BOOT_ARCHIVE=boot.$ARCH-$OS

export VERSION=`cat "$CONFIGDIR/version"`

# extract list of targets
#
TARGETS=$(grep ^request $CONFIGDIR/targets | sed -e 's/request \(.*\)/\1/')
if [ x"$TARGETS" = x ] ; then
  complain "empty target list"
fi

# determine list of files to download from targets
#
DOWNLOADS="$BOOT_ARCHIVE"
for tgt in $TARGETS ; do
  case $tgt in
    src-smlnj)		DOWNLOADS="$DOWNLOADS compiler.tgz" ;;
    ml-ulex)		DOWNLOADS="$DOWNLOADS ml-lpt" ;;
    ml-ulex-mllex-tool) DOWNLOADS="$DOWNLOADS ml-lpt" ;;
    ml-lex-mllex-tool)	DOWNLOADS="$DOWNLOADS ml-lex" ;;
    ml-lex-lex-ext)	DOWNLOADS="$DOWNLOADS ml-lex" ;;
    ml-yacc-grm-ext)	DOWNLOADS="$DOWNLOADS ml-yacc" ;;
    ml-antlr)		DOWNLOADS="$DOWNLOADS ml-lpt" ;;
    ml-antlr-grm-ext)	DOWNLOADS="$DOWNLOADS ml-lpt" ;;
    ml-lpt-lib)		DOWNLOADS="$DOWNLOADS ml-lpt" ;;
    pgraph-util)	DOWNLOADS="$DOWNLOADS pgraph" ;;
    tdp-util)		DOWNLOADS="$DOWNLOADS trace-debug-profile" ;;
    cml-lib)		DOWNLOADS="$DOWNLOADS cml" ;;
    mlrisc)		DOWNLOADS="$DOWNLOADS MLRISC" ;;
    mlrisc-tools)	DOWNLOADS="$DOWNLOADS MLRISC" ;;
    ml-nlffi-lib)	DOWNLOADS="$DOWNLOADS nlffi" ;;
    ml-nlffigen)	DOWNLOADS="$DOWNLOADS nlffi" ;;
    nowhere)		DOWNLOADS="$DOWNLOADS MLRISC" ;;
    *) DOWNLOADS="$DOWNLOADS $tgt" ;;
  esac
done

#
# the URL for the (usually remote) source archive
#
. "$CONFIGDIR"/srcarchiveurl

echo Downloading version $VERSION from $SRCARCHIVEURL

#
# download the files
#
for d in $DOWNLOADS ; do
  if [ ! -d $d ] ; then
    echo "get $d"
    tarfile=$d.tgz
    curl -O $SRCARCHIVEURL/$tarfile
    if [ $? -ne 0 ] ; then
      complain "unable to download $SRCARCHIVEURL/$tarfile"
    fi
    tar -xzf $tarfile
    rm $tarfile
  fi
done
