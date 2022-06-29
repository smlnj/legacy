#!/bin/sh
#
# Script to fetch the distribution files for building the Windows
# version of SML/NJ
#
# usage:
#	fetch-dist.sh [-32 | -64] <version>
#
# NOTE currently only 32-bit builds are supported
#

CMD="fetch-dist.sh"

complain() {
    echo $CMD: "$@"
    exit 1
}

SIZE="32"
ARCH="x86"
if [ x"$1" = x-32 ] ; then
  SIZE=32
  ARCH="x86"
  OPSYS="win32"
  shift
elif [ x"$1" = x-64 ] ; then
  SIZE=64
  ARCH="amd64"
  OPSYS="win64"
  shift
fi

# we currently only support 32-bit builds
#
if [ $SIZE = "64" ] ; then
  complain only 32-bit builds are currently supported for Windows
  exit 1
fi

# get the version number
#
if [ $# != 1 ] ; then
  echo "usage: $CMD [-32 | -64] <version>"
  exit 1
fi
VERSION=$1

CONFIGURL=http://smlnj.cs.uchicago.edu/dist/working/$VERSION/config.tgz

#
# set the various directory and file pathname variables
#
ROOT=$(pwd)
DISTROOT=$ROOT/smlnj		# where we build the distribution
CONFIGDIR=$DISTROOT/config
BASEDIR=$DISTROOT/base		# where the base source tree is rooted
BOOT_ARCHIVE=boot.$ARCH-$OPSYS

# switch to the $DISTROOT
#
if [ -d $DISTROOT ] ; then
  complain "please remove $DISTROOT first"
fi
mkdir $DISTROOT
cd $DISTROOT

# the files that we need to download
#
# first we need to download and unbundle the config directory for the release
#
curl -s -S -O $CONFIGURL
tar -xzf config.tgz
if [ "$?" != 0 ] ; then
  # note that if config.tgz does not exist, curl will still work (it will get a
  # 404 page from the server)
  cd $ROOT
  rm -rf $DISTROOT
  complain "unable to download/unpack config.tgz"
fi

# check that the version numbers match
#
if [ ! -r config/version ] ; then
  echo "$CMD [Error]: config/version is missing"
  exit 1
fi
CONFIG_VERSION=$(cat config/version)
if [ x"$VERSION" != x"$CONFIG_VERSION" ] ; then
  complain "version in config/version is $CONFIG_VERSION"
  cd $ROOT
  rm -rf $DISTROOT
  exit 1
fi

#
# create the base source subdirectory
#
mkdir "$BASEDIR"

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
