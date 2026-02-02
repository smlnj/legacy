#!/bin/sh
#
# Script to build a single tarball for Unix/Linux systems on x86-64
#
# usage:
#	build-pkg.sh <version>
#

CMD="build-pkg.sh"
ROOT=$(pwd)

cleanup () {
  cd $ROOT
  rm -rf $DISTROOT
}

usage() {
  echo "usage: build-pkg.sh <version>"
  exit $1
}

complain() {
  echo "$CMD [Error]: $@"
  exit 1
}

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
    if mkdir "$1" ; then
      :
    else
      complain "$CMD: !!! Unable to make directory $1!"
    fi
  fi
}

SIZE="64"
ARCH="amd64"

# process command-line arguments
#
if [ "$#" != "1" ] ; then
  usage
fi

VERSION=$1

CONFIGURL=http://smlnj.cs.uchicago.edu/dist/working/$VERSION/config.tgz
DISTROOT=smlnj
CONFIGDIR=config
BASEDIR=base
BOOT_ARCHIVE=boot.amd64-unix

# create a directory for the build
#
if [ -d "$DISTROOT" ] ; then
  complain "please remove "$DISTROOT" first"
fi
mkdir "$DISTROOT"
cd "$DISTROOT"

# first we need to download and unbundle the config directory for the release
#
curl -s -S -O $CONFIGURL
tar -xzf config.tgz
rm config.tgz
if [ "$?" != 0 ] ; then
  # note that if config.tgz does not exist, curl will still work (it will get a
  # 404 page from the server)
  cleanup
  complain "unable to download/unpack config.tgz"
fi

# check that the version numbers match
#
if [ ! -r config/version ] ; then
  cleanup
  complain "config/version is missing"
fi
CONFIG_VERSION=$(cat config/version)
if [ x"$VERSION" != x"$CONFIG_VERSION" ] ; then
  cleanup
  complain "version in config/version is $CONFIG_VERSION"
fi

#
# create the base source subdirectory
#
makedir "$BASEDIR"

"$CONFIGDIR"/unpack . runtime
"$CONFIGDIR"/unpack . "$BOOT_ARCHIVE"
"$CONFIGDIR"/unpack . smlnj-lib
"$CONFIGDIR"/unpack . system
"$CONFIGDIR"/unpack . old-basis

rm -f "$BASEDIR"/runtime.tgz "$BASEDIR"/smlnj-lib.tgz "$BASEDIR"/system.tgz "$BASEDIR"/old-basis.tgz

# source code for the various targets that are part of the
# standard installation.
#
TARGETS="\
  asdl \
  ckit \
  cml \
  doc \
  MLRISC \
  ml-burg \
  ml-lex
  ml-lpt \
  ml-yacc \
  nlffi \
  pgraph-util \
  tdp-util \
"

for file in $TARGETS ; do
  "$CONFIGDIR"/unpack . $file
done

# remove tarballs (except for the boot files)
for file in *.tgz ; do
  if [ x"$file" != x"$BOOT_ARCHIVE".tgz ] ; then
    rm $file
  fi
done

# package up the source tree as a compressed tar file
#
cd $ROOT
tar -czf smlnj-$ARCH-unix-$VERSION.tgz smlnj

cleanup
