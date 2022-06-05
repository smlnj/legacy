#!/bin/sh
#
# Script to build the installer package for x86/amd64 on Mac OS X (10.7+)
#
# usage:
#	build-pkg.sh [-32 | -64] <version>
#

CMD="build-pkg.sh"

SIZE="64"
ARCH="amd64"
if [ x"$1" = x-32 ] ; then
  SIZE=32
  ARCH="x86"
  shift
elif [ x"$1" = x-64 ] ; then
  SIZE=64
  ARCH="amd64"
  shift
fi

# get the version number
#
if [ $# != 1 ] ; then
  echo "usage: $CMD [-32 | -64] version"
  exit 1
fi
VERSION=$1

CONFIGURL=http://smlnj.cs.uchicago.edu/dist/working/$VERSION/config.tgz
DISTROOT=smlnj.dst
ID=org.smlnj.$ARCH.pkg
ROOT=$(pwd)
RSRC=Resources

# you need a developer ID to sign the final package;
#
case x"$USER" in
  xjhr) SIGN="Developer ID Installer: John Reppy" ;;
  *)
    echo "$CMD [Warning]: unknown user, so package will not be signed!"
    SIGN=none
  ;;
esac

if [ -d $DISTROOT ] ; then
  echo "$CMD [Error]: please remove $DISTROOT first"
  exit 1
fi
mkdir $DISTROOT
cd $DISTROOT

# first we need to download and unbundle the config directory for the release
#
curl -s -S -O $CONFIGURL
tar -xzf config.tgz
if [ "$?" != 0 ] ; then
  # note that if config.tgz does not exist, curl will still work (it will get a
  # 404 page from the server)
  echo "$CMD [Error]: unable to download/unpack config.tgz"
  cd $ROOT
  rm -rf $DISTROOT
  exit 1
fi

# check that the version numbers match
#
if [ ! -r config/version ] ; then
  echo "$CMD [Error]: config/version is missing"
  exit 1
fi
CONFIG_VERSION=$(cat config/version)
if [ x"$VERSION" != x"$CONFIG_VERSION" ] ; then
  echo "$CMD [Error]: version in config/version is $CONFIG_VERSION"
  cd $ROOT
  rm -rf $DISTROOT
  exit 1
fi

# build the distribution (note that this assumes that config/targets is what we want!)
#
config/install.sh -default $SIZE
if [ "$?" != 0 ] ; then
  echo "$CMD [Error]: problem building SML/NJ"
  exit 1
fi

# get the other files to include in the distribution
#
cp -p $ROOT/components/license.html .
#
# as of 110.78, README is included in doc/html/readme/
#svn export https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/sml/trunk/READMES/$VERSION-README.html

# cleanup
#
rm *tgz

# back up to the root
#
cd $ROOT

# TODO: here is probably where we should use codesign to enable the hardened runtime
# for the runtime executable.  Something like the following command:
#
#    codesign --force --options runtime --sign $SIGN bin/.run/run.amd64-darwin
#
# we may also need to add the --entitlements flag to enable things like executing
# the code that we generate

# create the resources directory and fill it
#
if [ -d $RSRC ] ; then
  rm -rf $RSRC
fi
mkdir $RSRC
sed -e "s/VERSION/$VERSION/g" components/distribution_xml.in > $RSRC/distribution.xml
cp -p components/smlnj-background.jpg $RSRC/background.jpg
sed -e "s/VERSION/$VERSION/g" components/welcome_html.in > $RSRC/welcome.html
cp -p components/license.html $RSRC/license.html
cp -p components/conclusion.html $RSRC/conclusion.html

# copy the readme, while adjusting the fontsize for the installer panel
# NOTE: this command relies on the fact that there is only one absolute
# font-size command in the README file (the others are relative)
#
sed -E 's/font-size: [0-9]+pt;/font-size: 9pt;/' \
  $DISTROOT/doc/html/readme/$VERSION-README.html > $RSRC/readme.html

# build package
#
PKG_OPTS="--identifier $ID --version $VERSION --scripts components/scripts/ \
  --install-location /usr/local/smlnj --root $DISTROOT"
pkgbuild $PKG_OPTS smlnj.pkg

# build distribution package
#
BUILD_OPTS="--package-path components --resources $RSRC \
  --distribution $RSRC/distribution.xml ./smlnj-$ARCH-$VERSION.pkg"
if [ x"$SIGN" = xnone ] ; then
  echo "$CMD: building unsigned package smlnj-$ARCH-$VERSION.pkg"
  productbuild $BUILD_OPTS
else
  echo "$CMD: building signed package smlnj-$ARCH-$VERSION.pkg"
  productbuild --sign "$SIGN" $BUILD_OPTS
fi

# cleanup
#
rm -rf $RSRC $DISTROOT smlnj.pkg
