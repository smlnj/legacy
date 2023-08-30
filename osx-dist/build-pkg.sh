#!/bin/sh
#
# Script to build the installer package for x86/amd64 on Mac OS X (10.7+)
#
# usage:
#	build-pkg.sh [-32 | -64] <version>
#

CMD="build-pkg.sh"
ROOT=$(pwd)

cleanup () {
  cd $ROOT
  rm -rf $RSRC $DISTROOT smlnj.pkg
}

usage() {
  echo "usage: build-pkg.sh [-32 | -64] [--no-sign] <version>"
  exit $1
}

complain() {
    echo "$CMD [Error]: $@"
    exit 1
}

SIZE="64"
ARCH="amd64"
SIGNER="$USER"
VERSION=""

# process command-line arguments
#
while [ "$#" != "0" ] ; do
    arg=$1; shift
    case $arg in
    -32) SIZE=32 ; ARCH="x86" ;;
    -64) SIZE=64 ; ARCH="amd64" ;;
    --no-sign) SIGNER="none" ;;
    -*) usage 1 ;;
    *) VERSION=$arg ; break ;;
    esac
done

if [ x"$VERSION" = x ] ; then
  usage 1
fi
if [ "$#" != "0" ] ; then
  usage 1
fi

CONFIGURL=http://smlnj.cs.uchicago.edu/dist/working/$VERSION/config.tgz
DISTROOT=smlnj.dst
ID=org.smlnj.$ARCH.pkg
RSRC=Resources

# you need a developer ID to sign the final package;
#
case x"$SIGNER" in
  xjhr) SIGN="Developer ID Installer: John Reppy" ;;
  xnone) SIGN=none ;;
  *)
    echo "$CMD [Warning]: unknown user, so package will not be signed!"
    SIGN=none
  ;;
esac

if [ -d $DISTROOT ] ; then
  complain "please remove $DISTROOT first"
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

# build the distribution (note that this assumes that config/targets is what we want!)
#
config/install.sh -default $SIZE
if [ "$?" != 0 ] ; then
  cleanup
  complain "problem building SML/NJ"
fi

# get the other files to include in the distribution
#
cp -p $ROOT/components/license.html .
#
# as of 110.78, README is included in doc/html/readme/
#svn export https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/sml/trunk/READMES/$VERSION-README.html

# remove the tarballs
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
sed -e "s/VERSION/$VERSION/g" -e "s/ARCH/$ARCH/g" components/distribution_xml.in > $RSRC/distribution.xml
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
SCRIPTS_DIR="components/scripts"$SIZE"/"
PKG_OPTS="--identifier $ID --version $VERSION --scripts $SCRIPTS_DIR \
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

cleanup
