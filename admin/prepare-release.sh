#!/bin/sh
#
# prepare a release by following steps 1 -- 10 of the
# "Building a Release" guide at http://smlnj.org/local/build-release.html
#
# Once this script is finished running, there should be bin/boot files
# for all supported architectures in base/system.  The next step is
# updating the history file.
#
# usage:
#	admin/prepare-release.sh [-d <date>] <version>
#

THIS="$0"
HERE="$(pwd)"

usage() {
  echo "usage: admin/prepare-release.sh [-d <date>] <version>"
  exit 1
}

error_local_changes() {
  echo "$THIS: !!!There are local changes in your repository that need to be pushed."
  echo "git status"
  git status
  exit 1
}

if [ ! -x ./admin/prepare-release.sh ] ; then
  echo "$THIS: !!! must run this script from root of SML/NJ tree"
  exit 1
fi

# get the DATE and VERSION for the release
#
if [ x"$1" = x-d ] ; then
  if [ $# -ne 3 ] ; then
    usage
  fi
  DATE=$2
  VERSION=$3
elif [ $# -ne 1 ] ; then
  usage
else
  DATE=$(date +"%B %d, %Y")
  VERSION=$1
fi

# the log file
#
LOG="$HERE/LOG-$VERSION"

# determine the size of the installed version of SML that we are going to
# use to build the release.
#
if [ -x bin/sml ] ; then
  case $(bin/sml @SMLsuffix) in
    amd64-*) SZ_OPT="-64" ;;
    *) SZ_OPT="-32" ;;
  esac
else
  echo "bin/sml is missing"
  exit 1
fi

# function to compile to fixed point and build an installation from
# the compiled code.  This corresponds to steps 2 -- 6 in the guide.
#
build_from_fixpt() {
  # compile to a fixed point
  echo "compiling to a fixed point ..."
  echo "***** cd base/system" >> "$LOG"
  cd base/system || exit 1
  echo "***** ./fixpt $SZ_OPT" >> "$LOG"
  ./fixpt $SZ_OPT >> "$LOG" 2>&1 || exit 1

  # makeml
  echo "makeml ..."
  echo "***** ./makeml $SZ_OPT" >> "$LOG"
  ./makeml $SZ_OPT >> "$LOG" 2>&1 || exit 1

  # installml
  echo "installml ..."
  echo "***** ./installml $SZ_OPT -clean" >> "$LOG"
  ./installml $SZ_OPT -clean >> "$LOG" 2>&1 || exit 1

  # install.sh
  echo "config/install.sh ..."
  echo "***** cd $HERE" >> "$LOG"
  cd "$HERE" || exit 1
  echo "***** ./config/install.sh $SZ_OPT" >> "$LOG"
  ./config/install.sh $SZ_OPT >> "$LOG" 2>&1 || exit 1
}

# step 0: check for local changes that have not been added/committed
#
git diff --quiet
if [ $? != 0 ] ; then
  error_local_changes
fi
git diff --cached --quiet
if [ $? != 0 ] ; then
  error_local_changes
fi

# step 1: refresh output
#
echo "refresh sources ..."
echo "***** git pull" > "$LOG"
git pull >> "$LOG" 2>&1 || exit 1
echo "***** git push" > "$LOG"
git push >> "$LOG" 2>&1 || exit 1

# steps 2-6
build_from_fixpt

# step 7: set version and releasedate
echo "set version to $VERSION and releasedate to $DATE"
echo "***** cd config" >> "$LOG"
cd config || exit 1
echo "***** echo $VERSION > version" >> "$LOG"
echo $VERSION > version
echo "***** echo $DATE > releasedate" >> "$LOG"
echo $DATE > releasedate
echo "***** git commit -a -m \"updating version number to $VERSION\"" >> "$LOG"
git commit -a -m "updating version number to $VERSION" >> "$LOG" 2>&1 || exit 1
echo "***** git push" >> "$LOG"
git push >> "$LOG" 2>&1 || exit 1
echo "***** cd $HERE" >> "$LOG"
cd "$HERE" || exit 1

# step 8: repeat steps 2-6
build_from_fixpt

# step 9: cross compile
echo "cross compile ..."
echo "***** cd base/system" >> "$LOG"
cd base/system || exit 1
echo "***** ./allcross" >> "$LOG"
./allcross $SZ_OPT >> "$LOG" 2>&1 || exit 1
