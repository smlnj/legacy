#!/bin/sh
#
# create the source tar files for a distribution.
#
# usage:
#	admin/build-tar-files.sh
#

set +x

# export all source files into the smlnj directory
#
git clone git@github.com:smlnj/legacy.git smlnj
cd smlnj
here=`pwd`

# cleanup stuff that shouldn't be in the release
#
rm -rf smlnj-lib/Dev

dirs="\
    MLRISC \
    ckit \
    cml \
    config \
    eXene \
    heap2asm \
    ml-burg \
    ml-lex \
    ml-lpt \
    ml-yacc \
    asdl \
    nlffi \
    pgraph \
    smlnj-c \
    smlnj-lib \
    trace-debug-profile \
  "

for d in $dirs ; do
  #
  # some directories require special pre-processing
  #
  case $d in
    asdl)
      cd asdl
      autoheader -Iconfig
      autoconf -Iconfig
      rm -rf autom4te.cache
      cd $here
    ;;
  esac
  #
  # build the tarball
  #
  tar -czf $here/$d.tgz $d
done

base_dirs="\
    cm \
    compiler \
    runtime \
    system \
    old-basis \
  "

cd $here/base
for d in $base_dirs ; do
  tar -czf $here/$d.tgz $d
done

# building the documentation requires configuring it and then generating the manual pages
# and HTML files
#
cd $here/doc
autoconf -Iconfig || exit 1
rm -rf autom4te.cache
./configure
#
# generate the documentation into $here/doc/doc
#
make doc || exit 1
#
# build tar file of generated documentation
#
tar -czf $here/doc.tgz doc

# TODO: Add boot files
tar -cf "$here/smlnj.tar" -C "$here" "doc"
for d in $dirs; do tar -uf "$here/smlnj.tar" -C "$here" "$d"; done
for d in $base_dirs; do tar -rf "$here/smlnj.tar" -C "$here" "base/$d"; done
gzip --stdout "$here/smlnj.tar" > "$here/smlnj.tgz"
rm -f "$here/smlnj.tar"
