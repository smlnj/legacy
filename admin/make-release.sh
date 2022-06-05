#!/bin/sh
#
# "tag" a release snapshot by copying all of the trees into a fresh release tree.
#
# Note that this version has been modified for the legacy-branch
#

if [ $# -lt 1 ] ; then
    echo Usage: $0 '<release-number>'
    exit 1
fi

relno=$1

legacy_url=https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/legacy
release_url=https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/sml/releases/release-$1

here=`pwd`

echo "tagging legacy release ..."
svn cp $legacy_url $release_url -m "Legacy release $relno"

