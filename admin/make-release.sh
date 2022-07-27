#!/bin/sh
#
# "tag" a release snapshot
#
# Note that this version has been modified for the legacy-branch, which now
# lives at GitHub
#

if [ $# -lt 1 ] ; then
    echo Usage: $0 '<release-number>'
    exit 1
fi

relno=$1
reltag="v"$relno

git tag -a -m "tag release $relno" $reltag
