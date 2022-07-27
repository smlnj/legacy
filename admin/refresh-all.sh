#!/bin/sh
#
# update sources
#

if [ $# -ge 1 ] ; then
    cd $1
fi

git pull
