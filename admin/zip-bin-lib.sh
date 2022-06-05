#!/bin/sh

target=smlnj.zip

if [ $# -gt 0 ] ; then
    target=$1
    shift
fi

zip -A -r ${target} bin lib
