#!/bin/sh
#
# COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Script to checkout a fresh copy of the SML/NJ sources.
#
# Usage:
#	admin/checkout-all.sh [options] [dir]
#
# The options are
#
#	-e, --export	-- use "svn export" instead of "svn checkout"
#	-r ARG		-- use ARG as the checkout revision
#	--release ARG	-- checkout a release that was created with make-release.sh
#  	--force		-- overwrite existing source trees
#

command=checkout
revision=""
release=""
force=no

usage() {
  echo "usage: checkout-all.sh [-e | --export] [-r ARG | --revision ARG] [--release ARG] [dir]"
  exit $1
}

while [ $# -ge 1 ] ; do
  case $1 in
    --export|-e)
	command=export
    ;;
    --revision|-r)
	shift
	if [ $# -ge 1 ] ; then
	  revision="-r $1"
	else
	  usage 1
	fi
    ;;
    --release)
	shift
	if [ $# -ge 1 ] ; then
	  release="releases/release-$1"
	else
	  usage 1
	fi
    ;;
    --help|-h) usage 0
    ;;
    --force)
	force=yes
    ;;
    -*) usage 1
    ;;
  esac
  shift
done

if [ x"$release" != x -a x"$revision" != x ] ; then
  echo "checkout-all.sh: cannot specify both a release and revision"
  exit 1
fi

if [ $# -ge 1 ] ; then
    mkdir $1
    cd $1
fi

# default release is the trunk
#
if [ x"$release" = x ] ; then
  release="trunk"
fi

gf=https://smlnj-gforge.cs.uchicago.edu/svn
smlnj=$gf/smlnj

# checkout source target
checkout(){
    source=$1/$release
    target=$2
    if [ ! -d $target ] ; then
	echo "svn $command $revision $source $target"
	svn $command $revision $source $target
    elif [ $force = yes ] ; then
	echo Replacing existing $target tree
	rm -rf $target
	echo "svn $command $revision $source $target"
	svn $command $revision $source $target
    else
	echo Tree $target already exists.
    fi
}

# checkout from smlnj tree
smlnj(){
    checkout $smlnj/$1 $1
}

smlnjbase(){
    checkout $smlnj/$1 $2
}

smlnj config
smlnjbase sml base
smlnj smlnj-lib
smlnj MLRISC
smlnj ml-yacc
smlnj trace-debug-profile
smlnj pgraph

smlnj ckit
smlnj cml
smlnj eXene
smlnj smlnj-c
smlnj ml-burg
smlnj ml-lex
smlnj heap2asm
smlnj nlffi

smlnj doc

# the following do not get tagged releases, so we fallback to regular
# behavior
release="trunk"
revision=""
checkout $gf/asdl asdl
checkout $gf/ml-lpt ml-lpt
checkout $gf/smldoc smldoc
