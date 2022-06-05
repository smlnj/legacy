#!/bin/sh
#
# COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Script to check the svn status of the various working copies
#
# Usage:	admin/stat-all.sh [-q]
#

QUIET=""
if [ $# -gt 2 ] ; then
  usage
elif [ x"$1" = "x-q" ] ; then
  QUIET=-q
elif [ $# -eq 1 ] ; then
  usage
fi

here=`pwd`

for tree in admin config base smlnj-lib MLRISC ml-yacc trace-debug-profile pgraph \
            ckit cml eXene smlnj-c ml-burg ml-lex heap2asm nlffi \
	    ml-lpt doc asdl smldoc
do
  if [ -d $tree -a -d $tree/.svn ] ; then
      echo Checking ${tree}...
      cd $tree
      svn status $QUIET
      cd $here
  fi
done
