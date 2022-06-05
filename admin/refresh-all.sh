#!/bin/sh

if [ $# -ge 1 ] ; then
    cd $1
fi

here=`pwd`

for tree in admin config base smlnj-lib MLRISC ml-yacc trace-debug-profile pgraph \
            ckit cml eXene smlnj-c ml-burg ml-lex heap2asm nlffi \
            asdl ml-lpt smldoc doc dev-notes
do
  if [ -d $tree -a -d $tree/.svn ] ; then
      echo Refreshing ${tree}...
      cd $tree
      svn update
      cd $here
  fi
done
