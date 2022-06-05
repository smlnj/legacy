#!/bin/sh
#
# Script to run "svn update" on all of the repositories of a normal SML/NJ
# checkout (e.g., one from admin/checkout-all.sh).
#
# usage: admin/svn-upgrade-all.sh
#

# checkout source target
upgrade() {
    if [ -d $1 ] ; then
	echo "checking $1 for upgrade"
	svn upgrade $1
    fi
}

upgrade admin
upgrade config
upgrade base
upgrade smlnj-lib
upgrade MLRISC
upgrade ml-yacc
upgrade trace-debug-profile
upgrade pgraph

upgrade ckit
upgrade cml
upgrade eXene
upgrade smlnj-c
upgrade ml-burg
upgrade ml-lex
upgrade heap2asm
upgrade nlffi

upgrade asdl
upgrade ml-lpt
upgrade smldoc

upgrade doc
