#!/bin/sh

propfile=`mktemp /tmp/propXXXXX`
trap 'rm -f ${propfile}' 0 1 2 3 15

revisionOf(){
    svn info $1 | grep '^Revision:' | sed 's/^Revision:[ \t]*\([0-9]*\)$/\1/'
}

procPropLine(){
    while read f1 f2 f3 ; do
	if [ x$f1 != x ] ; then
	    case $f2 in
		-r*)
		    echo $f1 $f2 $f3
		    ;;
		*)
		    rev=`revisionOf $f2`
		    echo $f1 -r${rev} $f2
		    ;;
	    esac
	fi
    done
}

if [ $# != 2 ] ; then
    echo usage: $0 from to
    exit 1
fi

fromdir="$1"
todir="$2"

svn cp $fromdir $todir

svn propget svn:externals $fromdir | procPropLine >$propfile

svn propset svn:externals --file $propfile --quiet $todir

exit 0
