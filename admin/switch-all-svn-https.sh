#!/bin/sh

r=`pwd`

switchone(){
    d=$1
    u=`svn info | grep '^URL:' | sed 's/URL: //'`
    case $u in
	svn://smlnj-gforge*)
	    n=`echo $u | sed 's/svn:\/\/smlnj-gforge\.cs\.uchicago\.edu/https:\/\/smlnj-gforge\.cs\.uchicago\.edu\/svn/'`
	    echo $d '->' $n
	    svn switch --relocate $u $n
	    ;;
	*)
	    echo Will not switch $d "($u)".
	    ;;
    esac
}

if [ -d .svn ] ; then
    switchone .
else
    for d in * ; do
	if [ -d $d/.svn ] ; then
	    cd $d
	    switchone $d
	    cd $r
	fi
    done
fi

exit 0
