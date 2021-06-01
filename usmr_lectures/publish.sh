#!/bin/bash

HTMLSUF=.html
FILESUF=_files

if [ -n "$1" ]
then
    filename="$1"
else
    exit 1
fi

basename=${filename%.html}
filename="$basename$HTMLSUF"
dirname="$basename$FILESUF"

/usr/bin/rsync -r "$filename" ../docs/lectures/
/usr/bin/rsync -r "$dirname"  ../docs/lectures/
/usr/bin/rsync -r libs        ../docs/lectures/
/usr/bin/rsync -r mc_libs     ../docs/lectures/
/usr/bin/rsync -r xaringan-themer.css ../docs/lectures/

exit 0
