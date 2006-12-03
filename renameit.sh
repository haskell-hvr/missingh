#!/bin/bash

OLDNAME="$1"
NEWNAME="$2"

mod2fn () {
        echo -n "$1" | sed 's,\.,/,g'
        echo ".hs"
}

mod2destdir () {
        dirname "src/`mod2fn "$1"`"
}

OLDFN=$(mod2fn "${OLDNAME}")
NEWFN=$(mod2fn "${NEWNAME}")
NEWDIR=$(mod2destdir "${NEWNAME}")
echo "Old module name:     ${OLDNAME}"
echo "New module name:     ${NEWNAME}"
echo "Old module file:     ${OLDFN}"
echo "New module file:     ${NEWFN}"
echo "New module dir:      ${NEWDIR}"

