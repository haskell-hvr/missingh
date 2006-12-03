#!/bin/bash

set -e

OLDNAME="$1"
NEWNAME="$2"

mod2fn () {
        echo -n "$1" | sed 's,\.,/,g'
        echo ".hs"
}

mod2destdir () {
        dirname "src/`mod2fn "$1"`"
}

sedname () {
        echo "$1" | sed 's/\./\\./g' 
}

OLDFN=$(mod2fn "${OLDNAME}")
NEWFN=src/$(mod2fn "${NEWNAME}")
NEWDIR=$(mod2destdir "${NEWNAME}")
echo "Old module name:     ${OLDNAME}"
echo "New module name:     ${NEWNAME}"
echo "Old module file:     ${OLDFN}"
echo "New module file:     ${NEWFN}"
echo "New module dir:      ${NEWDIR}"

mkdir -p ${NEWDIR}
darcs add --recursive src || true
darcs mv ${OLDFN} ${NEWFN}

for FILE in \
        MissingH.cabal \
        `find src -name "*.hs"` \
        `find src -name "*.lhs"` \
        `find MissingH -name "*.hs"` \
        `find MissingH -name "*.lhs"` \
        `find testsrc -name "*.hs"` 
do
        sed -i "s,`sedname ${OLDNAME}`,${NEWNAME},g" $FILE
done

darcs record -m "Renamed ${OLDNAME} to ${NEWNAME}"
           

