# arch-tag: Main Makefile
# Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

SOURCES := $(wildcard libsrc/MissingH/*.hs)
OBJS := $(SOURCES:.hs=.o)

all: libmissingH.a

setup: Setup.lhs Setup.description
	ghc -package Cabal Setup.lhs -o setup

libmissingH.a: $(OBJS)
	-rm -f libmissingH.a
	ar q libmissingH.a $(OBJS)

%.o: %.hs
	ghc -c -o $@ $<

doc:
	-rm -rf html
	mkdir html
	haddock -t 'MissingH API Manual' -h -o html $(SOURCES)

clean:
	-./setup clean
	-rm -rf html `find . -name "*.o"` `find . -name "*.hi"` \
		`find . -name "*~"` *.a setup dist

