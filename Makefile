# arch-tag: Main Makefile
# Copyright (C) 2004 - 2005 John Goerzen <jgoerzen@complete.org>
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

SOURCES := $(wildcard libsrc/MissingH/*.hs) \
	$(wildcard libsrc/MissingH/*/*.hs) \
	$(wildcard libsrc/MissingH/*/*/*.hs) 
LHSSOURCES := $(wildcard libsrc/MissingH/*/*.lhs) \
	$(wildcard libsrc/MissingH/*/*/*.lhs)
O1 := $(SOURCES:.hs=.o) $(LHSSOURCES)
OBJS := $(O1:.lhs=.o)

all: libmissingH.a

setup: Setup.lhs MissingH.cabal
	ghc -package Cabal Setup.lhs -o setup

libmissingH.a: $(OBJS)
	-rm -f libmissingH.a
	ar q libmissingH.a $(OBJS)

%.o: %.hs
	ghc -O2 -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -ilibsrc --make `echo $< | sed -e s,libsrc/,, -e s,.hs$$,, -e s,/,.,g`

%.o: %.lhs
	ghc -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -ilibsrc --make `echo $< | sed -e s,libsrc/,, -e s,.lhs$$,, -e s,/,.,g`

doc:
	-rm -rf html
	mkdir html
	haddock $(HADDOCKARGS) --package=MissingH \
	   --dump-interface=html/MissingH.haddock \
	   -t 'MissingH API Manual' -h -o html $(SOURCES)

clean:
	-./setup clean
	-rm -rf html `find . -name "*.o"` `find . -name "*.hi"` \
		`find . -name "*~"` *.a setup dist testsrc/runtests
	-cd doc && $(MAKE) clean

testsrc/runtests: all $(shell find testsrc -name "*.hs")
	ghc6 -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -package HUnit --make -o testsrc/runtests -itestsrc -ilibsrc testsrc/runtests.hs

test-ghc6: testsrc/runtests
	testsrc/runtests 

test-hugs:
	runhugs -98 +o -P$(PWD)/libsrc:$(PWD)/testsrc: testsrc/runtests.hs

interact-hugs:
	hugs -98 +o -P$(PWD)/libsrc:

interact-ghci: all
	ghci -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -ilibsrc

interact: interact-hugs

test: test-ghc6 test-hugs

