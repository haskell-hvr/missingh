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

SOURCES := $(wildcard MissingH/*.hs) \
	$(wildcard MissingH/*/*.hs) \
	$(wildcard MissingH/*/*/*.hs) 
LHSSOURCES := $(wildcard MissingH/*/*.lhs) \
	$(wildcard MissingH/*/*/*.lhs)
O1 := $(SOURCES:.hs=.o) $(LHSSOURCES)
OBJS := $(O1:.lhs=.o)
LHSCONVSOURCES := $(patsubst %.lhs,doctmp/%.hs,$(LHSSOURCES))
UNLIT ?= $(shell ghc --print-libdir)/unlit

.PHONY: all
all: setup
	./setup configure
	./setup build

setup: Setup.lhs MissingH.cabal
	ghc -package Cabal Setup.lhs -o setup

doctmp/%.hs: %.lhs doctmp
	mkdir -p `dirname $@`
	$(UNLIT) $< $@

doctmp:
	mkdir doctmp

#libmissingH.a: $(OBJS)
#	-rm -f libmissingH.a
#	ar q libmissingH.a $(OBJS)

#%.o: %.hs
#	ghc -O2 -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -ilibsrc --make `echo $< | sed -e s,libsrc/,, -e s,.hs$$,, -e s,/,.,g`
#
#%.o: %.lhs
#	ghc -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -ilibsrc --make `echo $< | sed -e s,libsrc/,, -e s,.lhs$$,, -e s,/,.,g`
#

.PHONY: doc
doc: $(LHSCONVSOURCES)
	-rm -rf html
	mkdir html
	haddock $(HADDOCKARGS) --package=MissingH \
	   --dump-interface=html/MissingH.haddock \
	   -t 'MissingH API Manual' -h -o html $(SOURCES) $(LHSCONVSOURCES)

clean:
	-./setup clean
	-rm -rf html `find . -name "*.o"` `find . -name "*.hi"` \
		`find . -name "*~"` *.a setup dist testsrc/runtests \
		local-pkg doctmp
	-rm -rf testtmp/*
	-cd doc && $(MAKE) clean

.PHONY: local-pkg
local-pkg: all
	echo "[" > local-pkg
	cat .installed-pkg-config >> local-pkg
	echo "]" >> local-pkg

testsrc/runtests: local-pkg $(SOURCES) $(LHSSOURCES)
	ghc6 -O2 -o testsrc/runtests -Ldist/build -odir dist/build \
           -package-conf local-pkg \
           -hidir dist/build -idist/build -itestsrc \
	   -fallow-overlapping-instances -fglasgow-exts \
	   -fallow-undecidable-instances \
                -package HUnit -package MissingH --make testsrc/runtests.hs


#testsrc/runtests: all $(shell find testsrc -name "*.hs")
#	ghc6 -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -package HUnit --make -o testsrc/runtests -itestsrc -ilibsrc testsrc/runtests.hs

test-ghc6: testsrc/runtests
	testsrc/runtests 

test-hugs:
	runhugs -98 +o -P$(PWD):$(PWD)/testsrc: testsrc/runtests.hs

interact-hugs:
	hugs -98 +o -P$(PWD):

interact-ghci: all
	ghci -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts

interact: interact-hugs

test: test-ghc6 test-hugs

