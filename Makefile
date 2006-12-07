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

PROJECT := MissingH
GHCPARMS := -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -cpp

.PHONY: all hugsbuild
all: setup			# GHC build
	./setup configure
	./setup build

hugsbuild: setup
	./setup configure --hugs
	./setup build

setup: Setup.hs $(PROJECT).cabal
	ghc -package Cabal Setup.hs -o setup

doctmp/%.hs: %.lhs doctmp
	mkdir -p `dirname $@`
	$(UNLIT) $< $@

doctmp:
	mkdir doctmp

.PHONY: doc
doc: $(LHSCONVSOURCES) setup
	-rm -rf html
	./setup configure
	./setup haddock
#	haddock $(HADDOCKARGS) --package=$(PROJECT) \
#	   --dump-interface=html/$(PROJECT).haddock \
#	   -t '$(PROJECT) API Manual' -h -o html $(HUGSCONVSOURCES) $(LHSCONVSOURCES)
	mv dist/doc/html .

.PHONY: hugsbuild


clean:
	-./setup clean
	-rm -rf html `find . -name "*.o"` `find . -name "*.hi"` \
		`find . -name "*~"` *.a setup dist testsrc/runtests \
		local-pkg doctmp
	-rm -rf testtmp/*

.PHONY: local-pkg
local-pkg: all
	echo "[" > local-pkg
	cat .installed-pkg-config >> local-pkg
	echo "]" >> local-pkg

testsrc/runtests: all $(wildcard testsrc/*.hs) $(wildcard testsrc/*/*.hs) $(wildcard testsrc/*/*/*.hs)
	cd testsrc && ghc --make -package FilePath -package mtl -package HUnit $(GHCPARMS) -o runtests  -i../dist/build:../src runtests.hs

test-ghc6: testsrc/runtests
	testsrc/runtests

test-hugs: 
	runhugs -98 +o "-Fcpphs --noline" -P$(PWD)/src:$(PWD)/testsrc: testsrc/runtests.hs

interact-hugs:
	hugs -98 +o -P$(PWD)/dist/build:

interact-ghci: all
	ghci -idist/build -Ldist/build $(GHCPARMS)

interact: interact-hugs

test: test-ghc6 test-hugs

