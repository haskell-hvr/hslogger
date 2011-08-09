# Copyright (C) 2004 - 2011 John Goerzen <jgoerzen@complete.org>
# License: BSD3
#
all: setup
	@echo "Please use Cabal to build this package; not make."
	./setup configure
	./setup build

setup: Setup.hs
	ghc --make -o setup Setup.hs

install: setup
	./setup install

clean:
	runhaskell ./Setup.hs clean

.PHONY: test
test: test-ghc test-hugs
	@echo ""
	@echo "All tests pass."

test-hugs: setup
	@echo " ****** Running hugs tests"
	./setup configure -f buildtests --hugs
	./setup build
	runhugs -98 +o -P$(PWD)/dist/scratch:$(PWD)/dist/scratch/programs/runtests: \
		dist/scratch/programs/runtests/Main.hs

test-ghc: setup
	@echo " ****** Building GHC tests"
	./setup configure -f buildtests
	./setup build
	@echo " ****** Running GHC tests"
	./dist/build/runtests/runtests
