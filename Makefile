# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build exec merlin

build:
	@dune build

exec:
	$(MAKE) -C src all
	ln -fs _build/default/src/jsontoflat.exe jsontoflat.exe
	ln -fs _build/default/src/bctodb.exe bctodb.exe
	ln -fs _build/default/src/sample.exe sample.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

install:
	@dune install

clean:
	@dune clean
	$(MAKE) -C src clean
	rm -fr bctodb.exe jsontoflat.exe sample.exe

build-deps:
	opam install dune.1.10.0 curly sqlite3 ppx_deriving ppx_deriving_yojson
