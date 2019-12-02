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
	ln -fs _build/default/src/bctojson.exe bctojson.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

install:
	@dune install

clean:
	@dune clean
	$(MAKE) -C src clean
	rm -fr bctojson.exe jsontoflat.exe

build-deps:
	opam install dune.1.10.0 curly ppx_deriving ppx_deriving_yojson
