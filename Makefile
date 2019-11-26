# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build bctojson merlin

build:
	@dune build

bctojson:
	$(MAKE) -C src bctojson.exe
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
	rm -fr bctojson.exe

build-deps:
	opam install dune.1.10.0 ppx_deriving ppx_deriving_yojson
