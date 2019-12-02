# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev

# --------------------------------------------------------------------
all: bctojson jsontoflat .merlin

run: __force__
	dune exec ./$(COMPILER)

bctojson: __force__
	dune build --profile=$(PROFILE) bctojson.exe

jsontoflat: __force__
	dune build --profile=$(PROFILE) jsontoflat.exe

clean: __force__
	dune clean

merlin: .merlin

# --------------------------------------------------------------------
%.exe: __force__
	dune build --profile=$(PROFILE) $@

%.cmxs: __force__
	dune build --profile=$(PROFILE) $@

.merlin: __force__
	dune build --profile=$(PROFILE) $@
