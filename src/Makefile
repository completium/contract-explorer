# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev

# --------------------------------------------------------------------
all: bctodb jsontoflat sample .merlin

run: __force__
	dune exec ./$(COMPILER)

bctodb: __force__
	dune build --profile=$(PROFILE) bctodb.exe

jsontoflat: __force__
	dune build --profile=$(PROFILE) jsontoflat.exe

sample: __force__
	dune build --profile=$(PROFILE) sample.exe

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
