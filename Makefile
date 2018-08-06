######################################################################
#                                                                    #
#                            OCamlFormat                             #
#                                                                    #
#  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  #
#                                                                    #
#  This source code is licensed under the MIT license found in the   #
#  LICENSE file in the root directory of this source tree.           #
#                                                                    #
######################################################################

SHELL=bash

.PHONY: default
default: exe

dune-workspace: dune-workspace.in
	sed -e "s|@OPAM_SWITCH[@]|$$(opam switch show)|g" $< > $@

.PHONY: setup
setup: dune-workspace

.PHONY: exe
exe: setup
	dune build _build/dev/src/ocamlformat.exe _build/release/src/ocamlformat.exe

.PHONY: bc
bc: setup
	dune build _build/dev/src/ocamlformat.bc

.PHONY: dev
dev: setup
	dune build _build/dev/src/ocamlformat.exe

.PHONY: opt
opt: setup
	dune build _build/release/src/ocamlformat.exe

.PHONY: clean cleanbisect
clean: cleanbisect
	rm -rf _build dune-workspace
cleanbisect:
	rm -Rf _coverage
	find ./ -name 'bisect*.out' -delete

SRCS=$(shell \ls src/{,import/}*.ml{,i})

.PHONY: fmt
fmt:
	$(shell \ls -t _build/*/src/ocamlformat.{exe,bc} | head -1) -i $(SRCS)

.PHONY: test
test: exe
	$(MAKE) fixpoint
	$(MAKE) regtests

.PHONY: regtests fixpoint
fixpoint: exe
	_build/release/src/ocamlformat.exe -n 1 -i $(SRCS)

regtests: exe
	$(MAKE) -C test regtests

.PHONY: coverage
coverage: setup
	dune build _build/coverage/src/ocamlformat.exe
	$(MAKE) cleanbisect
	$(MAKE) MODE=coverage -C test regtests
	_build/coverage/src/ocamlformat.exe -i $(SRCS)
	bisect-ppx-report -I _build/coverage/ -html _coverage/ `find test -name 'bisect*.out'`
	@echo "open _coverage/index.html"
