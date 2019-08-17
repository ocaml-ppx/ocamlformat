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
default: exe gen-help

dune-workspace: dune-workspace.in
	sed -e "s|@OPAM_SWITCH[@]|$$(opam switch show)|g" $< > $@

.PHONY: setup
setup: dune-workspace

.PHONY: exe
exe: setup
	dune build _build/dev/src/ocamlformat.exe _build/release/src/ocamlformat.exe

.PHONY: gen-help
gen-help:
	dune build _build/release/ocamlformat-help.txt

.PHONY: reason
reason: setup
	dune build _build/dev/src/ocamlformat_reason.exe _build/release/src/ocamlformat_reason.exe

.PHONY: ocamlformat-diff
ocamlformat-diff:
	dune build _build/dev/tools/ocamlformat-diff/ocamlformat_diff.exe _build/release/tools/ocamlformat-diff/ocamlformat_diff.exe

.PHONY: clean cleanbisect
clean: cleanbisect
	rm -rf _build dune-workspace
cleanbisect:
	rm -Rf _coverage
	find ./ -name 'bisect*.out' -delete

SRCS=$(shell \find src tools -name '[^.]*.ml' -or -name '[^.]*.mli' -or -name '[^.]*.mlt')

.PHONY: fmt
fmt:
	dune exec --context dev -- ocamlformat -i $(SRCS)

.PHONY: test regtests fixpoint test-reason
test: fixpoint regtests

fixpoint: setup
	dune exec --context dev -- ocamlformat -n 1 -i $(SRCS)

regtests: setup
	dune build @_build/dev/test/runtest

test-reason: setup
	dune build @_build/dev/test/reason/runtest-reason --auto-promote

.PHONY: coverage
coverage: setup cleanbisect
	dune build @_build/coverage/test/runtest
	dune exec --context coverage -- ocamlformat -i $(SRCS)
	bisect-ppx-report -I _build/coverage/ -html _coverage/ `find test -name 'bisect*.out'`
	@echo "open _coverage/index.html"
