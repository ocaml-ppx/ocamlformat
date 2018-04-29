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

OCAMLDOT=ocamldot

.PHONY: default
default: exe

jbuild-workspace: jbuild-workspace.in
	sed -e "s|@OPAM_SWITCH[@]|$$(opam switch show)|g" $< > $@

.PHONY: setup
setup: jbuild-workspace

.PHONY: exe
exe: setup
	jbuilder build src/ocamlformat.exe

.PHONY: bc
bc: setup
	jbuilder build _build/dbg/src/ocamlformat.bc

.PHONY: dbg
dbg: setup
	jbuilder build _build/dbg/src/ocamlformat.exe

.PHONY: opt
opt: setup
	jbuilder build _build/default/src/ocamlformat.exe

.PHONY: reason
reason: setup
	jbuilder build src/ocamlformat_reason.exe

.PHONY: install
install:
	jbuilder build -p ocamlformat,ocamlformat_reason,ocamlformat_support

SRCS=$(shell \ls src/{,import/}*.ml{,i})

mod_dep.dot: $(SRCS)
	ocamldep.opt $(SRCS) | $(OCAMLDOT) -r Ocamlformat -fullgraph > mod_dep.dot

.PHONY: clean cleanbisect
clean: cleanbisect
	rm -rf _build jbuild-workspace
cleanbisect:
	rm -Rf _coverage
	find ./ -name 'bisect*.out' -delete

.PHONY: fmt
fmt:
	$(shell \ls -t _build/*/src/ocamlformat.{exe,bc} | head -1) -i $(SRCS)

.PHONY: test
test: exe reason
	$(MAKE) fixpoint
	$(MAKE) regtests

.PHONY: regtests fixpoint
fixpoint: exe reason
#       check fix-point
	_build/default/src/ocamlformat.exe -n 1 -i $(SRCS)

regtests: exe reason dbg
#       check regtests
	$(MAKE) -C test regtests

.PHONY: coverage
coverage: setup
	jbuilder build _build/coverage/src/ocamlformat.exe
	$(MAKE) cleanbisect
	$(MAKE) MODE=coverage -C test regtests
	_build/coverage/src/ocamlformat.exe -i $(SRCS)
	bisect-ppx-report -I _build/coverage/ -html _coverage/ `find test -name 'bisect*.out'`
	@echo "open _coverage/index.html"
