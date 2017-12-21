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

JBFLAGS= -j $(shell opam config var jobs)
OCAMLDOT=ocamldot

.PHONY: default
default: exe

jbuild-workspace: jbuild-workspace.in
	sed -e "s|@OPAM_SWITCH[@]|$$(opam switch show)|g" $< > $@

.PHONY: setup
setup: jbuild-workspace

.PHONY: exe
exe: setup
	jbuilder build $(JBFLAGS) src/ocamlformat.exe

.PHONY: bc
bc: setup
	jbuilder build $(JBFLAGS) _build/dbg/src/ocamlformat.bc

.PHONY: dbg
dbg: setup
	jbuilder build $(JBFLAGS) _build/dbg/src/ocamlformat.exe

.PHONY: opt
opt: setup
	jbuilder build $(JBFLAGS) _build/default/src/ocamlformat.exe

.PHONY: reason
reason: setup
	jbuilder build $(JBFLAGS) src/ocamlformat_reason.exe

.PHONY: install
install:
	jbuilder build $(JBFLAGS) -p ocamlformat,ocamlformat_reason,ocamlformat_support

SRCS=$(shell \ls src/{,import/}*.ml{,i})

mod_dep.dot: $(SRCS)
	ocamldep.opt $(SRCS) | $(OCAMLDOT) -r Ocamlformat -fullgraph > mod_dep.dot

.PHONY: clean
clean:
	rm -rf _build jbuild-workspace

.PHONY: fmt
fmt:
	$(shell \ls -t _build/*/src/ocamlformat.{exe,bc} | head -1) -i $(SRCS)
