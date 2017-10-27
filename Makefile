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

src/jbuild-workspace: src/jbuild-workspace.in
	sed -e "s|@OPAM_SWITCH[@]|$$(opam switch show)|g" $< > $@

src/Version.ml:
	@touch src/Version.ml

.PHONY:
version: src/Version.ml
	@echo "let version = \"$(shell if [[ "%%VERSION%%" == "%%"*"%%" ]]; then git describe --tags --dirty --always; else echo "%%VERSION%%"; fi)\"" | diff -N src/Version.ml - | patch src/Version.ml

.PHONY: setup
setup: src/jbuild-workspace version

.PHONY: exe
exe: setup
	jbuilder build --root=src $(JBFLAGS) ocamlformat.exe

.PHONY: bc
bc: setup
	jbuilder build --root=src $(JBFLAGS) _build/dbg/ocamlformat.bc

.PHONY: dbg
dbg: setup
	jbuilder build --root=src $(JBFLAGS) _build/dbg/ocamlformat.exe

.PHONY: opt
opt: setup
	jbuilder build --root=src $(JBFLAGS) _build/default/ocamlformat.exe

.PHONY: reason
reason: setup
	jbuilder build --root=src $(JBFLAGS) ocamlformat_reason.exe

SRCS=$(shell \ls src/{,import/}*.ml{,i})

mod_dep.dot: $(SRCS)
	ocamldep.opt $(SRCS) | $(OCAMLDOT) -r Ocamlformat -fullgraph > mod_dep.dot

.PHONY: clean
clean:
	rm -rf src/Version.ml src/_build src/jbuild-workspace

.PHONY: fmt
fmt:
	$(shell \ls -t src/_build/*/ocamlformat.{exe,bc} | head -1) -i $(SRCS)
