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

# patch changes from https://github.com/ocaml/ocaml/pull/1229

src/format/format.ml:
	curl -s https://raw.githubusercontent.com/ocaml/ocaml/4.04.2/stdlib/format.ml > $@

src/format/format.mli:
	curl -s https://raw.githubusercontent.com/ocaml/ocaml/4.04.2/stdlib/format.mli > $@

src/format/format.ml.patch: src/format/format.ml
	diff -u src/format/format.ml src/format/format_.ml > $@; [ $$? -eq 1 ]

src/format/format.mli.patch: src/format/format.mli
	diff -u src/format/format.mli src/format/format_.mli > $@; [ $$? -eq 1 ]

src/format/format_.ml: src/format/format.ml
	patch -s -d src/format -i format.ml.patch -o format_.ml

src/format/format_.mli: src/format/format.mli
	patch -s -d src/format -i format.mli.patch -o format_.mli

src/Version.ml:
	@touch src/Version.ml

.PHONY:
version: src/Version.ml
	@echo "let version = \"$(shell if [[ "v0.1" == "%%"*"%%" ]]; then git describe --tags --dirty --always; else echo "v0.1"; fi)\"" | diff -N src/Version.ml - | patch src/Version.ml

.PHONY: setup
setup: src/jbuild-workspace src/format/format_.ml src/format/format_.mli version

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
	jbuilder build --root=src $(JBFLAGS) _build/opt/ocamlformat.exe

.PHONY: reason
reason: setup
	jbuilder build --root=src $(JBFLAGS) ocamlformat_reason.exe

SRCS=$(shell \ls src/{,import/}*.ml{,i})

mod_dep.dot: $(SRCS)
	ocamldep.opt $(SRCS) | $(OCAMLDOT) -r Ocamlformat -fullgraph > mod_dep.dot

.PHONY: clean
clean:
	rm -rf src/Version.ml src/_build src/jbuild-workspace src/format/format{,_}.ml{,i}

.PHONY: fmt
fmt:
	$(shell \ls -t src/_build/*/ocamlformat.{exe,bc} | head -1) -i $(SRCS)
