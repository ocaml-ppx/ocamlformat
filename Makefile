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

.PHONY: default
default: exe gen-help

.PHONY: exe
exe:
	dune build -p ocamlformat

.PHONY: gen-help
gen-help:
	dune build ocamlformat-help.txt

.PHONY: reason
reason:
	dune build -p ocamlformat_reason

.PHONY: ocamlformat-diff
ocamlformat-diff:
	dune build -p ocamlformat_diff

.PHONY: clean
clean:
	dune clean

SRCS=$(shell \find src tools -name '[^.]*.ml' -or -name '[^.]*.mli' -or -name '[^.]*.mlt')

.PHONY: fmt
fmt:
	dune exec -- ocamlformat -i $(SRCS)

.PHONY: test fixpoint regtests regtests-promote test-reason
test: fixpoint regtests

fixpoint:
	dune exec -- ocamlformat -n 1 -i $(SRCS)

regtests:
	dune runtest

regtests-promote:
	dune runtest --auto-promote

test-reason:
	dune build @runtest-reason --auto-promote
