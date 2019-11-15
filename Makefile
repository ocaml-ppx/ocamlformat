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
	dune build bin/ocamlformat.exe

.PHONY: gen-help
gen-help:
	dune build ocamlformat-help.txt

.PHONY: reason
reason:
	dune build bin/ocamlformat_reason.exe

.PHONY: ocamlformat-diff
ocamlformat-diff:
	dune build tools/ocamlformat-diff/ocamlformat_diff.exe

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build @bin/fmt @lib/fmt @tools/fmt

.PHONY: test regtests regtests-promote test-reason
test: fmt regtests

regtests:
	dune runtest

regtests-promote:
	dune runtest --auto-promote

test-reason:
	dune build @runtest-reason --auto-promote
