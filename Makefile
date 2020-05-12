##########################################################################
#                                                                        #
#                              OCamlFormat                               #
#                                                                        #
#            Copyright (c) Facebook, Inc. and its affiliates.            #
#                                                                        #
#      This source code is licensed under the MIT license found in       #
#      the LICENSE file in the root directory of this source tree.       #
#                                                                        #
##########################################################################

.PHONY: default
default: exe

.PHONY: exe
exe:
	dune build bin/ocamlformat.exe

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build @fmt

.PHONY: test regtests regtests-promote
test: fmt regtests

regtests:
	dune runtest

regtests-promote:
	dune runtest --auto-promote

headers:
	tools/update_headers.sh
	dune build @fmt --auto-promote
