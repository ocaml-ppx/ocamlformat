#!/usr/bin/env bash
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

find * -type f | grep -v '\(_build\|_opam\|test\|vendor\|.merlin\|ocamlformat.el\)' | grep -v '.*\.\(org\|md\|txt\)$' | xargs headache -c tools/config.headache -h tools/header.txt
