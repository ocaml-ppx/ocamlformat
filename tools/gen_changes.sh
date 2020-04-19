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

git log --no-merges --pretty=format:"  + %s (%an)" $1..$2
