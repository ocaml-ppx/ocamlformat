#!/usr/bin/env bash

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

# usage: OCAMLFORMAT=<option>=<value> test_branch.sh [<rev>]

set -e

if [[ ! -z "$1" ]]; then
    branch="$1"
else
    branch=$(git rev-parse HEAD)
fi

base=$(git merge-base master $branch)

git checkout $base
make
make -C test test_setup test_unstage test_clean test_pull test test_stage
git checkout $branch
make
make -C test test test_diff
