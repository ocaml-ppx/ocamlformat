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

# usage: test_branch.sh [<rev>] [<option>=<value>*] [<option>=<value>*]
#
# The first arg is the revision/branch to test.
#
# The second arg is the value of OCAMLFORMAT to be used when formatting
# the merge base of the test branch.
#
# The third arg is the value of OCAMLFORMAT to be used when formatting
# the test branch.

set -e

if [[ ! -z "$1" ]]; then
    branch="$1"
else
    branch=$(git rev-parse HEAD)
fi

base=$(git merge-base master $branch)

git checkout $base
make
OCAMLFORMAT="$2" make -C test-extra test_setup test_unstage test_clean test_pull test test_stage
git checkout $branch
make
OCAMLFORMAT="$3" make -C test-extra test test_diff
