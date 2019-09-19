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

# First arg is git rev others are passed to make -C test-extra
run_in_worktree ()
{
  local tmp=`mktemp -d`
  git worktree add --detach "$tmp" "$1"
  shift
  make -C "$tmp"
  local exe=`ls "$tmp"/_build/{default,dev}/src/ocamlformat.exe 2>/dev/null | head -n1`
  OCAMLFORMAT_EXE=$exe make -C test-extra "$@"
  git worktree remove -f "$tmp"
}

OCAMLFORMAT="$2" run_in_worktree "$base" test_setup test_unstage test_clean test_pull test test_stage
OCAMLFORMAT="$3" run_in_worktree "$branch" test test_diff
