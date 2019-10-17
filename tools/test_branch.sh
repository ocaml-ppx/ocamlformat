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
# the test branch. When this arg is not provided, we use the same value
# than for the base branch. This arg should be the empty string '' to
# set OCAMLFORMAT to an empty value.

set -e

if [[ ! -z "$1" ]]; then
    branch="$1"
else
    branch=$(git rev-parse HEAD)
fi

opts_base="$2"

if [[ $# -lt 3 ]]; then
    opts_branch="$opts_base"
else
    opts_branch="$3"
fi

base=$(git merge-base master $branch)

# First arg is git rev others are passed to make -C test-extra
run_in_worktree ()
{
  local tmp=`mktemp -d`
  git worktree add --detach "$tmp" "$1"
  shift
  make -C "$tmp"
  local exe=`ls "$tmp"/_build/{default,dev}/bin/ocamlformat.exe 2>/dev/null | head -n1`
  make -C test-extra "OCAMLFORMAT_EXE=$exe" "$@"
  git worktree remove --force "$tmp"
}

OCAMLFORMAT="$opts_base" run_in_worktree "$base" test_setup test_unstage test_clean test_pull test test_stage
OCAMLFORMAT="$opts_branch" run_in_worktree "$branch" test test_diff
