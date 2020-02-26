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

# usage: test_branch.sh [-a=rev] [-b=rev] [<option>=<value>*] [<option>=<value>*]
#
# -a set the base branch and -b the test branch. The default value for
# the base branch is the merge-base between the test branch and master.
# The default value for the test branch is HEAD.
#
# The first arg is the value of OCAMLFORMAT to be used when formatting
# using the base branch (a)
#
# The second arg is the value of OCAMLFORMAT to be used when formatting
# the test branch (b). When this arg is not provided, we use the same
# value than for the base branch. This arg should be the empty string ''
# to set OCAMLFORMAT to an empty value.

set -e

arg_a=
arg_b=
while getopts "a:b:" opt; do
  case "$opt" in
    a) arg_a=$OPTARG ;;
    b) arg_b=$OPTARG ;;
  esac
done
shift $((OPTIND-1))

opts_a=$1
opts_b=${2-$opts_a}

rev_b=$(git rev-parse "${arg_b:-HEAD}")
rev_a=$(git rev-parse "${arg_a:-$(git merge-base master "$rev_b")}")

if [[ "$rev_a" = "$rev_b" ]]; then
  echo "The base branch is the same as the branch to test ($rev_a)"
  if [[ "$opts_a" = "$opts_b" ]]; then
      exit 1
  fi
fi

# First arg is git rev others are passed to make -C test-extra
# Env: OCAMLFORMAT
run_in_worktree ()
{
  local tmp=`mktemp -d`
  echo "Building $1 in $tmp"
  git worktree add --detach "$tmp" "$1"
  shift
  ( cd "$tmp"
    dune build -p ocamlformat
    dune install --prefix=dist ocamlformat &>/dev/null )
  local exe="$tmp/dist/bin/ocamlformat"
  echo "Built $exe"
  make -C test-extra "OCAMLFORMAT_EXE=$exe" "$@"
  git worktree remove --force "$tmp"
}

make -C test-extra test_setup test_unstage test_clean test_pull

OCAMLFORMAT="$opts_a" run_in_worktree "$rev_a" test test_stage
OCAMLFORMAT="$opts_b" run_in_worktree "$rev_b" test test_diff
