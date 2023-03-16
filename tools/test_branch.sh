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

# usage: test_branch.sh [-n] [-a=rev] [-b=rev] [<option>=<value>*] [<option>=<value>*]
#
# -a set the base branch and -b the test branch. The default value for
# the base branch is the merge-base between the test branch and main.
# The default value for the test branch is HEAD.
#
# If -n is passed, values of -a and -b are paths to ocamlformat binaries
# instead of revs.
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
arg_n=0
while getopts "a:b:n" opt; do
  case "$opt" in
    a) arg_a=$OPTARG ;;
    b) arg_b=$OPTARG ;;
    n) arg_n=1 ;;
  esac
done
shift $((OPTIND-1))
opts_a=$1
opts_b=${2-$opts_a}

# Build a specific version of ocamlformat and copy the binary.
# Arg: rev dest
build_version ()
{
  local rev=$1 dest=$2
  local tmp=`mktemp -d`
  echo "Building $rev in $tmp"
  git worktree add --detach "$tmp" "$rev"
  ( cd "$tmp"
    dune build @install
    dune install --prefix=dist ocamlformat &>/dev/null )
  cp "$tmp/dist/bin/ocamlformat" "$dest"
  git worktree remove --force "$tmp"
}

if [[ $arg_n = 0 ]]; then
  # Build two versions of ocamlformat in temporary directories

  rev_b=$(git rev-parse "${arg_b:-HEAD}")
  rev_a=$(git rev-parse "${arg_a:-$(git merge-base main "$rev_b")}")

  if [[ "$rev_a" = "$rev_b" ]]; then
    echo "The base branch is the same as the branch to test ($rev_a)" >&2
    if [[ "$opts_a" = "$opts_b" ]]; then
        exit 1
    fi
  fi

  exe_dir=`mktemp -d`
  trap 'rm -r "$exe_dir"' EXIT

  exe_a="$exe_dir/a"
  exe_b="$exe_dir/b"
  build_version "$rev_a" "$exe_a"
  build_version "$rev_b" "$exe_b"

else
  # Two versions of ocamlformat are provided

  if [[ -z $arg_a ]] || [[ -z $arg_b ]]; then
    echo "-a and -b are mandatory when -n is passed." >&2
    exit 1
  fi
  exe_a=$arg_a
  exe_b=$arg_b
fi

make -C test-extra test_setup test_unstage test_clean test_pull

OCAMLFORMAT="$opts_a" make -C test-extra "OCAMLFORMAT_EXE=$exe_a" test test_stage
OCAMLFORMAT="$opts_b" make -C test-extra "OCAMLFORMAT_EXE=$exe_b" test test_diff
