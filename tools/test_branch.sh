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

CODE_CACHE=/tmp/ocamlformat_test_branch_cache

# Arguments: name git_uri [excluded_dirs...]
# Env: DST_DIR
fetch_project ()
{
  local name="$1" git_uri="$2"
  shift 2
  local code="$CODE_CACHE/$name"
  if ! [[ -d $code ]]; then
    mkdir -p "$code"
    git clone "$git_uri" "$code"
  fi

  local f dst="$DST_DIR/$name" exclude_args=()
  for f in ./_build "$@"; do exclude_args+=(-path "./$f" -not -prune -or); done

  ( cd "$code"
    find . "${exclude_args[@]}" \
      -name '*.ml' -and -not -name '*.pp.ml' -or \
      -name '*.mli' -and -not -name '*.pp.mli' -or \
      -name '*.opam' -or -name '.ocamlformat*'
  ) |
  while read f; do
    mkdir -p "$dst/${f%/*}"
    cp "$code/$f" "$dst/$f"
  done
}

# Copy files to be formatted for every projects in $DST_DIR/project_name
# Env: DST_DIR
fetch_projects ()
{
  fetch_project ocamlformat "`pwd`" test
  fetch_project infer "https://github.com/facebook/infer.git"
  fetch_project js_of_ocaml "https://github.com/ocsigen/js_of_ocaml.git"
  fetch_project ocaml "https://github.com/ocaml/ocaml.git" experimental testsuite/tests
  fetch_project dune "https://github.com/ocaml/dune.git" test vendor otherlibs example
  # fetch_project owl "https://github.com/owlbarn/owl"
  # fetch_project irmin "https://github.com/mirage/irmin"
  # fetch_project index "https://github.com/mirage/index"
  # fetch_project duniverse "https://github.com/ocamllabs/duniverse" duniverse
  # fetch_project dune-release "https://github.com/ocamllabs/dune-release"
}

# Arguments: ocamlformat_rev src_dir dst_dir
# Env: OCAMLFORMAT
run_in_worktree ()
{
  local rev="$1" src_dir="$2" dst_dir="$3"
  local tmp=`mktemp -d`
  echo "Building $rev in $tmp"
  git worktree add --detach "$tmp" "$rev"
  shift
  ( cd "$tmp"
    dune build -p ocamlformat
    dune install --prefix=dist ocamlformat &>/dev/null )
  local exe="$tmp/dist/bin/ocamlformat"
  echo "Built $exe"

  ( cd "$src_dir"
    find . -type d -exec mkdir -p "$dst_dir/{}" \;
    find . -type f -name '*.ml' -or -name '*.mli' |
    parallel --bar "$exe" --enable-outside-detected-project --no-version-check -o "$dst_dir/{}" "{}"
  ) || true

  git worktree remove --force "$tmp"
}

opt_a=
opt_b=
while getopts "a:b:" opt; do
  case "$opt" in
    a) opt_a=$OPTARG ;;
    b) opt_b=$OPTARG ;;
  esac
done
shift $((OPTIND-1))

opts_a=$1
opts_b=${2-$opts_a}

rev_b=$(git rev-parse "${opt_b:-HEAD}")
rev_a=$(git rev-parse "${opt_a:-$(git merge-base master "$rev_b")}")

if [[ "$rev_a" = "$rev_b" ]]; then
  echo "The base branch is the same as the branch to test ($rev_a)"
  exit 1
fi

base_dir=`mktemp -d`
src_dir="$base_dir/src"
a_dir="$base_dir/a"
b_dir="$base_dir/b"

DST_DIR=$src_dir fetch_projects

OCAMLFORMAT="$opts_a" run_in_worktree "$rev_a" "$src_dir" "$a_dir"
OCAMLFORMAT="$opts_b" run_in_worktree "$rev_b" "$src_dir" "$b_dir"

for p in `ls "$src_dir"`; do
  report="test_branch_${p}_${rev_a:0:8}_${rev_b:0:8}.diff"
  git diff --no-index "$a_dir/$p" "$b_dir/$p" > "$report" || true
  echo "Wrote diffs to $report"
done

rm -r "$base_dir"
