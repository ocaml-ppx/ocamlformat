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

# usage: bisect.sh [<rev>]
#
# The first arg is the revision to test. By default HEAD.
#
# Run the testsuite with ppx_bisect enabled.

set -e

if [[ ! -z "$1" ]]; then
    branch="$1"
else
    branch=$(git rev-parse HEAD)
fi

tmp=`mktemp -d`
git worktree add --detach "$tmp" "$branch"

sed -i 's/;;INSERT_BISECT_HERE;;/(preprocess (pps bisect_ppx))/' "$tmp/bin/dune" "$tmp/lib/dune"

# Run the tests
make -C "$tmp" test

dst="coverage"
bisect-ppx-report -I "$tmp/_build/default" -html "$dst" `find "$tmp" -name 'bisect*.out'`
echo "Coverage report generated in $dst/"
echo " => open $dst/index.html"

git worktree remove --force "$tmp"
