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

# usage: bisect.sh [<rev>]
#
# The first arg is the revision to test. By default HEAD.
#
# Run the testsuite with ppx_bisect enabled.

set -e

if [[ -n "$1" ]]; then
    branch="$1"
else
    branch=$(git rev-parse HEAD)
fi

tmp=$(mktemp -d)
git worktree add --detach "$tmp" "$branch"

make -C "$tmp" coverage

git worktree remove --force "$tmp"
