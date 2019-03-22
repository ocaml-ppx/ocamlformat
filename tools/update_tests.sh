#!/usr/bin/env bash

######################################################################
#                                                                    #
#                            OCamlFormat                             #
#                                                                    #
#  Copyright (c) 2018-present, Facebook, Inc.  All rights reserved.  #
#                                                                    #
#  This source code is licensed under the MIT license found in the   #
#  LICENSE file in the root directory of this source tree.           #
#                                                                    #
######################################################################

# usage: update_tests.sh <file>*
#
# The arguments are the names of the tests to update (without the directory),
#  e.g.:  update_tests.sh comments.ml comments_in_record.ml

ROOT=$(git rev-parse --show-toplevel)
EXE="$ROOT"/_build/dev/src/ocamlformat.exe
OLD_DIR=$(pwd)
RUN_DIR=$ROOT/test
TEST_DIR=passing

function update () {
    FILE="${1}"
    echo "Updating $FILE"

    cd $RUN_DIR

    if [ -f $TEST_DIR/$FILE ]; then
        if [ -f $TEST_DIR/$FILE.opts ]; then
            if [ -f $TEST_DIR/$FILE.ref ]; then
                $EXE `cat $TEST_DIR/$FILE.opts` $TEST_DIR/$FILE \
                    &> $TEST_DIR/$FILE.ref
            elif [ -f $TEST_DIR/$FILE.ref.ocpi ]; then
                $EXE `cat $TEST_DIR/$FILE.opts` $TEST_DIR/$FILE \
                    | ocp-indent &> $TEST_DIR/$FILE.ref.ocpi
            else
                $EXE `cat $TEST_DIR/$FILE.opts` $TEST_DIR/$FILE -i
            fi
        else
            if [ -f $TEST_DIR/$FILE.ref ]; then
                $EXE $TEST_DIR/$FILE &> $TEST_DIR/$FILE.ref
            elif [ -f $TEST_DIR/$FILE.ref.ocpi ]; then
                $EXE $TEST_DIR/$FILE | ocp-indent &> $TEST_DIR/$FILE.ref.ocpi
            else
                $EXE $TEST_DIR/$FILE -i
            fi
        fi
    else
        echo "Error: File $FILE not found"
    fi

    cd $OLD_DIR
}

if [[ $1 = "--all" ]]; then
    ALL=($RUN_DIR/$TEST_DIR/*.ml{,i})
    set "${ALL[@]##*/}"
fi

for F in "$@"; do
    update $F
done
