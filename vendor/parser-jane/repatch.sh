#!/bin/bash
set -uo pipefail

if [[ "$#" == "1" ]] ; then
    flambda_backend_dir="$1"
else
    echo "Wrong number of arguments"
    exit 1
fi

cd $(dirname $0)
cd ..

dirs="parser-standard ocaml-common"

for dir in $dirs; do
    mv parser-jane/for-$dir parser-jane/for-$dir-old
    mkdir parser-jane/for-$dir
done

./parser-jane/update.sh $flambda_backend_dir

root=$(pwd)

for dir in $dirs; do
    old_base=$root/parser-jane/for-$dir-old
    new_base=$root/parser-jane/for-$dir
    tip=$root/$dir

    cleanup() {
        rm -rf $old_base
        rm -rf $new_base/.git
        rm -rf $tip/.git
    }
    trap cleanup ERR EXIT

    (
        (
            cd $old_base
            git init -b main
            git add .
            git commit -m "create old-base" || true
        )
        (
            cd $new_base
            git init -b main
            git remote add old-base $old_base -f
            git reset old-base/main
            git add .
            git commit -m "old-base -> new-base" || true
        )
        (
            cd $tip
            git init -b main
            git remote add old-base $old_base -f
            git reset old-base/main
            git add .
            git commit -m "old-base -> old-tip" || true
            git remote add new-base $new_base -f
            echo "Merging $tip" >&3
            git merge new-base/main 2>&1 | grep '^CONFLICT' >&3 || true
        )
    ) 3>&1 2> /tmp/log.err >&2
    errno=$?

    if [ $errno -ne 0 ]; then
        cat /tmp/log.err >&2
        exit $errno
    fi

    cleanup
done
