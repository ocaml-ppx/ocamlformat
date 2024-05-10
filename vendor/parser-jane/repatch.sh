#!/bin/bash
set -euo pipefail

if [[ "$#" == "1" ]] ; then
    flambda_backend_dir="$1"
else
    echo "Wrong number of arguments"
    exit 1
fi

cd $(dirname $0)
cd ..

cleanup() {
    rm -f changes-parser.patch
    rm -f changes-common.patch
}
trap cleanup ERR EXIT

commands=(
    "diff -ruN parser-jane/for-parser-standard/ parser-standard/ > changes-parser.patch || true"
    "diff -ruN parser-jane/for-ocaml-common/ ocaml-common/ > changes-common.patch || true"
    "./parser-jane/update.sh $flambda_backend_dir"
    "rm -rf parser-standard/ ocaml-common/"
    "cp -r parser-jane/for-parser-standard parser-standard/"
    "cp -r parser-jane/for-ocaml-common ocaml-common/"
    "patch -p1 -d parser-standard/ < changes-parser.patch"
    "patch -p1 -d ocaml-common/ < changes-common.patch"
)

for cmd in "${commands[@]}"
do
    echo "> $cmd"
    eval $cmd
done
