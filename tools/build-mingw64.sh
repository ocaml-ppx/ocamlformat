#!/bin/bash

# Script to build `ocamlformat' under Windows, using the `mingw64' toolchain.
# All it requires is a standard Cygwin installation with the `mingw64'
# toolchain.

# The script builds `flexdll', `ocaml', `dune', `ocamlformat' and their
# dependencies and leaves the resulting binary at
#
#   _build/install/default/bin/ocamlformat.exe

set -euo pipefail

root=$(git rev-parse --show-toplevel)

flexdll_version=0.39
flexdll_url=https://github.com/alainfrisch/flexdll

ocaml_version=4.12.0
ocaml_url=https://github.com/ocaml/ocaml

dune_version=2.9.0
dune_url=https://github.com/ocaml/dune

packages=(
    https://github.com/janestreet/sexplib0                v0.14.0
    https://github.com/ocaml-dune/csexp                   master
    https://github.com/dune-universe/uuseg                duniverse-v13.0.0
    https://github.com/ocaml/ocaml-re                     master
    https://github.com/ocaml/odoc                         1.5.2
    https://github.com/OCamlPro/ocp-indent                master
    https://gitlab.inria.fr/fpottier/menhir               master
    https://github.com/janestreet/stdio                   master
    https://github.com/dune-universe/fpath                duniverse-v0.7.3
    https://github.com/mirage/either                      main
    https://github.com/dbuenzli/cmdliner                  master
    https://github.com/janestreet/base                    v0.14.1
    https://github.com/dune-universe/astring              duniverse-v0.8.5
    https://github.com/dune-universe/uucp                 duniverse-v13.0.0
    https://github.com/dune-universe/uutf                 duniverse-v1.0.2
    https://github.com/ocaml-community/cppo               master
    https://gitlab.inria.fr/fpottier/fix                  master
    https://github.com/janestreet/result                  master
    https://github.com/ocaml-ppx/ppxlib                   0.22.2
    https://github.com/ocurrent/ocaml-version             master
    https://github.com/ocaml-ppx/ppx_derivers             master
    https://github.com/janestreet/ocaml-compiler-libs     master
    https://github.com/ocaml/stdlib-shims                 master
    https://github.com/ocaml-ppx/ocaml-migrate-parsetree  v2.2.0
)

build_dir=${root}/build-mingw64

mkdir -p ${build_dir}

cd ${build_dir}

if ! [ -d _flexdll ]
then
    curl -O -L ${flexdll_url}/releases/download/${flexdll_version}/flexdll-bin-${flexdll_version}.zip
    unzip -d _flexdll flexdll-bin-${flexdll_version}.zip
fi

export PATH=$(pwd)/_flexdll:${PATH}

[ -d _ocaml ] || git clone -b ${ocaml_version} ${ocaml_url} _ocaml

cd _ocaml

if ! [ -f local/bin/ocamlc.exe ]
then
    set +eu
    ./configure --build=x86_64-unknown-cygwin --host=x86_64-w64-mingw32 --prefix "$(cygpath -aml local)"
    set -eu
    make -j8
    make install
fi

export PATH=$(pwd)/local/bin:${PATH}

cd ..

[ -d dune ] || git clone -b ${dune_version} ${dune_url} dune

cd dune

if ! [ -f _build/install/default/bin/dune.exe ]
then
    make release
fi

export PATH=$(pwd)/_build/install/default/bin:${PATH}

cd ..

echo ${packages[@]} | xargs -n 2 | while read url ref
do
    dir=$(basename ${url})
    [ -d ${dir} ] || git clone -b ${ref} ${url} ${dir}
done

cd ..

dune build --profile=release ocamlformat.install
