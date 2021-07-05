#!/bin/bash

# Script to build `ocamlformat' under Windows, using the `mingw64' toolchain.
# All it requires is a standard Cygwin installation with the `mingw64'
# toolchain.

set -euo pipefail

opam_url=https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz

build_dir=_build-mingw64

mkdir -p ${build_dir}

cd ${build_dir}

curl -O -L ${opam_url}

tar xf $(basename ${opam_url})

bash opam64/install.sh --prefix $(pwd)

export PATH=$(pwd)/bin:${PATH}

export OPAMROOT="$(cygpath -aml _opam)"

opam init default "https://github.com/fdopen/opam-repository-mingw.git#opam2" -c "ocaml-variants.4.12.0+mingw64c" --disable-sandboxing --no-setup

eval $(opam env)

cd ..

set +eu
opam install -y --deps-only ./ocamlformat.opam
set -eu

dune subst

dune build -p ocamlformat
