#!/bin/bash
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

# Script to build `ocamlformat' under Windows, using the `mingw64' toolchain.
# All it requires is a standard Cygwin installation with the `mingw64'
# toolchain.

set -euo pipefail

opam_url=https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz
opam_archive=$(basename ${opam_url})

build_dir=_build-mingw64

mkdir -p ${build_dir}

cd ${build_dir}

[ -f ${opam_archive} ] || curl -O -L ${opam_url}

[ -d opam64 ] || tar xf ${opam_archive}

[ -f bin/opam.exe ] || bash opam64/install.sh --prefix $(pwd)

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
