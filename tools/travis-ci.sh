#!/usr/bin/env bash

######################################################################
#                                                                    #
#                            OCamlFormat                             #
#                                                                    #
#  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  #
#                                                                    #
#  This source code is licensed under the MIT license found in the   #
#  LICENSE file in the root directory of this source tree.           #
#                                                                    #
######################################################################

# Warning: this script should only be called from .travis.yml

set -e

CheckBuild () {
    # before_install
    wget -O ${HOME}/opam https://github.com/ocaml/opam/releases/download/2.0.5/opam-2.0.5-x86_64-linux
    chmod +x ${HOME}/opam
    export PATH=${HOME}:${PATH}
    export OPAMYES=1
    export OPAMJOBS=2
    opam init --bare --disable-sandboxing
    opam switch --dry-run ${OCAML_VERSION} 2>/dev/null || opam switch create ${OCAML_VERSION}
    # install
    opam switch ${OCAML_VERSION}
    opam pin remove --no-action ocamlformat || true # Otherwise opam will ignored new deps
    opam update --upgrade
    opam pin add --no-action ocamlformat .
    opam install --deps-only --with-test ocamlformat
    # script
    opam exec -- make
    opam exec -- make reason
}

CheckTests () {
    # script
    opam exec -- make test
    # do not run 'make test-reason' because of heavy deps
}

HasNoChangelogNeededLabel () {
    url="https://api.github.com/repos/$TRAVIS_REPO_SLUG/pulls/$TRAVIS_PULL_REQUEST"
    curl "$url" | jq '.labels|any(.name == "no-changelog-needed")'
}

CheckChangesModified () {
    if [ "$TRAVIS_PULL_REQUEST" != false ] && [ "$TRAVIS_BRANCH" = master ]
    then
        if [ "$(HasNoChangelogNeededLabel)" != false ] ; then
            echo skipped
        else
            git diff --exit-code "$TRAVIS_BRANCH...$TRAVIS_COMMIT" -- CHANGES.md \
                > /dev/null && exit 1 || echo pass
        fi
    fi
}

case $CI_KIND in
build)
    CheckBuild
    ;;
build-and-tests)
    CheckBuild
    CheckTests
    ;;
changes)
    CheckChangesModified
    ;;
*)
    echo unknown CI kind
    exit 1
    ;;
esac
