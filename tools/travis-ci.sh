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
    opam install --deps-only ocamlformat
    # script
    opam exec -- make
}

HasNoChangelogNeededLabel () {
    url="https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/labels"
    response=$(curl -s -L "$url")
    echo "$response"
    # Because of rate limiting, the response may not be what we expect.
    # Assume the label is not present in this case.
    if needed=$(jq 'any(.name == "no-changelog-needed")' <<<"$response") &&
      [[ $needed = true ]]
    then return 0
    else return 1
    fi
}

CheckChangesModified () {
    if [ "$TRAVIS_PULL_REQUEST" != false ] && [ "$TRAVIS_BRANCH" = master ]
    then
        if HasNoChangelogNeededLabel ; then
            echo skipped
        else
            if git diff --exit-code "$TRAVIS_COMMIT_RANGE" -- CHANGES.md > /dev/null
            then echo "The changelog is not uptodate"; exit 1
            else echo pass
            fi
        fi
    fi
}

case $CI_KIND in
build)
    CheckBuild
    ;;
changes)
    CheckChangesModified
    ;;
*)
    echo unknown CI kind
    exit 1
    ;;
esac
