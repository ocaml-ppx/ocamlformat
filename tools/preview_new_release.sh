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

set -o errexit
set -o nounset

user=
user_set=0
version=
version_set=0
update_repos=0
prefix=
prefix_set=0

function usage()
{
    echo "usage: $0 -u <GITHUB_USERNAME> -v <RELEASE> [-U] [-p <PREFIX>]"
}

while getopts ":u:v:Up:" opt; do
    case "$opt" in
        u)
            user=$OPTARG;
            user_set=1;
            ;;
        v)
            version=$OPTARG;
            version_set=1;
            ;;
        U)
            update_repos=1;
            ;;
        p)
            prefix=$OPTARG;
            prefix_set=1;
            ;;
        *)
            usage;
            exit 1;
            ;;
    esac
done;

if [ "$user_set" = 0 ] || [ "$version_set" = 0 ]; then
    usage;
    exit 1;
fi;

preview_branch=preview-ocamlformat-$version

if [ "$prefix_set" = 0 ]; then
    prefix=$HOME/$preview_branch;
fi;

log_dir=$prefix/logs;

projects=(
#    'facebook/flow' # not interested in upgrading
    'facebook/infer'
    'mirage/alcotest'
    'mirage/decompress'
    'mirage/digestif'
    'mirage/index'
    'mirage/irmin'
    'mirage/mirage'
    'mirage/ocaml-cohttp'
#    'mirage/ocaml-conduit' # not interested in upgrading
    'mirage/ocaml-git'
    'ocaml/dune'
    'ocaml/ocaml-lsp'
    'ocaml/odoc'
    'ocamllabs/dune-release'
    'ocaml-ppx/ppxlib'
    'ocsigen/js_of_ocaml'
    'owlbarn/owl'
    'realworldocaml/mdx'
);

function get_main_branch() {
    gitHeadsDir="$(git rev-parse --show-toplevel)/.git/refs/heads";

    if [ -f "$gitHeadsDir/main" ]; then
        main_branch='main';
    elif [ -f "$gitHeadsDir/master" ]; then
        main_branch='master';
    else
        main_branch=$(git remote show origin | grep 'HEAD branch' | cut -d' ' -f5);
    fi;
}

rm -rf $log_dir &> /dev/null || true;
mkdir --parents $log_dir;

for project in "${projects[@]}"; do
    cd $prefix;

    namespace=`echo $project | cut -d "/" -f 1`;
    dir=`echo $project | cut -d "/" -f 2`;
    echo "=> Checking $project";

    if [ ! -d "$dir" ] ; then
        fork="git@github.com:$user/$dir.git";
        upstream="git@github.com:$namespace/$dir.git";
        git clone --recurse-submodules $fork $dir;
        cd $dir;
        git remote add upstream $upstream;
        get_main_branch;
    else
        cd $dir;

        if [ "$update_repos" = 1 ]; then
            get_main_branch;
            git checkout $main_branch --quiet;
            git fetch upstream --quiet;
            git rebase upstream/$main_branch --quiet;
            git push origin $main_branch --quiet;
        fi;
    fi;

    dune=dune;

    if git show-ref --verify --quiet "refs/heads/$preview_branch"; then
        get_main_branch;
        git checkout $main_branch --quiet;
        git branch -D $preview_branch --quiet;
    fi;
    git checkout -b $preview_branch --quiet;

    if [ "$project" == "ocaml/dune" ]; then
        make release;
        dune=_build/default/bin/dune.exe;
    fi;

    ocamlformat_file=`readlink -f .ocamlformat`;
    cat $ocamlformat_file | sed -e "/^version/d" > .ocamlformat.tmp;
    mv .ocamlformat.tmp $ocamlformat_file;
    $dune build @fmt &> $log_dir/$dir.log || true;
    $dune promote &> /dev/null;
    cat $ocamlformat_file | sed -e "1s/^/version = $version\n/" > .ocamlformat.tmp;
    mv .ocamlformat.tmp $ocamlformat_file;
    git commit --all -m "Preview: upgrade to ocamlformat $version";
done;
