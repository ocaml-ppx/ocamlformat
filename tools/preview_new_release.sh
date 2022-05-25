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

github_user_=
github_user_set=0
gitlab_user_=
gitlab_user_set=0
version=
version_set=0
update_repos=0
prefix=
prefix_set=0

function usage()
{
    echo "usage: $0 -u <GITHUB_USERNAME> -y <GITLAB_USERNAME> -v <RELEASE> [-U] [-p <PREFIX>]"
}

while getopts ":u:y:v:Up:" opt; do
    case "$opt" in
        u)
            github_user=$OPTARG;
            github_user_set=1;
            ;;
        y)
            gitlab_user=$OPTARG;
            gitlab_user_set=1;
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

if [ "$github_user_set" = 0 ] || [ "$gitlab_user_set" = 0 ] || [ "$version_set" = 0 ]; then
    usage;
    exit 1;
fi;

preview_branch=preview-ocamlformat-$version

if [ "$prefix_set" = 0 ]; then
    prefix=$HOME/$preview_branch;
fi;

log_dir=$prefix/logs;

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

dirname=`dirname $0`;

while read line; do
    cd $prefix;

    git_platform=`echo $line | cut -d "," -f 1`;
    namespace=`echo $line | cut -d "," -f 2`;
    dir=`echo $line | cut -d "," -f 3`;
    echo "=> Checking $namespace/$dir";

    case "$git_platform" in
        "github")
            user=$github_user;
            ;;
        "gitlab")
            user=$gitlab_user;
            ;;
        *)
            ;;
    esac;

    if [ ! -d "$dir" ] ; then
        fork="git@$git_platform.com:$user/$dir.git";
        upstream="git@$git_platform.com:$namespace/$dir.git";
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

    if git show-ref --verify --quiet "refs/heads/$preview_branch"; then
        get_main_branch;
        git checkout $main_branch --quiet;
        git branch -D $preview_branch --quiet;
    fi;
    git checkout -b $preview_branch --quiet;

    dune=dune;
    if [ "$namespace/$dir" == "ocaml/dune" ]; then
        make release;
        dune=_build/default/bin/dune.exe;
    fi;

    sed -i --follow-symlinks -e "s/^version\(.*\)/#version = $version/" .ocamlformat;

    if [ "$namespace/$dir" == "tezos/tezos" ]; then
        sed -i --follow-symlinks -e "s/^version\(.*\)/#version = $version/" scripts/lint.sh;
        git commit --all -m "Update .ocamlformat files";
        bash scripts/lint.sh --update-ocamlformat;
    fi;

    $dune build @fmt &> $log_dir/$dir.log || true;
    $dune promote &> /dev/null;
    sed -i --follow-symlinks -e "s/^#version\(.*\)/version = $version/" .ocamlformat;

    git commit --all -m "Preview: upgrade to ocamlformat $version";

    if [ "$namespace/$dir" == "tezos/tezos" ]; then
        sed -i --follow-symlinks -e "s/^#version\(.*\)/version = $version/" scripts/lint.sh;
        git commit --all -m "Update .ocamlformat files";
        bash scripts/lint.sh --update-ocamlformat;
    fi;
done < $dirname/projects.data;
