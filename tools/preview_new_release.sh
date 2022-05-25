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
prefix=
prefix_set=0

function usage()
{
    echo "usage: $0 -u <GITHUB_USERNAME> -y <GITLAB_USERNAME> -v <RELEASE> [-p <PREFIX>]"
}

while getopts ":u:y:v:p:" opt; do
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
    prefix=$HOME;
fi;

preview_dir=$prefix/$preview_branch;
log_dir=$preview_dir/logs;

rm -rf $preview_dir &> /dev/null || true;
mkdir --parents $log_dir;

dirname=`dirname $0`;

while read line; do
    cd $preview_dir;

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

    fork="git@$git_platform.com:$user/$dir.git";
    upstream="git@$git_platform.com:$namespace/$dir.git";
    git clone --single-branch --depth=1 --filter=blob:none --no-checkout --recurse-submodules $fork $dir;
    cd $dir;
    git remote add upstream $upstream;
    git checkout -b $preview_branch --quiet;

    sed -i --follow-symlinks -e "s/^version\(.*\)/#version = $version/" .ocamlformat;

    if [ "$namespace/$dir" == "tezos/tezos" ]; then
        sed -i --follow-symlinks -e "s/^version\(.*\)/#version = $version/" devtools/git-gas-diff/.ocamlformat;
        sed -i --follow-symlinks -e "s/^version\(.*\)/#version = $version/" scripts/lint.sh;
        git commit --all -m "Update .ocamlformat files";
        bash scripts/lint.sh --update-ocamlformat;
    fi;

    dune=dune;
    if [ "$namespace/$dir" == "ocaml/dune" ]; then
        make release;
        dune=_build/default/bin/dune.exe;
    fi;

    $dune build @fmt &> $log_dir/$dir.log || true;
    $dune promote &> /dev/null;
    sed -i --follow-symlinks -e "s/^#version\(.*\)/version = $version/" .ocamlformat;

    git commit --all -m "Preview: upgrade to ocamlformat $version";

    if [ "$namespace/$dir" == "tezos/tezos" ]; then
        sed -i --follow-symlinks -e "s/^#version\(.*\)/version = $version/" devtools/git-gas-diff/.ocamlformat;
        sed -i --follow-symlinks -e "s/^#version\(.*\)/version = $version/" scripts/lint.sh;
        git commit --all -m "Update .ocamlformat files";
        bash scripts/lint.sh --update-ocamlformat;
    fi;
done < $dirname/projects.data;
