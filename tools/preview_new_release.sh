#!/usr/bin/env bash

set -o errexit
set -o nounset

github_prefix_set=0
gitlab_prefix_set=0
version_set=0
prefix_set=0

function usage()
{
  echo "usage: $0 -u <GITHUB_URL_PREFIX> -y <GITLAB_URL_PREFIX> -v <RELEASE> -p <PREFIX>"
  echo "Url prefix is of the form 'git@github.com:my_user'."
}

function comment_version()
{
  version=$1
  file=$2
  sed -i --follow-symlinks -e "s/^version\(.*\)/#version = $version/" $file
}

function uncomment_version()
{
  version=$1
  file=$2
  sed -i --follow-symlinks -e "s/^#version\(.*\)/version = $version/" $file
}

while getopts ":u:y:v:p:" opt; do
  case "$opt" in
    u)
      github_prefix=$OPTARG
      github_prefix_set=1
      ;;
    y)
      gitlab_prefix=$OPTARG
      gitlab_prefix_set=1
      ;;
    v)
      version=$OPTARG
      version_set=1
      ;;
    p)
      prefix=$OPTARG
      prefix_set=1
      ;;
    *)
      usage
      exit 1
      ;;
  esac
done

if [[ "$github_prefix_set" -eq 0 ]] ||
  [[ "$gitlab_prefix_set" -eq 0 ]] ||
  [[ "$version_set" -eq 0 ]] ||
  [[ "$prefix_set" -eq 0 ]]; then
  usage
  exit 1
fi;

project_list_file=`realpath tools/projects.data`
preview_branch=preview-ocamlformat-$version

preview_dir=$prefix/$preview_branch
log_dir=$preview_dir/logs
bin_dir=$preview_dir/bin

echo "Using directory $preview_dir"
rm -rf "$preview_dir" &> /dev/null || true
mkdir -p "$log_dir" "$bin_dir"

# Build the currently checked out version of OCamlformat and copy the binary in
# a directory that will not change
echo "Building OCamlformat"
dune build @install
cp -L _build/install/default/bin/ocamlformat "$bin_dir/ocamlformat"
PATH=$bin_dir:$PATH

while IFS=, read git_platform namespace project; do

  case "$git_platform" in
    "github")
      fork="$github_prefix/$project"
      upstream="https://github.com/$namespace/$project"
      ;;
    "gitlab")
      fork="$gitlab_prefix/$project.git"
      upstream="https://gitlab.com/$namespace/$project.git"
      ;;
    *)
      echo "Unknown git platform: $git_platform"
      continue
      ;;
  esac;

  echo "=> Checking $namespace/$project"

  echo "Cloning from $upstream"
  clone_dir="$preview_dir/$project"
  git clone --quiet --single-branch --depth=1 --filter=blob:none --no-checkout --recurse-submodules "$upstream" "$clone_dir"
  cd "$clone_dir"
  git checkout -b "$preview_branch" --quiet

  dune=dune

  case "$namespace/$project" in
    "tezos/tezos")
      bash scripts/lint.sh --update-ocamlformat
      ;;
    "ocaml/dune")
      make release
      dune=_build/default/bin/dune.exe
      ;;
  esac

  comment_version $version .ocamlformat
  $dune build @fmt --auto-promote &> "$log_dir/$project.log" || true
  uncomment_version "$version" .ocamlformat
  git diff --shortstat
  git commit --quiet --all -m "Preview: Upgrade to ocamlformat $version (unreleased)

The aim of this commit is to gather feedback.

Changelog can be found here: https://github.com/ocaml-ppx/ocamlformat/blob/main/CHANGES.md"

  echo "Pushing to $fork"
  git push --quiet -fu "$fork" "$preview_branch"
done < "$project_list_file"
