#!/usr/bin/env bash

set -o errexit
set -o nounset

github_prefix_set=0
gitlab_prefix_set=0
version_set=0
prefix_set=0
prev_binary_set=0
preview=0
github_user_set=0
gitlab_user_set=0

function usage()
{
  echo "usage: $0 [-n] -u <GITHUB_URL_PREFIX> -y <GITLAB_URL_PREFIX> [-U <GITHUB_USER>] [-Y <GITLAB_USER>] -v <RELEASE> -p <PREFIX> -b <EXE>"
  echo "Url prefix is of the form 'git@github.com:my_user'."
}

# Comment the 'version' option and strip the part after the '='.
# Spaces around the '=' are conserved.
function comment_version()
{
  sed -i --follow-symlinks -e "s/^\(version *= *\)[^ ].*/#\1/" $1
}

# Uncomment the 'version' option and update its value.
function uncomment_version()
{
  local version=$1 file=$2
  sed -i --follow-symlinks -e "s/^#\(version.*\)$/\1$version/" $file
}

# Apply ocamlformat on a project, update the '.ocamlformat' and commit.
# $signoff_opts must be set.
function apply_fmt()
{
  local version=$1 commit_msg=$2
  echo "Applying version $version"
  comment_version .ocamlformat
  $dune build @fmt --auto-promote &> "$log_dir/$project.log" || true
  uncomment_version "$version" .ocamlformat
  if ! git diff --shortstat --exit-code; then
    git commit --quiet --all $signoff_opts -m "$commit_msg"
    IGNORE_REVS+=("# Upgrade to OCamlformat $version")
    IGNORE_REVS+=("$(git rev-parse HEAD)")
  fi
}

while getopts ":u:y:v:p:b:nU:Y:" opt; do
  case "$opt" in
    u)
      github_prefix=$OPTARG
      github_prefix_set=1
      ;;
    U)
      github_user=$OPTARG
      github_user_set=1
      ;;
    y)
      gitlab_prefix=$OPTARG
      gitlab_prefix_set=1
      ;;
    Y)
      gitlab_user=$OPTARG
      gitlab_user_set=1
      ;;
    v)
      version=$OPTARG
      version_set=1
      ;;
    p)
      prefix=$OPTARG
      prefix_set=1
      ;;
    b)
      prev_binary=$(realpath $OPTARG)
      prev_binary_set=1
      ;;
    n)
      preview=1
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
preview_branch=upgrade-ocamlformat-$version

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
cp -L _build/install/default/bin/ocamlformat "$bin_dir/ocamlformat-next"
PATH=$bin_dir:$PATH

if [[ $preview -eq 1 ]]; then
  title="Preview: Upgrade to OCamlformat $version (unreleased)"
  description="OCamlformat is not released yet. The aim of this preview is to gather feedback.

Changelog can be found here: https://github.com/ocaml-ppx/ocamlformat/blob/main/CHANGES.md"
else
  title="Upgrade to OCamlformat $version"
  description="OCamlformat $version has just been released.

Changelog can be found here: https://github.com/ocaml-ppx/ocamlformat/releases"
fi

# Write the URLs to open the pull requests to that file
pr_urls_file=`realpath pull_request_urls`

# Options in 'opts' are enclosed in '<>' to allow safe checks that don't
# require complex parsing.
while IFS=, read git_platform namespace project opts; do (

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
  git clone --quiet --single-branch --filter=blob:none --recurse-submodules "$upstream" "$clone_dir"
  cd "$clone_dir"
  main_branch=`git branch --show-current`
  git checkout -b "$preview_branch" --quiet

  case "$namespace/$project" in
    "ocaml/dune")
      make release
      dune=_build/default/bin/dune.exe
      ;;
    *) dune=dune ;;
  esac

  if [[ $opts = *"<signoff>"* ]]; then
    signoff_opts=--signoff
  else
    signoff_opts=--no-signoff
  fi

  # Lines to insert into .git-blame-ignore-revs. Updated by [apply_fmt].
  IGNORE_REVS=()

  # Do a pass with the previous version.
  if [[ $prev_binary_set -eq 1 ]]; then
    ln -sf "$prev_binary" "$bin_dir/ocamlformat"
    prev_version=$(ocamlformat --version)
    apply_fmt "$prev_version" "Apply OCamlformat $prev_version"
  fi

  ln -sf "ocamlformat-next" "$bin_dir/ocamlformat"
  apply_fmt "$version" "$title

$description"
  if ! [[ $opts = *"<no-ignore-revs>"* ]] && [[ "${#IGNORE_REVS}" -gt 0 ]]; then
    printf "%s\n" "${IGNORE_REVS[@]}" >> .git-blame-ignore-revs
    git add .git-blame-ignore-revs
    git commit --quiet $signoff_opts -m "Update .git-blame-ignore-revs"
  fi

  echo "Pushing to $fork"
  git push --quiet -fu "$fork" "$preview_branch"

  # Pull request URL
  url_encode() { jq -rn --arg arg "$1" '$arg | @uri'; }
  case "$git_platform" in
    "github")
      if [[ $github_user_set -eq 1 ]]; then
        echo "https://github.com/$namespace/$project/compare/$main_branch...$github_user:$project:upgrade-ocamlformat-0.30.0?expand=1&title=`url_encode "$title"`&body=`url_encode "$description"`" >> "$pr_urls_file"
      fi
      ;;
  esac
  ) || echo "Error when upgrading $namespace/$project"
done < "$project_list_file"
