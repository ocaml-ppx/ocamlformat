# Contributing to OCamlFormat

## Testing on other projects

The `tools/test_branch.sh` script facilitates testing by building two versions of OCamlFormat and running them on the sources of specific projects. The list of tested projects can be found and modified in `test-extra/Makefile`.

These project sources are cloned into the `test-extra/code` directory. We use Git to manage the state of the script:

- Staged changes indicate the diffs introduced by formatting with the first revision.
- Unstaged changes represent the diffs introduced by upgrading OCamlFormat to the second revision.

## Release procedure

Follow these steps for the release procedure:

1. Ensure the release milestone is achieved.
2. Review the impact of the release on real source code: In `test-extra/Makefile`, uncomment the extended list of projects to test and run `tools/test_branch.sh -a "previous_release"`. Any diffs should be discussed with the maintainers of these projects to gather feedback before continuing the release.
3. If a supported OCaml version lacks CI support, run the test suite locally.
4. In `CHANGES.md`, replace `unreleased` with the current version (`a.b.c`) and date. In `README.md`, `doc/faq.mld`, and `doc/getting_started.mld`, update the recommended version to use in `.ocamlformat` files. Commit these changes.
5. Tag using `dune-release tag`.
6. Verify that the release binary has a release version number with: `dune build @install && dune install --prefix _install && ./_install/bin/ocamlformat --version && rm -rf _install`. It should output just `a.b.c`.
7. Lint using `dune-release lint`.
8. Amend the release commit if necessary and update the tag using `dune-release tag --force`.
9. Push the changes with `git push --tags`.
10. Release in automatic mode using `dune-release -p ocamlformat-lib,ocamlformat,ocamlformat-rpc-lib`.
11. Close the release milestone.
12. Each release should be announced on <https://discuss.ocaml.org/>.
13. Create a Windows asset:
    - Go to <https://github.com/ocaml-ppx/ocamlformat/actions/workflows/build-mingw64.yml>.
    - Run the workflow on the `main` branch.
    - Once the build is successful, go to the workflow result page.
    - Download the `ocamlformat-main.exe` artifact.
    - Rename it to `ocamlformat-a.b.c.exe`.
    - Go to <https://github.com/ocaml-ppx/ocamlformat/releases/edit/a.b.c>.
    - Add the binary.
    - Click "Update release".

## Backporting changes to a point release

Sometimes, it's helpful to create a point release, but the main branch contains changes that would be unsuitable for a point release.

Consider a situation where the last release is 0.3.5, and since then, the main branch contains new features F1, F2 and a bugfix B. Instead of reverting F1 and F2 in the main branch, you can create a release branch for the 0.3 releases.

Please follow the detailed guide in the original document on how to prepare a release branch, create a release, and merge the release branch.

## Building on Windows

`ocamlformat` can be built as a native Windows binary using the `mingw64` toolchain under Cygwin. The required Cygwin packages are:

- `git`, `curl`, `unzip`
- `m4`, `patchutils`, `make`
- `mingw64-x86_64-binutils`, `mingw64-x86_64-gcc-core`, `mingw64-x86_64-headers`, `mingw64-x86_64-runtime`

To build the binary, execute `bash tools/build-mingw64.sh` from the root of the repository. The first time this script is run, it will install `opam` in the subdirectory `_build-mingw64` and use it to install all the dependencies of `ocamlformat`, after which it will build the binary. Subsequent runs will only rebuild `ocamlformat`. If you need to start from scratch, simply remove the `_build-mingw64` directory.

This script can also be triggered as a GitHub Action named `build-mingw64`, which will build the binary in a GitHub worker and upload it back to GitHub. To retrieve it, select the Action run in question and scroll down to "Artifacts".

## Building the documentation

To build the documentation, run the following command:

```
dune build @doc && xdg-open _build/default/_doc/_html/ocamlformat/index.html
```
