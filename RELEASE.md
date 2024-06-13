# Explaination of the versionning scheme

OCamlformat now uses an unsual versionning scheme. For every version `X.Y.Z` of
ocamlformat, there are multiple packages :

- `ocamlformat.X.Y.Z` which contains the `ocamlformat` executable
- `ocamlformat-X_Y_Z.1.0` which contains the `ocamformat-X_Y_Z` executable

You also have packages for the libs : `ocamlformat-lib.X.Y.Z` and
`ocamlformat-lib-X_Y_Z.1.0`. `ocamlformat-lib-X_Y_Z` is only published in its
own package because it is required for both `ocamlformat-lib` and the executable
`ocamlformat-X_Y_Z`.

You can only have one `ocamlformat` executable on your switch, but you can have
multiple `ocamlformat-X_Y_Z` installed.

The `ocamlformat` package does not contain the formatter itself: it just reads
your configuration, and search for the `ocamlformat-X_Y_Z` executable for the
correct version. If the executable is found, it is executed, if not,
instructions to install it are printed.

# How to make a release

When you are develloping, the current version is `dev`. You have packages
`ocamlformat-dev`, `ocamlformat-dev`, etc.

To make a release you need to pick a release number `A.B.C`, and checkout to a
fresh release branch. Then go to `release_version_renamer/lexer.mll` and replace
the line `let target_version = "dev"` with the line `let target_version =
"A_B_C"`. Make sure to separate the version numbers with underscores and not
dots : they need to be used in module names.

Then you can simply run `bash release_rename.sh` and the release is ready.
