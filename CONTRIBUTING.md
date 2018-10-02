# Contributing to OCamlFormat

## Reporting Issues

If you encounter a problem when using ocamlformat or if you have any questions, please open a [GitHub issue](https://github.com/ocaml-ppx/ocamlformat/issues/).

1. Check first if your issue has already been [reported](https://github.com/ocaml-ppx/ocamlformat/issues/).
2. Include the version of ocamlformat you are using (`ocamlformat --version`).
3. Include a (preferably short) source file which demonstrates the issue.
4. Describe the expected and actual behavior.
5. Do not unsubscribe from the issue until it is closed, the maintainers may ask for your feedback.

## Pull Requests

We actively welcome pull requests.

1. Prior to investing a large amount of time into significant or invasive changes, or those that affect the output, it is likely more efficient to first open an issue for discussion and planning.
2. If you are not familiar with the project, focus first on the [Good first issues](https://github.com/ocaml-ppx/ocamlformat/issues?q=is%3Aissue+is%3Aopen+label%3AGood-first-issue).
3. Fork the repository and create your branch from `master`.
4. If you have added code that should be tested, add tests (they should be located in the `tests/passing` directory).
'.
5. Ensure the test suite passes (see [Running the tests](#running-the-tests)).
6. If you haven't already, complete the Contributor License Agreement ("CLA").

### Running the tests

Once ocamlformat has been built, run `make test` to check for regressions.

The first step of `make test` is to ensure that invoking ocamlformat on its own source code produces the same source code (you can run `make fixpoint` to only check this). If ocamlformat is not integrated with the editor you use, you should run `make fmt` to reformat all ocamlformat source files.

The second part of `make test` is to ensure the test suite passes, the test report is displayed in the terminal and you should only get `[PASSED]` (in green) and `[FAILED]` (in orange). You should not get any result in red nor any `[REGRESSION]`.

## Contributor License Agreement ("CLA")

In order to accept your pull request, we need you to submit a CLA. You only need to do this once to work on any of Facebook's open source projects. Complete your CLA here: <https://code.facebook.com/cla>. If you have any questions, please drop us a line at cla@fb.com.

## License

By contributing to OCamlFormat, you agree that your contributions will be licensed under the LICENSE file in the root directory of this source tree.
