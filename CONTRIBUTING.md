# Contributing to OCamlFormat

## Reporting Issues

If you encounter a problem when using OCamlFormat or if you have any questions, please open a [GitHub issue](https://github.com/ocaml-ppx/ocamlformat/issues/).

1. Check first if your issue has already been [reported](https://github.com/ocaml-ppx/ocamlformat/issues/).
2. Include the version of OCamlFormat you are using (`ocamlformat --version`).
3. Include a (preferably short) source file which demonstrates the issue.
4. Describe the expected and actual behavior.
5. Do not unsubscribe from the issue until it is closed, the maintainers may ask for your feedback.

When acknowledged, the project maintainers will add [labels](#ocamlformat-labels) to your issue throughout its lifespan.

## Pull Requests

We actively welcome pull requests.

1. Prior to investing a large amount of time into significant or invasive changes, or those that affect the output, it is likely more efficient to first open an issue for discussion and planning.
2. If you are not familiar with the project, focus first on the [Good first issues](https://github.com/ocaml-ppx/ocamlformat/issues?q=is%3Aissue+is%3Aopen+label%3AGood-first-issue).
3. Fork the repository and create your branch from `master`.
4. If you have added code that should be tested, add tests (they should be located in the `tests/passing` directory).
'.
5. Ensure the test suite passes (see [Running the tests](#running-the-tests)).
6. If you haven't already, complete the Contributor License Agreement ("CLA").

When acknowledged, the project maintainers will add [labels](#ocamlformat-labels) to your pull request throughout its lifespan.


### Running the tests

Once OCamlFormat has been built, run `make test` to check for regressions.

The first step of `make test` is to ensure that invoking OCamlFormat on its own source code produces the same source code (you can run `make fixpoint` to only check this). If OCamlFormat is not integrated with the editor you use, you should run `make fmt` to reformat all OCamlFormat source files.

The second part of `make test` is to ensure the test suite passes, the test report is displayed in the terminal and you should only get `[FAILED] [BASELINE]` (in orange) results. You should not get any result in red nor any `[REGRESSION]`.

Once `make test` passes, pull requests should be tested on the code in a set of external repositories. This can be done by executing `tools/test_branch.sh <rev>` where `<rev>` is the git revision/branch containing the pull request's changes. If a pull request affects an option, `OCAMLFORMAT=<option>=<value> tools/test_branch.sh <rev>` should also be run to test with the option enabled.

The `tools/test_branch.sh` script runs two versions of `ocamlformat` (the specified branch, or `HEAD` if omitted, and its merge base with master) on the test code and reports the differences in the formatted code. Formatting failures may also be reported on the terminal, these will need to be fixed before a pull request can be merged. The differences should be inspected to ensure they are as intended. Pull requests changing the format of code should generally introduce an option to enable the alternate style (if in doubt, open an issue for discussion). Any differences in the formatted code with the option disabled should be summarized and explained in the pull request discussion.

To benefit from the autocompletion of git branch names in the `tools/test_branch.sh` script, if you use the `bash` shell, add the following line to your `~/.bashrc` file:
```
source <path-to-ocamlformat>/tools/ocamlformat_test_branch
```

If you use the `zsh` shell, add the following lines to your `~/.zshrc` file:
```
autoload bashcompinit
bashcompinit
source <path-to-ocamlformat>/tools/ocamlformat_test_branch
```

## Contributor License Agreement ("CLA")

In order to accept your pull request, we need you to submit a CLA. You only need to do this once to work on any of Facebook's open source projects. Complete your CLA here: <https://code.facebook.com/cla>. If you have any questions, please drop us a line at cla@fb.com.

## License

By contributing to OCamlFormat, you agree that your contributions will be licensed under the LICENSE file in the root directory of this source tree.

## OCamlFormat Labels

### Pull Request specific

- [CLA Signed](https://github.com/ocaml-ppx/ocamlformat/labels/CLA%20Signed): Added automatically as long as the user signed the Contributor License Agreement (see above)

### Issue specific

- [Good-first-issue](https://github.com/ocaml-ppx/ocamlformat/labels/Good-first-issue): This issue is a good entry point in the project for new contributors

### Kind

- [Kind/Bug](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2FBug): This issue describes a problem or this contribution addresses a problem (erroneous/unintended behavior)
- [Kind/Docs](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2FDocs): This is related to a documentation change (user documentation or source code documentation)
- [Kind/Feature-request](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2FFeature-request): This issue or contribution proposes a new feature
- [Kind/Style-suggestion](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2FStyle%20suggestion): This issue or contribution proposes a style modification for the formatted output
- [Kind/To-discuss](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2FTo-discuss): Discussion needed to converge to a solution

### Priority

- [Priority/Critical](https://github.com/ocaml-ppx/ocamlformat/labels/Priority%2FCritical): Highest priority, for blocking bugs or features that prevent ocamlformat to be used
- [Priority/High](https://github.com/ocaml-ppx/ocamlformat/labels/Priority%2FHigh): For non-blocking bugs or features, that don't prevent ocamformat to be used
- [Priority/Medium](https://github.com/ocaml-ppx/ocamlformat/labels/Priority%2FMedium): For important documentation and style suggestions
- [Priority/Low](https://github.com/ocaml-ppx/ocamlformat/labels/Priority%2FLow): Lowest priority, for less important documentation and style suggestions

### Status

- [Status/0-More-info-needed](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F0-More-info-needed): More information is needed before this issue can be triaged
- [Status/0-Triage](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F0-Triage): This issue needs triaging
- [Status/1-Acknowledged](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F1-Acknowledged): This issue has been triaged and is being investigated
- [Status/2-Regression](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F2-Regression): Issue present in the current version but not in an earlier one
- [Status/3-Fixed-need-test](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F3-Fixed-need-test): This issue has been fixed and needs checking
- [Status/4-Fixed](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F4-Fixed): This issue has been fixed
- [Status/5-Awaiting-feedback](https://github.com/ocaml-ppx/ocamlformat/labels/Status%2F5-Awaiting-feedback): This issue or contribution requires feedback from other maintainers or from its submitter
