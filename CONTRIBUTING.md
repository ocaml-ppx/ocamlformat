# OCamlFormat Contribution Guide

Welcome and thank you for your interest in contributing to OCamlFormat. This contribution guide will help you through the contribution process.

## Reporting Issues

If you encounter an issue with OCamlFormat, or if you have inquiries, we encourage you to open a [GitHub issue](https://github.com/ocaml-ppx/ocamlformat/issues/). Please follow the steps below:

1. First, verify if your issue hasn't already been [reported](https://github.com/ocaml-ppx/ocamlformat/issues/).
2. Specify the OCamlFormat version you're using (`ocamlformat --version`).
3. Include a (preferably short) source file that replicates the issue.
4. Clearly define the expected and actual behavior.
5. Remain engaged with the issue until closure as your feedback might be needed.

## Pull Requests

We heartily welcome pull requests. To ensure an effective contribution, please adhere to the steps below:

1. For significant, invasive, or output-affecting changes, consider opening an issue for discussion before committing substantial time and effort.
2. If you're new to the project, starting with the [Good first issues](https://github.com/ocaml-ppx/ocamlformat/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22Good-first-issue+%3Agreen_heart%3A%22) can be beneficial.
3. Fork the repository and create your branch from `main`.
4. If you've added code that should be tested, supplement it with tests located in the `test/` directory.
5. Ensure that the test suite passes (see [Running the tests](#running-the-tests) for instructions).

### Running the Tests

After building OCamlFormat, execute `dune runtest` to check for any regressions. There should be no unexpected diffs. If there are, promote all the diffs with `dune promote`.

Once the test suite passes, test your pull requests against code in external repositories by executing `tools/test_branch.sh`. If your pull request modifies an option, also run `tools/test_branch.sh "<option>=<value>"` to test with the option enabled.

The `tools/test_branch.sh` script compares two `ocamlformat` versions (the specified branch or `HEAD` if omitted, and its main merge base) on other project code and reports any formatting differences. Inspect any differences to make sure they are intentional. Differences in the formatted code with the option disabled should be summarised and explained in the pull request discussion.

OCamlformat's own code must be formatted with the development version and not with the latest release. This can be done with `dune build @fmt`.

## Licensing

By contributing to OCamlFormat, you accept that your contributions will fall under the LICENSE located in the root directory of this source tree.

## OCamlFormat Labels

To better manage issues and pull requests, we use the following labels:

### Issue-Specific Labels

- [Good-first-issue](https://github.com/ocaml-ppx/ocamlformat/labels/Good-first-issue%20%3A%2B1%3A): Suitable for newcomers to the project

### Kind Labels

- [Kind/Bug](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2FBug%20%3Ax%3A): Represents a problem or solution to an erroneous/unintended behavior
- [Kind/Docs](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2Fdocs): Pertains to changes in user or source code documentation
- [Kind/Feature-request](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2Ffeature-request): Proposes a new feature
- [Kind/Style-suggestion](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2Fstyle-suggestion): Suggests a modification for the formatted output
- [Kind/To-discuss](https://github.com/ocaml-ppx/ocamlformat/labels/Kind%2Fto-discuss): Invites discussion to converge on a solution

You're welcome to contribute to issues and pull requests marked as ["Help wanted"](https://github.com/ocaml-ppx/ocamlformat/labels/Help-wanted%20%3Awarning%3A), or any other issues.
