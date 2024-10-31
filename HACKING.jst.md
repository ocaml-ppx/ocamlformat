Jane Street `ocamlformat` instructions
======================================

This file describes the shape of the `ocamlformat` repo, with a
specific eye toward making it easy to update `ocamlformat` to deal
with Jane Street extensions.

Overall structure
-----------------

The main implementation of `ocamlformat` lives in the `lib` directory.
This is where the `ocamlformat` executable is driven.

The most important file for our purposes here is `lib/Fmt_ast.ml`, which
defines the functions that actually print the formatted AST. Other modules
of interest in `lib/`:

* `Fmt.ml` has combinators useful for printing. You're likely to use this
  module but not super likely to change it.

* `Ast.ml` has various helpers, mainly for (a) constructing contexts, and (b)
  analyzing whether terms need parentheses. It's common to need to change the
  latter for new syntax.

* `Cmts.ml` deals with comments (not the `cmt` file format!). Comments are not
  places in the extended AST, but rather maintained in a table that is checked
  at various points using the helpers in this module.

* `Sugar.ml` has various bits of support code for sugaring/desugaring syntax
  (e.g., "multi-argument" functions).

`ocamlformat` also includes two copies of the OCaml parser, all in
`vendor`:

* `parser-standard` is meant to be very similar to the parser used in the
  OCaml compiler. Its role in `ocamlformat` is (only) to provide a safety-check:
  we want to make sure that the formatting does not change the AST as parsed by
  the compiler. So after formatting, the parsed AST is checked against the
  original parsed AST to make sure they are the same (modulo some normalization,
  as written in `lib/Normalize_std_ast.ml`). Key point: this parser and its
  representation are *not* pretty-printed and never interact with the
  pretty-printer. The *only* reason this is here is to mimic the behavior of the
  OCaml compiler. Accordingly, it should be as close to the OCaml compiler's
  parser as possible.

* `parser-extended` uses an extended parsetree, capable of storing extra
  information useful in preserving the structure of the user's code. It is this
  extended parsetree that gets pretty-printed. The parser here forms part of the
  core implementation of `ocamlformat`.

The directory `vendor/ocaml-common` contains files that are shared between
`parser-standard` and `parser-extended`, like `location.ml`.  The `test`
directory contains tests (see the Testing section below).

Design considerations
---------------------

Because the value of `parser-standard` is entirely in its ability to mimic the
compiler's parser, we want to keep this parser as close as possible to the
compiler's. We will want to copy over any changes made to the compiler's parser
into this version of `ocamlformat`'s parser.

On the other hand, the `parser-extended` can go off in its own direction: its
parsetree should be designed to make pretty-printing easy. In addition, we want
to make sure that incorporating upstream changes is easy. We thus feel free to
edit the parsetree in `parser-extended`, but we do so by adding new
constructors, not modifying existing ones. In addition, new code should be
marked off with comments like `(* Jane Street extension *)` and `(* End Jane
Street extension *)`. Because of the ability to extend the parsetree here, we do
*not* use jane-syntax in `parser-extended`.

`ocamlformat` routinely checks, at various places in `Fmt_ast`, that the thing
it's about to print is an exact subterm of a "context" we're within (some larger
term that we're in the middle of printing). Make sure you've designed your
additions to `parser-extended` so that, while printing, you'll always recur on
exact structural subterms. If you, for example, print an attribute specially
and then try to recur on a term without that attribute, you're in for a bad
time.

Before building
---------------

You will need a switch that is on OCaml 5. Most jane street devs use different
switches for working on ocamlformat and working on the compiler. You will need
to install several libraries in your switch. This command may work:

```
opam install menhir.20231231 fix ocp-indent bechamel-js alcotest camlp-streams fpath either dune-build-info uuseg ocaml-version stdio
```

Building
--------

To build, run `dune build`.

How to update `ocamlformat`
---------------------------

The base branch to work from is called `jane`. Create a branch off of `jane`.

1. Apply the parsing changes you need from `flambda-backend` to
   `vendor/parser-standard`.

   In either case, when incorporating new support files from the compiler,
   consider whether than can be shared in `vendor/ocaml-common` rather than
   copied into each of the parser directories.  This is typically the case if
   the support module doesn't depend on the parsetree.

   * Option 1 (preferred): Use the `vendor/parser-jane/repatch.sh` script to
     import all changes from the latest version of `flambda-backend` (presumably
     including the changes you are interested in).

     This may pull in additional parser changes, which you can see by looking at
     the diff it creates. You aren't obligated to add styling for all new
     things, but it might be good to point out to the author of the relevant
     features that they have work to do here eventually, or make tickets.

   * Option 2: Manually apply the relevant changes to `vendor/parser-standard`.
     This option is worse because it means `parser-standard` no longer tracks a
     specific revision of the compiler's parser, and is likely to create
     conflicts when using Option 1 in the future. Remember: this "standard"
     parser should be as close as possible to the compiler's. Note that some
     files used by both parsers are stored in `vendor/ocaml-common` and may need
     to be updated.

2. Get `ocamlformat` compiled and passing the tests. If the patch to
   `flambda-backend` was backward compatible, then this should be
   straightforward. (If your changes affect files in `vendor/ocaml-common`, this
   might not be so easy. That's OK. Just move on to the next step.)

3. Edit the parsetree in `vendor/parser-extended/parsetree.mli` to support your
   new syntax. Copy over any changes to the parser and lexer from the
   `flambda-backend` patch, updating the parser's semantic actions as necessary.

4. Edit the pretty-printer in `lib/Fmt_ast.ml` to format your new syntax nicely.
   This may require changes to other `lib/` modules, such as `Ast.ml` and
   `Sugar.ml`.

5. Add tests. Get them to pass. See the "Testing" section below.

Testing
-------

To just run your built ocamlformat against a file manually, run
`dune exec ocamlformat -- --enable-outside-detected-project path/to/your/file.ml`.

Run the tests with `dune test`. There are two kinds of tests:

1) Correctly formatted files, which ocamlformat is run on to check that there
   are no changes.  We have historically mainly added these, but not for any
   particularly good reason.
2) Incorrectly formatted files, for which the output of ocamlformat is checked
   against a reference.

To add a test, you add one, two or three files depending on what kind of test it
is:

- (Always) Add `tests/passing/tests/foo.ml` (where foo is the name of your new
  test).  This is the file ocamlformat will be run on.
- (Optional) If your file is incorrectly formatted, write the correctly
  formatted version in `tests/passing/tests/foo.ml.ref`.
- (Optional) If it is expected `ocamlformat` will print information to stderr
  when running your test (uncommon) write that output to
  `tests/passing/tests/foo.ml.err`.

Now, run `dune test`. It will discover your new file and suggest edits to
the generated `tests/passing/dune.inc` file to run your new tests. Run
`dune promote` to update `dune.inc`. This will *not* accept your new tests -- it
just allows you to run your new tests.

Then, run `dune test` again to actually run your tests. You will see any changes
necessary to make the tests pass. You can run `dune promote` to accept those
changes.

If you get some cryptic error output with a few lines of the `dune.inc` file, it
is likely that ocamlformat has crashed (e.g. with a parser error) while looking
at your test. The cryptic error output will mention the name of the test. Run
ocamlformat manually on your test file to see the actual error.

Debugging
---------

- To see the `parser-extended` ast coming out of the parser for a given source
  file, use `dune exec tools/printast/printast.exe -- path/to/your/file.ml`.

- It can be useful to visualize the _boxes_ the pretty printer creates to align
  and group code.  To see these, run `ocamlformat` with the `-g` flag.  E.g.,:

  ```
  dune exec ocamlformat -- -g --enable-outside-detected-project testfile.ml
  ```

  This will create a file `/tmp/testfile_boxes.html`, which can be opened in a
  browser to see a visual representation of the boxes.

Validity checking
-----------------

The ocamlformat repo has (at least) two validity checks for repo health:

* The ocamlformat sources themselves must be formatted. You can run this check
with `make fmt` (which will also auto-format `dune-project`).  To reformat files
that are incorrect, run `dune build @fmt --auto-promote`. Running `make test`
runs `make fmt` and `dune runtest` together.  The CI will check both the
formatting check and the ocamlformat tests (but will not update `dune-project`).

* All commits must be signed off. This is easy. When you're done with your
sequence of commits and it's all ready to merge, just run
`git rebase <starting commit> --signoff`, where `<starting commit>` is the
commit before any of your edits. You can often say something like `origin/jane`
or `HEAD~4` or similar.
