[![Build Status](https://travis-ci.org/ocaml-ppx/ocamlformat.svg?branch=master)](https://travis-ci.org/ocaml-ppx/ocamlformat)

# OCamlFormat

OCamlFormat is a tool to format OCaml code.

OCamlFormat works by parsing source code using the OCaml compiler's standard parser, deciding where to place comments in the parse tree, and writing the parse tree and comments in a consistent style.

See the source code of OCamlFormat itself and [Infer](https://github.com/facebook/infer) for examples of the styles of code it produces.

OCamlFormat requires source code that meets the following conditions:

- Does not trigger warning 50 (“Unexpected documentation comment.”). For code that triggers warning 50, it is unlikely that ocamlformat will happen to preserve the documentation string attachment.

- Parses without any preprocessing, using the version of the standard ocaml (not camlp4) parser used to build ocamlformat. Attributes and extension points should be correctly preserved, but other mechanisms such as camlp4, cppo, etc. will not work.

- Is either a module implementation (`.ml`), an interface (`.mli`) or a sequence of toplevel phrases (`.mlt`). jbuild files in ocaml syntax also work.

Under those conditions, ocamlformat is expected to produce output equivalent to the input. As a safety check in case of bugs, prior to terminating or modifying any input file, ocamlformat enforces the following checks:

- The parse trees obtained by parsing the original and formatted files are equal up to some minor normalization (see [`Normalize`](./src/Normalize.ml)`.equal_impl` or `equal_intf`).

- The documentation strings, and their attachment, has been preserved (implicit in the parse tree check).

- The set of comments in the original and formatted files is the same up to their location.

## Code style

There is currently no single dominant style for OCaml code, and the code produced by OCamlFormat does not attempt to closely mimic any of the common styles. Instead of familiarity, the focus is on legibility, keeping the common cases reasonably compact while attempting to avoid confusing formatting in corner cases. Improvement is inevitably possible.

There are many ways to format code that are correct in the sense that they parse to equivalent abstract syntax. Which choices OCamlFormat makes are not precisely specified, but some general guidelines that have directed the design are:

- Legibility, in the sense of making it as hard as possible for quick visual parsing to give the wrong interpretation, is of highest priority.

- Whenever possible, the high-level structure of the code should be obvious by looking only at the left margin, in particular, it should not be necessary to visually jump from left to right hunting for critical keywords/tokens/etc.

- All else equal, compact code is preferable, so indentation/white space is not added unless it helps legibility.

- Special attention has been given to making some standard syntactic gotchas visually obvious.

- When reformatting code, comments should not move around too much, but some movement seems to be unavoidable.

There is a huge space for subjective and personal preferences here, and it would be valuable to explore alternatives by adding configuration options to see which styles projects gravitate to over time. It would be even more interesting to see proposals for changes to the output which are objectively better, as opposed to subjectively different.

A limitation originates from the treatment of comments by the OCaml parser: they are not included in the parse tree itself, but are only available as a separate list. This means that OCamlFormat must decide where to place the comments within the parse tree. It does this based on source locations of code and comments, as well as using a few heuristics such as whether only white space separates a comment and some code.

## Installation

OCamlFormat can be installed with `opam`:

```
opam install ocamlformat
```

For the emacs integration:

- add `$(opam config var share)/emacs/site-lisp` to `load-path` (as done by `opam user-setup install`)

- add `(require 'ocamlformat)` to `.emacs`

- optionally add the following to `.emacs` to bind `C-M-<tab>` to the ocamlformat command and install a hook to run ocamlformat when saving:
```
(add-hook 'tuareg-mode-hook (lambda ()
  (define-key merlin-mode-map (kbd "C-M-<tab>") 'ocamlformat)
  (add-hook 'before-save-hook 'ocamlformat-before-save)))
```

Alternately, see [`ocamlformat.opam`](./ocamlformat.opam) for manual build instructions.

## Documentation

OCamlFormat is documented in its man page and through its internal help:

* `ocamlformat --help`
* `man ocamlformat`

## Reason

OCamlFormat is influenced by and follows the same basic design as `refmt` for [Reason](https://github.com/facebook/reason), but outputs OCaml instead of Reason.

Support for converting Reason to OCaml is included in the `ocamlformat_reason` package, which works by using `refmt` to parse Reason into an OCaml parse tree. The Reason converter can be installed using `opam`:
```
opam pin add ocamlformat_reason https://github.com/ocaml-ppx/ocamlformat.git
```

## Community

* forum: <https://discuss.ocaml.org/tags/ocamlformat>
* github: <https://github.com/ocaml-ppx/ocamlformat>
* issues: <https://github.com/ocaml-ppx/ocamlformat/issues>
* developers mailing list: <http://lists.ocaml.org/listinfo/ocamlformat-dev>

See [CONTRIBUTING](./CONTRIBUTING.md) for how to help out.

## License

OCamlFormat is [MIT-licensed](./LICENSE.md).
