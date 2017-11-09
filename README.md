# OCamlFormat

OCamlFormat is a tool to format OCaml code.

OCamlFormat works by parsing source code using the OCaml compiler's standard parser, deciding where to place comments in the parsetree, and writing the parsetree and comments in a consistent style.

See the source code of OCamlFormat itself and [Infer](https://github.com/facebook/infer) for examples of the styles of code it produces.

Support for converting Reason to OCaml is included in the `ocamlformat_reason` package, which works by using `refmt` to parse Reason into an OCaml parsetree.

## Limitations

There is currently no single dominant style for OCaml code, and the code produced by OCamlFormat does not attempt to closely mimic any of the common styles. Instead, the focus is on legibility, keeping the common cases reasonably compact while attempting to avoid confusing formatting in corner cases. Improvement is inevitably possible.

Another limitation originates from the treatment of comments by the OCaml parser: they are not included in the parsetree itself, but are only available as a separate list. This means that OCamlFormat must decide where to place the comments within the parsetree. It does this based on source locations of code and comments, as well as using a few heuristics such as whether only white space separates a comment and some code.

A limitation of the current implementation, though not a fundamental issue, is that the object language is largely unimplemented.

## Influences

OCamlFormat follows the same basic design as `refmt` from [Reason](https://github.com/facebook/reason), but outputs OCaml instead of Reason, and differs in details.

## Installation

OCamlFormat can be installed with `opam`:

```
opam pin add ocamlformat_support https://github.com/ocaml-ppx/ocamlformat.git#support
opam pin add ocamlformat https://github.com/ocaml-ppx/ocamlformat.git
```

and for the Reason converter:

```
opam pin add ocamlformat_support https://github.com/ocaml-ppx/ocamlformat.git#support
opam pin add ocamlformat_reason https://github.com/ocaml-ppx/ocamlformat.git
```

Alternately, see [`ocamlformat.opam`](./ocamlformat.opam) and [`ocamlformat_reason.opam`](./ocamlformat_reason.opam) for manual build instructions.

## Documentation

OCamlFormat is documented in its man page and through its internal help:

* `ocamlformat --help`
* `man ocamlformat`

## Community

* forum: <https://discuss.ocaml.org/tags/ocamlformat>
* github: <https://github.com/ocaml-ppx/ocamlformat>
* issues: <https://github.com/ocaml-ppx/ocamlformat/issues>
* developers mailing list: <http://lists.ocaml.org/listinfo/ocamlformat-dev>

See [CONTRIBUTING](./CONTRIBUTING.md) for how to help out.

## License

OCamlFormat is MIT-licensed.
