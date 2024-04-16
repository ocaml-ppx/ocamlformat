Uuseg — Unicode text segmentation for OCaml
===============================================================================
v15.1.0+dune

Uuseg is an OCaml library for segmenting Unicode text. It implements
the locale independent [Unicode text segmentation algorithms][1] to
detect grapheme cluster, word and sentence boundaries and the [Unicode
line breaking algorithm][2] to detect line break opportunities.

The library is independent from any IO mechanism or Unicode text data
structure and it can process text without a complete in-memory
representation.

Uuseg is distributed under the ISC license. It depends on [Uucp].

[1]: http://www.unicode.org/reports/tr29/
[2]: http://www.unicode.org/reports/tr14/
[Uucp]: http://erratique.ch/software/uucp

Homepage: <http://erratique.ch/software/uuseg>

## Installation

Uuseg can be installed with `opam`:

    opam install uuseg
    opam install uuseg cmdliner uutf  # For the usegtrip tool. 

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc uuseg`.

Questions are welcome but better asked on the [OCaml forum] than on 
the issue tracker.

[online]: http://erratique.ch/software/uuseg/doc/
[OCaml forum]: https://discuss.ocaml.org/


## Sample programs

The [`usegtrip`] tool segments text provided on standard input.

See also the [doc examples].

[`usegtrip`]: test/usegtrip.ml
[doc examples]: test/examples.ml
