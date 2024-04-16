Uucp — Unicode character properties for OCaml
=============================================
v15.1.0+dune

Uucp is an OCaml library providing efficient access to a selection of
character properties of the [Unicode character database].

Uucp is distributed under the ISC license. It has no dependency.

Home page: <http://erratique.ch/software/uucp>

[Unicode character database]: http://www.unicode.org/reports/tr44/

## Installation

Uucp can be installed with `opam`:

    opam install uucp
    opam install uucp uunf cmdliner  # For ucharinfo cli tool

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation can be consulted [online] or via `odig doc uucp`.

Uucp's documentation also has a [minimal Unicode introduction][intro]
and some [Unicode OCaml tips][tips].

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: http://erratique.ch/software/uucp/doc/
[intro]: http://erratique.ch/software/uucp/doc/unicode.html#minimal
[tips]: http://erratique.ch/software/uucp/doc/unicode.html#tips
[OCaml forum]: https://discuss.ocaml.org/


## Sample programs

The [`ucharinfo`] tool allows to report character information on the
command line.

See also the [doc examples]. 

[`ucharinfo`]: test/ucharinfo.ml
[doc examples]: test/examples.ml
