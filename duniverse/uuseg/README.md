Uuseg — Unicode text segmentation for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Uuseg is an OCaml library for segmenting Unicode text. It implements
the locale independent [Unicode text segmentation algorithms][1] to
detect grapheme cluster, word and sentence boundaries and the
[Unicode line breaking algorithm][2] to detect line break
opportunities.

The library is independent from any IO mechanism or Unicode text data
structure and it can process text without a complete in-memory
representation.

Uuseg depends on [Uucp](http://erratique.ch/software/uucp) and
optionally on [Uutf](http://erratique.ch/software/uutf) for support on
OCaml UTF-X encoded strings. It is distributed under the ISC license.

[1]: http://www.unicode.org/reports/tr29/
[2]: http://www.unicode.org/reports/tr14/

Homepage: http://erratique.ch/software/uuseg  

## Installation

Uuseg can be installed with `opam`:

    opam install uuseg
    opam install uutf uuseg # for support on OCaml UTF-X encoded strings

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference can be consulted [online][doc] or
via `odig doc uuseg`.

[doc]: http://erratique.ch/software/uuseg/doc/


## Sample programs

If you installed Uuseg with `opam` sample programs are located in
the directory `opam config var uuseg:doc`.

In the distribution sample programs are located in the `test`
directory of the distribution, they can be built with:

   topkg build --tests true

- `test.native` tests the library, nothing should fail.
- `usegtrip.native` inputs Unicode text on `stdin` and rewrites
  segments on `stdout`. Invoke with `--help` for more information
  Depends on [Uutf](http://erratique.ch/software/uutf) and
  [Cmdliner](http://erratique.ch/software/cmdliner).
