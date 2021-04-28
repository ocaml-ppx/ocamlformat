## Compatibility `Either` module
[![OCaml-CI Build Status][ci-badge]][ci-status]
[![Documentation](https://img.shields.io/badge/doc-online-blue.svg)][docs]

Projects that want to use the `Either` module defined in OCaml 4.12.0 while
staying compatible with older versions of OCaml should use this library
instead.  On OCaml versions 4.12 and above, this library defines an alias
`Either` to the standard library's definition of the module.  Otherwise, it
provides an equivalent definition of `Either`.

[ci-status]: https://ci.ocamllabs.io/github/mirage/either
[ci-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fmirage%2Feither%2Fmain&logo=ocaml
[docs]: https://docs.mirage.io/either/either/index.html

#### Depending on this library

Opam libraries depending on this module are encouraged to use a conditional
dependency:

```
depends: [
  ("ocaml" {>= "4.12.0"} | "either")
]
```

This ensures that any dependencies of your library that _don't_ need pre-4.12
compatibility will never transitively depend on `either`.

<hr/>

#### Acknowledgements

This repository structure is mostly copied from
[`JaneStreet/result`](https://github.com/janestreet/result), which provides a
compatibility `result` type. Thanks to the OCaml maintainers for their
improvements to the standard library.
