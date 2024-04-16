(lang dune 3.6)
(name ocaml-version)

(formatting (enabled_for dune))
(generate_opam_files true)

(source (github ocurrent/ocaml-version))
(authors "Anil Madhavapeddy <anil@recoil.org>")
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(documentation "https://ocurrent.github.io/ocaml-version/doc")
(license ISC)

(package
 (name ocaml-version)
 (synopsis "Manipulate, parse and generate OCaml compiler version strings")
 (description "\
This library provides facilities to parse version numbers of the OCaml \
compiler, and enumerates the various official OCaml releases and configuration \
variants.

OCaml version numbers are of the form `major.minor.patch+extra`, where the \
`patch` and `extra` fields are optional.  This library offers the following \
functionality:

- Functions to parse and serialise OCaml compiler version numbers.
- Enumeration of official OCaml compiler version releases.
- Test compiler versions for a particular feature (e.g. the `bytes` type)
- [opam](https://opam.ocaml.org) compiler switch enumeration.

### Further information

- **Discussion:** Post on <https://discuss.ocaml.org/> with the `ocaml` tag \
  under the Ecosystem category.
- **Bugs:** <https://github.com/ocurrent/ocaml-version/issues>
- **Docs:** <http://docs.mirage.io/ocaml-version>
")
 (depends
  (ocaml (>= 4.07.0))
  (alcotest :with-test))
 (tags (org:ocamllabs)))
