# parse-wyc (parse what you can)

[![ocaml-ci](https://img.shields.io/badge/ocaml--ci-builds-informational?logo=ocaml&style=for-the-badge)](https://ci.ocamllabs.io/github/gpetiot/parse-wyc/)

Disclaimer: this is highly experimental!

Adapted from the recovery-parser of `merlin`.


## How to use it

The API of the library is:
```ocaml file=parse_wyc.ml
val implementation : Lexing.lexbuf -> Location.t list
val interface : Lexing.lexbuf -> Location.t list
val use_file : Lexing.lexbuf -> Location.t list
```

By using `Parse_wyc.implementation lx` you can retrieve the list of locations in the original file delimiting invalid code parts.
