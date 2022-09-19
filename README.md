[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-ppx%2Focamlformat%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/ocaml-ppx/ocamlformat)

# OCamlFormat

Hello, new user! Welcome! :wave:

If you are here, you are probably interested in using a formatting tool for your code base, so that you do not have to worry about formatting it by hand, and to speed up code review by focusing on the important parts.

OCamlFormat is probably what you are after!

OCamlFormat works by parsing then outputting again the same OCaml source file in a consistent style.

Read the [documentation](https://ocaml.org/p/ocamlformat) to learn more about OCamlFormat!

## Getting started

### Installation

OCamlFormat can be installed with `opam`:

```
opam install ocamlformat
```

Alternatively, see [`ocamlformat.opam`](./ocamlformat.opam) for manual build instructions.

### Formatting code!

Setting up your project to use the default profile and the OCamlFormat version you installed (hopefully the last one) in this `.ocamlformat` file is considered good practice:
```
profile = default
version = 0.24.1
```

To manually invoke OCamlformat the general command is:

```
ocamlformat [OPTION]... [SRC]...
```
See `ocamlformat --help` or `man ocamlformat` for the detail about options.

You can also view it [online](https://github.com/ocaml-ppx/ocamlformat/blob/main/ocamlformat-help.txt).

The most common usecase involves using the `dune` build system, once your project is correctly setup (see [Dune's manual](https://dune.readthedocs.io/en/stable/formatting.html#formatting-a-project)) you can reformat your project using:

```
dune build @fmt
```

## Community

* forum: <https://discuss.ocaml.org/tags/ocamlformat>
* github: <https://github.com/ocaml-ppx/ocamlformat>
* issues: <https://github.com/ocaml-ppx/ocamlformat/issues>

See [CONTRIBUTING](./CONTRIBUTING.md) for how to help out.

## License

OCamlFormat is [MIT-licensed](./LICENSE.md).
