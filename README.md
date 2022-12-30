[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-ppx%2Focamlformat%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/ocaml-ppx/ocamlformat)

# 🎨 ocamlformat

ocamlformat is a tool for formatting OCaml code. It automatically adjusts the layout of your code to follow the recommended style guidelines, making it easier to read and understand.

## 🚀 Installation

To use ocamlformat, you will need to have [OCaml](https://ocaml.org/) and [opam](https://opam.ocaml.org/) (the OCaml package manager) installed on your system.

Once you have these dependencies, installation is a breeze! Just run the following command:

```
opam install ocamlformat
```

## 💻 Usage

Formatting your code with ocamlformat is super simple! Just run the following command:

```
ocamlformat file.ml
```

You can also specify a configuration file to customize the formatting behavior of ocamlformat.
To set up your project to use the default profile and the ocamlformat version you installed let's create a configuration file named `.ocamlformat` containing:

```
profile = default
version = 0.24.1
```

For more information on configuration options, check out the [documentation](https://ocaml.org/p/ocamlformat) or run `ocamlformat --help`.

The most convenient way to format your code is through the `dune` build system. Just run the following command:

```
dune fmt
```

## 🤝 Contributing

We welcome contributions to ocamlformat! If you find a bug or want to suggest a feature, please open an issue on the [GitHub repository](https://github.com/ocaml-ppx/ocamlformat). If you want to contribute code, please follow the [contributing guidelines](./CONTRIBUTING.md) and open a pull request.

## 📜 License

ocamlformat is released under the [MIT License](./LICENSE.md).
