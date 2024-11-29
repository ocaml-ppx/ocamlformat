<h1 align="center">
  <a href="https://ocaml.org/p/ocamlformat/latest">
    ocamlformat
  </a>
</h1>

<p align="center">
  <strong>OCaml Code Formatter.</strong>
</p>

<p align="center">
  <a href="https://ocaml.ci.dev/github/ocaml-ppx/ocamlformat">
    <img src="https://img.shields.io/endpoint?url=https://ocaml.ci.dev/badge/ocaml-ppx/ocamlformat/main&logo=ocaml" alt="OCaml-CI Build Status" />
  </a>
</p>

**ocamlformat** is a code formatter for OCaml. It comes with opinionated default settings but is also fully customizable to suit your coding style.

- **Profiles:** ocamlformat offers profiles we predefined formatting configurations. Profiles include `default`, `ocamlformat`, `janestreet`.
- **Configurable:** Users can change the formatting profile and configure every option in their `.ocamlformat` configuration file.
- **Format Comments:** ocamlformat can format comments, docstrings, and even code blocks in your comments.
- **RPC:** ocamlformat provides an RPC server that can bed used by other tools to easily format OCaml Code.

ocamlformat is part of the [OCaml Platform](https://ocaml.org/docs/platform), the recommended set of tools for OCaml.

## Getting Started

### Installation

To use ocamlformat, you will need [OCaml](https://ocaml.org/) and [opam](https://opam.ocaml.org/) (the OCaml package manager) installed on your system. Visit the [OCaml.org installation page](https://ocaml.org/install) for instructions.

With OCaml and opam installed, you can install ocamlformat with:

```
opam install ocamlformat
```

### Usage

Formatting your code is as easy as running:

```
ocamlformat file.ml
```

To configure ocamlformat to your liking, create an `.ocamlformat` configuration file in your project with settings like:

```
profile = default
version = 0.27.0
```

Refer to our [documentation](https://ocaml.org/p/ocamlformat/latest/doc/index.html) or use `ocamlformat --help` for a full list of configuration options.

To make formatting your code even easier, use the `dune` build system:

```
dune fmt
```

## Documentation

The full documentation for ocamlformat, including comprehensive user and API guides, can be found on [OCaml.org](https://ocaml.org/p/ocamlformat/latest/doc/index.html).

## Contributing

### [Contributing Guide](CONTRIBUTING.md)

We wholeheartedly welcome contributors! To start, please read our [Contributing Guide](CONTRIBUTING.md) to familiarize yourself with our development process, including how to propose and how to start hacking on ocamlformat.

### [Hacking Guide](HACKING.md)

In addition to the Contributing Guide, we provide a [Hacking Guide](HACKING.md) for ocamlformat developers. It contains in-depth explanation of development processes, such as running tests, releasing ocamlformat, etc.

### [Code of Conduct][coc]

In order to foster a welcoming and respectful community, ocamlformat has adopted the [OCaml Code of Conduct](coc).

[coc]: https://ocaml.org/policies/code-of-conduct

### [Roadmap](ROADMAP.md)

Interested in the future of ocamlformat? Take a look at our [Roadmap](ROADMAP.md) to understand our vision and planned advancements for ocamlformat.

### [Discussions][discussions]

For conversations on ongoing development, be sure to visit the [ocamlformat][discussions] section of the OCaml Discuss forum.

[discussions]: https://discuss.ocaml.org/tag/ocamlformat

## License

ocamlformat is distributed under the terms of the MIT License. See the [LICENSE](LICENSE) file for complete details.

## Acknowledgments

We gratefully acknowledge the individuals and organizations that have significantly contributed to ocamlformat.

ocamlformat owes its existance to its initial author, [Josh Berdine](https://github.com/jberdine). We also want to express our appreciation to [Hugo Heuzard](https://github.com/hhugo), [Guillaume Petiot](https://github.com/gpetiot), and [Jules Aguillon](https://github.com/Julow). Their significant contributions have been instrumental to the advancement of ocamlformat.

Moreover, our gratitude extends to Facebook, who fostered the inception of ocamlformat as part of their work on the [ReasonML](https://reasonml.github.io/) project. [Jane Street](https://www.janestreet.com/) and [Tarides](https://tarides.com/) have also provided financial support and contributed to the ongoing development of the project.
