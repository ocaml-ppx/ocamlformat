[![Build Status](https://travis-ci.org/ocaml-ppx/ocamlformat.svg?branch=master)](https://travis-ci.org/ocaml-ppx/ocamlformat)

# OCamlFormat

OCamlFormat is a tool to format OCaml code.

OCamlFormat works by parsing source code using the OCaml compiler's standard parser, deciding where to place comments in the parse tree, and writing the parse tree and comments in a consistent style.

See the source code of OCamlFormat itself and [Infer](https://github.com/facebook/infer) for examples of the styles of code it produces.

## Table of Contents
- [FAQ for new users](#faq-for-new-users)
- [Features](#features)
  - [Overview](#overview)
  - [Code style](#code-style)
  - [Options](#options)
- [Installation](#installation)
- [Editor setup](#editor-setup)
  - [Emacs setup](#emacs-setup)
  - [Vim setup](#vim-setup)
- [Documentation](#documentation)
- [Community](#community)
- [License](#license)

## FAQ for new users

Hello, new user! Welcome! :wave:

If you are here, you are probably interested in using a formatting tool for your
code base, so that you do not have to worry about formatting it by hand, and to
speed up code review by focusing on the important parts.

Ocamlformat is probably what you are after!
But there are some things that you should know before formatting all the things.

### Should I use ocamlformat?

Ocamlformat is already being used by several projects, but it comes with some
important caveats. This FAQ should help you decide if it can work for you.

Ocamlformat is beta software.
While we do not follow [SemVer](https://semver.org/), we expect the program to change considerably before we reach version 1.0.0.
In particular, upgrading the ocamlformat package will cause your program to get reformatted.
Sometimes it is relatively pain-free, but sometimes it will make a diff in almost every file.
This can be a hard price to pay, since this means losing the corresponding git history.

If you use a custom configuration, options you rely on might also get removed in
a later release.

### What configuration should I use?

The recommended way is to use a versioned default profile, such as:

```
version=0.14.2
```

(or replace with the output of `ocamlformat --version`)

This ensures two things:
- you are using the default formatting configuration.
- the version that you use to format is recorded somewhere.
  If somebody else working on the project tries to use a different version,
  they will see an error message instead of reformatting the whole project in a
  different way.
- This check allows the running version to be more specific than the
  configurated version, but not the other way around.

### Can ocamlformat support my style?

No.
It is better to see ocamlformat as a tool to apply *a* style, rather than a tweakable tool to enforce your existing style.
There are some knobs that you can turn, such as overriding `margin` to determine the maximum line width.
But it is better not to set individual options to override what the default profile is doing.

To quote (and sed) [prettier's page on option philosophy](https://prettier.io/docs/en/option-philosophy.html):

> Ocamlformat has a few options because of history. **But we don’t want more of them.**
>
> By far the biggest reason for adopting ocamlformat is to stop all the on-going debates over styles.
>
> The more options ocamlformat has, the further from the above goal it gets. The debates over styles just turn into debates over which ocamlformat options to use.

### How to locally disable ocamlformat?

To disable the formatting of a specific toplevel item you must attach an `[@@ocamlformat "option=VAL"]` attribute to this item in the processed file, such as:

```ocaml
let do_not_touch
    (x : t)
      (y : t)
        (z : t) = [
  x; y; z
] [@@ocamlformat "disable"]
```

To disable the formatting of a specific expression you must attach an `[@ocamlformat "option=VAL"]` attribute to this expression in the processed file, such as:

```ocaml
let do_not_touch (x : t) (y : t) (z : t) = [
  x; y; z
] [@ocamlformat "disable"]
```

To disable a whole file, the preferred way is to add the name of the file to a local `.ocamlformat-ignore` file. An `.ocamlformat-ignore` file specifies files that ocamlformat should ignore. Each line in an `.ocamlformat-ignore` file specifies a filename relative to the directory containing the `.ocamlformat-ignore` file.
Shell-style regular expressions are supported. Lines starting with `#` are ignored and can be used as comments.

## Features

### Overview

OCamlFormat requires source code that meets the following conditions:

- Does not trigger warning 50 (“Unexpected documentation comment.”). For code that triggers warning 50, it is unlikely that ocamlformat will happen to preserve the documentation string attachment.

- Parses without any preprocessing, using the version of the standard ocaml (not camlp4) parser used to build ocamlformat. Attributes and extension points should be correctly preserved, but other mechanisms such as camlp4, cppo, etc. will not work.

- Is either a module implementation (`.ml`), an interface (`.mli`) or a sequence of toplevel phrases (`.mlt`). dune files in ocaml syntax also work.

Under those conditions, ocamlformat is expected to produce output equivalent to the input. As a safety check in case of bugs, prior to terminating or modifying any input file, ocamlformat enforces the following checks:

- The parse trees obtained by parsing the original and formatted files are equal up to some minor normalization (see [`Normalize`](./src/Normalize.ml)`.equal_impl` or `equal_intf`).

- The documentation strings, and their attachment, has been preserved (implicit in the parse tree check).

- The set of comments in the original and formatted files is the same up to their location.

### Code style

There are a number of preset code style profiles, selected using the `--profile` option by passing `--profile=<name>` on the command line or adding `profile = <name>` to an .ocamlformat configuration file. Each profile is a collection of settings for all options, overriding lower priority configuration of individual options. So a profile can be selected and then individual options can be overridden if desired.

The `conventional` or `default` profile aims to be as familiar and "conventional" appearing as the available options allow.

The `ocamlformat` profile aims to take advantage of the strengths of a parsetree-based auto-formatter, and to limit the consequences of the weaknesses imposed by the current implementation. This is a style which optimizes for what the formatter can do best, rather than to match the style of any existing code. Instead of familiarity, the focus is on legibility, keeping the common cases reasonably compact while attempting to avoid confusing formatting in corner cases. General guidelines that have directed the design include:

- Legibility, in the sense of making it as hard as possible for quick visual parsing to give the wrong interpretation, is of highest priority;

- Whenever possible the high-level structure of the code should be obvious by looking only at the left margin, in particular, it should not be necessary to visually jump from left to right hunting for critical keywords, tokens, etc;

- All else equal compact code is preferred as reading without scrolling is easier, so indentation or white space is avoided unless it helps legibility;

- Attention has been given to making some syntactic gotchas visually obvious.

The `compact` profile is similar to `ocamlformat` but opts for a generally more compact code style.

The `sparse` profile is similar to `ocamlformat` but opts for a generally more sparse code style.

If no profile is selected, the `conventional` one is used.

### Options

The full options' documentation is available in [ocamlformat-help.txt] and through `ocamlformat --help`.
Options can be modified by the means of:
- an .ocamlformat configuration file with an `option = VAL` line
- using the `OCAMLFORMAT` environment variable: `OCAMLFORMAT=option=VAL,...,option=VAL`
- an optional parameter on the command line
- a global `[@@@ocamlformat "option=VAL"]` attribute in the processed file
- an `[@@ocamlformat "option=VAL"]` attribute on an expression in the processed file

.ocamlformat files in the containing and all ancestor directories for each input file are used, as well as the global .ocamlformat file defined in `$XDG_CONFIG_HOME/ocamlformat`. The global .ocamlformat file has the lowest priority, then the closer the directory is to the processed file, the higher the priority.

When the option `--enable-outside-detected-project` is not set, .ocamlformat files outside of the project (including the one in `XDG_CONFIG_HOME`) are not read. The project root of an input file is taken to be the nearest ancestor directory that contains a .git or .hg or dune-project file. If no config file is found, formatting is disabled.

An `.ocamlformat-ignore` file specifies files that OCamlFormat should ignore.  Each line in an `.ocamlformat-ignore` file specifies a filename relative to the directory containing the `.ocamlformat-ignore` file. Lines starting with `#` are ignored and can be used as comments.

## Installation

OCamlFormat can be installed with `opam`:

```
opam install ocamlformat
```

Alternately, see [`ocamlformat.opam`](./ocamlformat.opam) for manual build instructions.

## Editor setup

### Disable outside project

As mentioned in the Options section, when the option `--disable-outside-detected-project` is set, .ocamlformat files outside of the project (including the one in `XDG_CONFIG_HOME`) are not read. The project root of an input file is taken to be the nearest ancestor directory that contains a .git or .hg or dune-project file. If no config file is found, then the formatting is disabled.

This feature is often the behavior you can expect from OCamlFormat when it is directly run from your text editor, so it is advised to use this option.

### Emacs setup

- add `$(opam config var share)/emacs/site-lisp` to `load-path` (as done by `opam user-setup install`)

- add `(require 'ocamlformat)` to `.emacs`

- optionally add the following to `.emacs` to bind `C-M-<tab>` to the ocamlformat command and install a hook to run ocamlformat when saving:
```
(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)))
```

To pass the option `--disable-outside-detected-project` (or `--disable`) to OCamlFormat:
- run `emacs`
- run `M-x customize-group⏎` then enter `ocamlformat⏎`
- select the Ocamlformat Enable item
- select the OCamlformat mode in the Value Menu: `Enable` (by default), `Disable` or `Disable outside detected project`
- save the buffer (`C-x C-s`) then enter `yes⏎` and exit

Other OCamlFormat options can be set in .ocamlformat configuration files.

### Vim setup

- be sure the `ocamlformat` binary can be found in PATH

- install the [Neoformat](https://github.com/sbdchd/neoformat#install) plugin

Optional: You can change the options passed to OCamlFormat (to use the option `--disable-outside-detected-project` for example), you can [customize NeoFormat](https://github.com/sbdchd/neoformat#config-optional) with:
```
let g:neoformat_ocaml_ocamlformat = {
            \ 'exe': 'ocamlformat',
            \ 'no_append': 1,
            \ 'stdin': 1,
            \ 'args': ['--disable-outside-detected-project', '--name', '"%:p"', '-']
            \ }

let g:neoformat_enabled_ocaml = ['ocamlformat']
```

## Documentation

OCamlFormat is documented in its man page and through its internal help:

* `ocamlformat --help`
* `man ocamlformat`

You can also view it [online](ocamlformat-help.txt).

## Reason

OCamlFormat is influenced by and follows the same basic design as `refmt` for [Reason](https://github.com/facebook/reason), but outputs OCaml instead of Reason.

This tool is not able to deal directly with Reason code (`*.re`/`*.rei` files),
but it is possible to first convert these files to ocaml syntax using `refmt -p
ml` and then running `ocamlformat` on this output.

## Community

* forum: <https://discuss.ocaml.org/tags/ocamlformat>
* github: <https://github.com/ocaml-ppx/ocamlformat>
* issues: <https://github.com/ocaml-ppx/ocamlformat/issues>

See [CONTRIBUTING](./CONTRIBUTING.md) for how to help out.

## License

OCamlFormat is [MIT-licensed](./LICENSE.md).
