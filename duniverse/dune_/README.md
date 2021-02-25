Dune - A composable build system
================================

Dune is a build system designed for OCaml/Reason projects only. It
focuses on providing the user with a consistent experience and takes
care of most of the low-level details of OCaml compilation. All you
have to do is provide a description of your project and dune will
do the rest.

The scheme it implements is inspired from the one used inside Jane
Street and adapted to the open source world. It has matured over a
long time and is used daily by hundreds of developers, which means
that it is highly tested and productive.

Dune comes with a [manual][manual]. If you want to get started
without reading too much, you can look at the [quick start
guide][quick-start] or watch [this introduction video][video].

The [example][example] directory contains examples of projects using
dune.

[![Travis status][travis-img]][travis]
[![AppVeyor status][appveyor-img]][appveyor]
[![Release][release-img]][release]

[manual]:         https://dune.readthedocs.io/en/latest/
[quick-start]:    https://dune.readthedocs.io/en/latest/quick-start.html
[example]:        https://github.com/ocaml/dune/tree/master/example
[travis]:         https://travis-ci.org/ocaml/dune
[travis-img]:     https://travis-ci.org/ocaml/dune.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/diml/dune/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/rsxayce22e8f2jkp?svg=true
[release]:        https://github.com/ocaml/dune/releases
[release-img]:    https://img.shields.io/github/release/ocaml/dune.svg
[merlin]:         https://github.com/ocaml/merlin
[opam]:           https://opam.ocaml.org
[issues]:         https://github.com/ocaml/dune/issues
[dune-release]:   https://github.com/ocamllabs/dune-release
[video]:          https://youtu.be/BNZhmMAJarw

Overview
--------

Dune reads project metadata from `dune` files, which are either
static files in a simple S-expression syntax or OCaml scripts. It uses
this information to setup build rules, generate configuration files
for development tools such as [merlin][merlin], handle installation,
etc...

Dune itself is fast, has very low overhead and supports parallel
builds on all platforms. It has no system dependencies: all you need
to build dune and packages using dune is OCaml. You don't need
`make` or `bash` as long as the packages themselves don't use `bash`
explicitly.

In particular, one can install OCaml on Windows with a binary installer
and then use only the Windows Console to build dune and packages
using dune.

Strengths
---------

### Composable

Take n repositories that use dune, arrange them in any way on the
file system and the result is still a single repository that dune
knows how to build at once.

This make simultaneous development on multiple packages trivial.

### Gracefully handles multi-package repositories

Dune knows how to handle repositories containing several
packages. When building via [opam][opam], it is able to correctly use
libraries that were previously installed even if they are already
present in the source tree.

The magic invocation is:

```sh
$ dune build --only-packages <package-name> @install
```

### Building against several configurations at once

Dune is able to build a given source code repository against
several configurations simultaneously. This helps maintaining packages
across several versions of OCaml as you can test them all at once
without hassle.

In particular, this makes it easy to handle [cross-compilation](https://dune.readthedocs.io/en/latest/cross-compilation.html).

This feature requires [opam][opam].

Requirements
------------

Dune requires OCaml version 4.08.0 to build itself and can build OCaml
projects using ocaml 4.02.3 or greater.

Installation
------------

The recommended way to install dune is via the [opam package manager][opam]:

```sh
$ opam install dune
```

If you are new to opam, make sure to run `eval $(opam config env)` to
make `dune` available in your `PATH`. The dune binary is self
contained and relocatable, so you can safely copy it somewhere else to
make it permanently available.

You can also build it manually with:

```sh
$ make release
$ make install
```

If you do not have `make`, you can do the following:

```sh
$ ocaml bootstrap.ml
$ ./dune.exe build -p dune --profile dune-bootstrap
$ ./dune.exe install dune
```

The first command builds the `dune.exe` binary. The second builds the
additional files that are installed by dune, such as the man pages, and
the last simply installs all of that on the system.

Note that unless you ran the optional `./configure` script, you can
simply copy `dune.exe` anywhere and it will just work. `dune` is
fully relocatable and discovers its environment at runtime rather than
hard-coding it at compilation time.

Support
-------

If you have questions about dune, you can send an email to
ocaml-core@googlegroups.com or [open a ticket on GitHub][issues].


Migration from jbuilder
-----------------------

Dune was formerly known as jbuilder. Migration from jbuilder to dune is
described in the [manual](http://dune.readthedocs.io/en/latest/migration.html).

Status
------

Dune is now fairly stable and is used by the majority of packages on
opam. Note that dune retains backward compatibility with Jbuilder, and
in particular existing Jbuilder projects will continue to be buildable
with dune.
