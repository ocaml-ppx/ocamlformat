***
FAQ
***

Why do many dune projects contain a Makefile?
=============================================

Many dune projects contain a root ``Makefile``. It is often only there for
convenience, for the following reasons:

1. there are many different build systems out there, all with a different CLI.
   If you have been hacking for a long time, the one true invocation you know is
   ``make && make install``, possibly preceded by ``./configure``

2. you often have a few common operations that are not part of the build and
   ``make <blah>`` is a good way to provide them

3. ``make`` is shorter to type than ``dune build @install``

How to add a configure step to a dune project?
==============================================

The with-configure-step_ example shows one way to do it which
preserves composability; i.e. it doesn't require manually running ``./configure``
script when working on multiple projects at the same time.

.. _with-configure-step: https://github.com/ocaml/dune/tree/master/example/sample-projects/with-configure-step.t

Can I use topkg with dune?
==========================

It's possible using the topkg-jbuilder_ but it's not recommended. dune-release_
subsumes topkg-jbuilder and is specifically tailored to dune projects.


How do I publish my packages with dune?
=======================================

Dune is just a build system and considers publishing outside of its scope.
However, the dune-release_ project is specifically designed for releasing dune
projects to opam. We recommend using tool for publishing dune packages.

Where can I find some examples of projects using dune?
======================================================

The dune-universe_ repository contains a snapshot of the latest versions of all
opam packages depending on dune. It is therefore a useful reference to
search through to find different approaches to constructing build rules.

What is Jenga?
==============

jenga_ is a build system developed by Jane Street mainly for internal use. It
was never usable outside of Jane Street, and hence not recommended for general
use. It has no relationship to dune apart from dune being the successor to Jenga
externally. Eventually, dune is expected to replace Jenga internally at Jane
Street as well.

.. _dune-universe: https://github.com/dune-universe/dune-universe
.. _topkg-jbuilder: https://github.com/samoht/topkg-jbuilder
.. _dune-release: https://github.com/samoht/dune-release
.. _jenga: https://github.com/janestreet/jenga

How to make warnings non-fatal?
===============================

``jbuilder`` used to display warnings, but most of them would not stop the
build. But ``dune`` makes all warnings fatal by default. This can be a
challenge when porting a codebase to ``dune``. There are two ways to make warnings
non-fatal:

- the ``jbuilder`` compatibility executable works even with ``dune`` files. You
  can use it while some warnings remain, and then switch over to the ``dune``
  executable. This is the recommended way to handle the situation.
- you can pass ``--profile release`` to ``dune``. It will set up different
  compilation options that usually make sense for release builds, including
  making warnings non-fatal. This is done by default when installing packages
  from opam.
- you can change the flags that are used by the ``dev`` profile by adding the
  following stanza to a ``dune`` file:

.. code:: scheme

  (env
    (dev
      (flags (:standard -warn-error -A))))

How to display the output of commands as they run?
==================================================

When ``dune`` runs external commands, it redirects and saves their output and
displays it when they have completed. This ensures that there is no interleaving
when writing to the console.

But this might not be what the you want, for example when debugging a hanging
build.

In that case, one can pass ``-j1 --no-buffer`` so that the commands are directly
printed on the console (and the parallelism is disabled so that the output stays
readable).

How can I generate an mli file from an ml file?
===============================================

When a module starts as just an implementation (``.ml`` file), it can be tedious
to define the corresponding interface (``.mli`` file).

It is possible to use the ``ocaml-print-intf`` program (available on opam
through ``opam install ocaml-print-intf``) to generate the right ``mli`` file:

.. code:: bash

  $ dune exec -- ocaml-print-intf ocaml_print_intf.ml
  val root_from_verbose_output : string list -> string
  val target_from_verbose_output : string list -> string
  val build_cmi : string -> string
  val print_intf : string -> unit
  val version : unit -> string
  val usage : unit -> unit

It has special support for dune so it will automatically understand external
dependencies.
