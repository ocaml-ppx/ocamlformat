****************************
Virtual libraries & variants
****************************

Virtual libraries correspond to dune's ability to compile parameterized
libraries and delay the selection of concrete implementations until linking an
executable.

The feature introduces two kinds of libraries: virtual and implementations. A
*virtual library* corresponds to an interface (although it may contain partial
implementation). An *implementation* of a virtual library fills in all
unimplemented modules in the virtual library.

The benefit of this partition is that other libraries may depend and compile
against the virtual library and only select concrete implementations for these
virtual libraries when linking executables. An example where this might be
useful would be a virtual, cross platform, ``clock`` library. This library would
have ``clock.unix`` and ``clock.win`` implementations. Executable using
``clock`` or libraries that use ``clock`` would conditionally select one of the
implementations, depending on the target platform.

Virtual library
===============

To define a virtual library, a ``virtual_modules`` field must be added to an
ordinary library stanza and the version of the dune language must be at least
1.5. This field defines modules for which only an interface would be present
(mli only):

.. code:: scheme

   (library
    (name clock)
    ;; clock.mli must be present, but clock.ml must not be
    (virtual_modules clock))

Apart from this field, the virtual library is defined just like a normal library
and may use all the other fields. A virtual library may include other modules
(with or without implementations), which is why it's not a pure "interface"
library.

Note: the ``virtual_modules`` field is not merged in ``modules``, which
represents the total set of modules in a library. If a directory has more than
one stanza and thus a ``modules`` field must be specified, virtual modules
still need to be added in ``modules``.

Implementation
===============

An implementation for a library is defined as:

.. code:: scheme

   (library
    (name clock_unix)
    ;; clock.ml must be present, but clock.mli must not be
    (implements clock))

The ``name`` field is slightly different for an implementation than it is for a
normal library. The ``name`` is just an internal name to refer to the
implementation, it does not correspond to any particular module like it does in
the virtual library.

Other libraries may then depend on the virtual library as if it was a regular
library:

.. code:: scheme

   (library
    (name calendar)
    (libraries clock))

But when it comes to creating an executable, we must now select a valid
implementation for every virtual library that we've used:

.. code:: scheme

   (executable
    (name birthday-reminder)
    (libraries
     clock_unix ;; leaving this dependency will make dune loudly complain
     calendar))

.. _dune-variants:

Variants
========

Variants were an experimental feature that were removed in dune 2.6.

Default implementation
======================

A virtual library may select a default implementation, which is enabled after
variant resolution, if no suitable implementation has been found.

.. code:: scheme

   (library
    (name time)
    (virtual_modules time)
    (default_implementation time-js))

The default implementation must live in the same package as the virtual library.
In the example above, that would mean that the ``time-js`` and ``time``
libraries must be in the same package

Before version 2.6, this was feature was experimental and was guarded under the
``library_variants`` language. In 2.6, this feature was promoted to the stable
language of dune and all uses of ``(using library_variants)`` are forbidden
since 2.6.

Limitations
===========

The current implementation of virtual libraries suffers from a few limitations.
Some of these are temporary.

* It is not possible to link more than one implementation for the same
  virtual library in one executable.

* It is not possible for implementations to introduce new public modules. That
  is, modules that aren't a part of the virtual library's cmi. Consequently, a
  module in an implementation either implements a virtual module or is private.

* It's not possible to load virtual libraries into utop. As a result,
  any directory that contains a virtual library will not work with ``$ dune
  utop``. This is an essential limitation, but it would be best to somehow skip
  these libraries or provide an implementation for them when loading a toplevel.

* Virtual libraries must be defined using dune. It's not possible for dune to
  implement virtual libraries created outside of dune. On the other hand,
  virtual libraries and implementations defined using dune should be usable with
  findlib based build systems.

* It is not possible for a library to be both virtual and implement another
  library. This isn't very useful, but technically, it could be used to create
  partial implementations. It is possible to lift this restriction if there's
  enough demand for this.
