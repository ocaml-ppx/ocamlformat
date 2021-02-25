  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (wrapped_executables false)
  > EOF

"Hello" is an instrumentation backend that instruments by printing "Hello,
Dune!" at the beginning of the module.

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries mylib)
  >  (instrumentation (backend hello)))
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (instrumentation (backend hello)))
  > EOF

  $ cat >mylib.ml <<EOF
  > let f () = ()
  > EOF

  $ cat >main.ml <<EOF
  > let () = Mylib.f ()
  > EOF

As instrumentation is disabled, this should not print the instrumentation
message.

  $ dune build
  $ _build/default/main.exe

This should print the instrumentation message twice, once for "main" and once
for "mylib":

  $ dune build --instrument-with hello
  $ _build/default/main.exe
  Hello from Mylib!
  Hello from Main!

An empty file:

  $ cat >main.ml <<EOF
  > EOF

We build the empty file.

  $ dune build

Nothing happens:

  $ _build/default/main.exe

We rebuild with instrumentation via the CLI.

  $ dune build --instrument-with hello

We get the message.

  $ _build/default/main.exe
  Hello from Main!

We also check that we can pass arguments to the ppx.

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (instrumentation (backend hello -place Spain)))
  > EOF
  $ dune build --instrument-with hello
  File "dune", line 4, characters 33-39:
  4 |  (instrumentation (backend hello -place Spain)))
                                       ^^^^^^
  Error: The possibility to pass arguments to instrumentation backends is only
  available since version 2.8 of the dune language. Please update your
  dune-project file to have (lang dune 2.8).
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ dune build --instrument-with hello
  $ _build/default/main.exe
  Hello from Spain!

Can also enable with an environment variable.

  $ DUNE_INSTRUMENT_WITH=hello dune build

  $ _build/default/main.exe
  Hello from Spain!

Instrumentation can also be controlled by using the dune-workspace file.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > EOF

  $ dune build

  $ _build/default/main.exe
  Hello from Spain!

It can also be controlled on a per-context scope.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (context (default (name coverage) (instrument_with hello)))
  > EOF

  $ dune build

  $ _build/coverage/main.exe
  Hello from Spain!

Per-context setting takes precedence over per-workspace setting.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > (context (default (name coverage) (instrument_with)))
  > EOF

  $ dune build

  $ _build/coverage/main.exe

Next, we check the backend can be used when it is installed.

  $ dune build ppx/hello.install
  $ dune install hello --prefix _install 2>/dev/null
  $ grep instrumentation.backend _install/lib/hello/dune-package
   (instrumentation.backend hello.ppx))
  $ mkdir -p installed
  $ cat >installed/dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > EOF
  $ cat >installed/dune-project <<EOF
  > (lang dune 2.7)
  > (wrapped_executables false)
  > EOF
  $ cat >installed/dune <<EOF
  > (executable
  >  (name main)
  >  (instrumentation (backend hello)))
  > EOF
  $ cat >installed/main.ml <<EOF
  > EOF
  $ OCAMLPATH=$PWD/_install/lib dune build --root installed
  Entering directory 'installed'
  $ installed/_build/default/main.exe
  Hello from Main!
