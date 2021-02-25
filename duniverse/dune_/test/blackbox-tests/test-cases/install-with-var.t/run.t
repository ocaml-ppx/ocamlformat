`dune install` should handle destination directories that don't exist

  $ cat > dune <<EOF
  > (install
  >  (section man)
  >  (files
  >     (man-page-a.1 as man-page-a.%{context_name}.1) ; incorrect usage!
  >     (man-page-b.1 as man1/man-page-b.%{context_name}.1)
  >     another-man-page.3)
  > )
  > EOF

  $ dune build @install
  $ dune install --prefix install --libdir lib
  Installing install/lib/foo/META
  Installing install/lib/foo/dune-package
  Installing install/lib/foo/opam
  Installing install/man/man-page-a.default.1
  Installing install/man/man1/man-page-b.default.1
  Installing install/man/man3/another-man-page.3

  $ dune_cmd cat _build/default/foo.install | grep man
  man: [
    "_build/install/default/man/man-page-a.default.1" {"man-page-a.default.1"}
    "_build/install/default/man/man1/man-page-b.default.1"
    "_build/install/default/man/man3/another-man-page.3"

Some variables are restricted in [dst] of [bin] section because evaluating
them could cause a dependency cycle (also, most of them make no sense in [dst] anyway).

  $ cat > dune <<EOF
  >  (install
  >   (section bin)
  >   (files (foobar.txt as "%{env:FOO=foobar}/foo.txt"))
  >  )
  > EOF

  $ dune build @install
  File "dune", line 3, characters 27-42:
  3 |   (files (foobar.txt as "%{env:FOO=foobar}/foo.txt"))
                                 ^^^^^^^^^^^^^^^
  Error: %{env:..} isn't allowed in this position.
  [1]

This is not a problem outside of bin section:

  $ cat > dune <<EOF
  >  (install
  >   (section man)
  >   (files (foobar.txt as "%{env:FOO=foobar}/foo.txt"))
  >  )
  > EOF

  $ dune build @install

Extension of [src] can't use the restricted variables either because
addition of .exe suffix to dst on Windows is conditional on the
extension of [src]:

  $ cat > dune <<EOF
  >  (install
  >   (section bin)
  >   (files (%{env:FOO=foobar.txt} as foo.txt))
  >  )    
  > EOF

  $ dune build @install
  File "dune", line 3, characters 12-31:
  3 |   (files (%{env:FOO=foobar.txt} as foo.txt))
                  ^^^^^^^^^^^^^^^^^^^
  Error: Because this file is installed in the 'bin' section, you cannot use
  the variable %{env:..} in its basename.
  [1]

This is fine if the destination extension is already .exe:

  $ cat > dune <<EOF
  >  (install
  >   (section bin)
  >   (files (%{env:FOO=foobar.txt} as foo.exe))
  >  )    
  > EOF

  $ dune build @install

Or if the extension of source is clearly not .exe:

  $ cat > dune <<EOF
  >  (install
  >   (section bin)
  >   (files (%{env:FOO=foobar}.txt as foo))
  >  )    
  > EOF

  $ dune build @install

Exe basename needs to be fully known if dst is missing though:

  $ cat > dune <<EOF
  >  (install
  >   (section bin)
  >   (files %{env:FOO=foobar}.txt)
  >  )    
  > EOF

  $ dune build @install
  File "dune", line 3, characters 11-26:
  3 |   (files %{env:FOO=foobar}.txt)
                 ^^^^^^^^^^^^^^^
  Error: Because this file is installed in the 'bin' section, you cannot use
  the variable %{env:..} in its basename.
  [1]

When basename is fully known, all is well:

  $ cat > dune <<EOF
  >  (install
  >   (section bin)
  >   (files %{env:FOO=.}/foobar.txt)
  >  )    
  > EOF

  $ dune build @install
