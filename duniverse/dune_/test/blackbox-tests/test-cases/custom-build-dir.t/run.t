  $ dune build foo --build-dir _foobar/ && find _foobar | grep -v '/[.]' | LANG=C sort
  _foobar
  _foobar/default
  _foobar/default/foo
  _foobar/log

  $ rm -rf _foobar

  $ dune build foo --build-dir .
  Error: Invalid build directory: .
  The build directory must be an absolute path or a sub-directory of the root
  of the workspace.
  [1]

  $ dune build foo --build-dir src/foo
  Error: Invalid build directory: src/foo
  The build directory must be an absolute path or a sub-directory of the root
  of the workspace.
  [1]

  $ mkdir project
  $ cp dune dune-project project/

Maybe this case should be supported?

  $ (cd project && dune build foo --build-dir ../build)
  Error: path outside the workspace: ../build from .
  [1]

Test with build directory being an absolute path

  $ X=$PWD/build
  $ (cd project && dune build foo --build-dir $X)
  $ find build | grep -v '/[.]' | LANG=C sort
  build
  build/default
  build/default/foo
  build/log

  $ rm -rf build

Test with a build directory that doesn't start with _

  $ touch pkg.opam
  $ dune build --build-dir build pkg.opam
  $ dune build --build-dir build
