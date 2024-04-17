opam monorepo lock &&
opam monorepo pull &&
rm *.opam &&
rm dune-project &&
mv no-deps-dune-project dune-project &&
dune exec release_version_renamer/main.exe -- $(find -name "dune" -not -path "./_build/*") dune-project &&
dune build;