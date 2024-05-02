dune exec release_version_renamer/main.exe -- $(find -name "dune" -not -path "./_build/*") no-deps-dune-project &&
opam monorepo lock &&
opam monorepo pull &&
rm -r duniverse/bytes
mkdir duniverse/ocamlformat_vendor &&
mv lib duniverse/ocamlformat_vendor &&
mv vendor duniverse/ocamlformat_vendor &&
mv lib-only-dune-project duniverse/ocamlformat_vendor/dune-project &&
mv ocamlformat-lib.opam duniverse/ocamlformat_vendor &&
rm *.opam &&
rm dune-project &&
rm -r lib-rpc
rm -r lib-rpc-server
rm -r bin/ocamlformat-rpc
rm -r test/rpc
mv test/unit duniverse/ocamlformat_vendor/test
mv no-deps-dune-project dune-project &&
dune build