rm *.opam;
dune exec release_version_renamer/main.exe -- $(find -name "dune" -not -path "./_build/*") dune-project;
dune build;