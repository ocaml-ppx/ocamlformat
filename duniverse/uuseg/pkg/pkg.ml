#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg


let uutf = Conf.with_pkg "uutf"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "uuseg" @@ fun c ->
  let uutf = Conf.value c uutf in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib ~api:["Uuseg"; "Uuseg_string"] "src/uuseg.mllib";
       Pkg.bin ~cond:(uutf && cmdliner) "test/usegtrip";
       Pkg.test "test/test";
       Pkg.test "test/examples";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/examples.ml"; ]
