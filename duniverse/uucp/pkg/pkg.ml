#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let uunf = Conf.with_pkg "uunf"
let cmdliner = Conf.with_pkg "cmdliner"

let uucp_api =
  [ "Uucp";
    "Uucp__age";
    "Uucp__alpha";
    "Uucp__block";
    "Uucp__break";
    "Uucp__case";
    "Uucp__case_fold";
    "Uucp__case_map";
    "Uucp__case_nfkc";
    "Uucp__case_nfkc_simple";
    "Uucp__cjk";
    "Uucp__emoji";
    "Uucp__func";
    "Uucp__gc";
    "Uucp__gen";
    "Uucp__hangul";
    "Uucp__id";
    "Uucp__name";
    "Uucp__num";
    "Uucp__script";
    "Uucp__white"; ]

let () =
  Pkg.describe "uucp" @@ fun c ->
  let uunf = Conf.value c uunf in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib ~api:uucp_api "src/uucp.mllib";
       Pkg.bin ~cond:(uunf && cmdliner) "test/ucharinfo";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/unicode.mld" ~dst:"odoc-pages/unicode.mld";
       Pkg.doc "test/examples.ml"; ]
