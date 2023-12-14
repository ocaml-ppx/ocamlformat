(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

open Bechamel
open Toolkit
open Ocamlformat_lib

type input =
  { name: string
  ; input_name: string
  ; kind: Syntax.t
  ; source: string
  ; conf: Conf.t
  ; action: [`Format] }

let inputs =
  let dir = "_build/default/bench/test" in
  let source_ml = Stdio.In_channel.read_all (dir ^ "/source_bench.ml") in
  [ { name= "format:conventional"
    ; input_name= "source.ml"
    ; kind= Syntax.Structure
    ; source= source_ml
    ; conf= Conf.default
    ; action= `Format } ]

let tests =
  List.map
    (fun {name; input_name; kind; source; conf; action} ->
      Test.make
        ~name:(Format.sprintf "%s (%s)" name input_name)
        ( Staged.stage
        @@ fun () ->
        match action with
        | `Format ->
            ignore
              (Translation_unit.parse_and_format kind ~input_name ~source
                 conf ) ) )
    inputs

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:false ~predictors:Measure.[|run|]
  in
  let instances =
    Instance.[minor_allocated; major_allocated; monotonic_clock]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let raw_results =
    Benchmark.all cfg instances
      (Test.make_grouped ~name:"ocamlformat" ~fmt:"%s %s" tests)
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  results

type 'a result = (string, 'a) Hashtbl.t

type 'a results = (string, 'a result) Hashtbl.t

let process_results results =
  let metrics_by_test = Hashtbl.create 16 in
  Hashtbl.iter
    (fun metric_name result ->
      Hashtbl.iter
        (fun test_name ols ->
          let metrics =
            try Hashtbl.find metrics_by_test test_name
            with Not_found -> Hashtbl.create 16
          in
          Hashtbl.add metrics metric_name ols ;
          Hashtbl.replace metrics_by_test test_name metrics )
        result )
    results ;
  metrics_by_test

let json_of_ols ols =
  match Bechamel.Analyze.OLS.estimates ols with
  | Some [x] -> `Float x
  | Some estimates -> `List (List.map (fun x -> `Float x) estimates)
  | None -> `List []

let json_of_ols_results ?name (results : Bechamel.Analyze.OLS.t results) :
    Yojson.Safe.t =
  let metrics_by_test = process_results results in
  let results =
    metrics_by_test |> Hashtbl.to_seq
    |> Seq.map (fun (test_name, metrics) ->
           let metrics =
             metrics |> Hashtbl.to_seq
             |> Seq.map (fun (metric_name, ols) ->
                    (metric_name, json_of_ols ols) )
             |> List.of_seq
             |> fun bindings -> `Assoc bindings
           in
           `Assoc [("name", `String test_name); ("metrics", metrics)] )
    |> List.of_seq
    |> fun items -> `List items
  in
  let bindings = [("results", results)] in
  let bindings =
    match name with
    | Some name -> ("name", `String name) :: bindings
    | None -> bindings
  in
  `Assoc bindings

let () =
  let results = benchmark () in
  let js_output = json_of_ols_results results in
  Format.printf "%s\n" (Yojson.Safe.to_string js_output)
