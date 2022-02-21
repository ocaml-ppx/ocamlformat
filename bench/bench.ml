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
open Ocamlformat
open Rresult

type range = int * int

type input =
  { name: string
  ; input_name: string
  ; kind: Syntax.t
  ; source: string
  ; conf: Conf.t
  ; action: [`Format | `Numeric of range] }

let inputs =
  let dir = "_build/default/bench/test" in
  let source_ml = Stdio.In_channel.read_all (dir ^ "/source_bench.ml") in
  [ { name= "format:conventional"
    ; input_name= "source.ml"
    ; kind= Syntax.Structure
    ; source= source_ml
    ; conf= Conf.default
    ; action= `Format }
  ; { name= "numeric:conventional"
    ; input_name= "source.ml"
    ; kind= Syntax.Structure
    ; source= source_ml
    ; conf= Conf.default
    ; action= `Numeric (10_000, 10_000) } ]

let test_logs = Hashtbl.create 8

let tests =
  let fmt = Format.str_formatter in
  List.map
    (fun {name; input_name; kind; source; conf; action} ->
      Test.make
        ~name:(Caml.Format.sprintf "%s (%s)" name input_name)
        ( Staged.stage
        @@ fun () ->
        let result =
          match action with
          | `Format ->
              Translation_unit.parse_and_format kind ~input_name ~source conf
              >>| fun x -> `Formatted x
          | `Numeric range ->
              let range = Range.make source ~range in
              Translation_unit.numeric kind ~input_name ~source ~range conf
              |> fun x -> Ok (`Indented x)
        in
        let () =
          match result with
          | Ok (`Formatted data) ->
              let tmp = Filename.temp_file "source.ml" ".out" in
              Stdio.Out_channel.write_all tmp ~data ;
              let cmd = Bos.Cmd.(v "diff" % input_name % tmp) in
              let output =
                match Bos.OS.Cmd.(out_string (run_out cmd)) with
                | Ok (x, _) -> x
                | Error (`Msg x) -> x
              in
              Caml.Sys.remove tmp ;
              Format.fprintf fmt "%s\n" output
          | Ok (`Indented x) ->
              let pp_sep fs () = Format.fprintf fs " " in
              Format.fprintf fmt "%a\n"
                (Format.pp_print_list ~pp_sep Format.pp_print_int)
                x
          | Error e -> Translation_unit.Error.print ~debug:true fmt e
        in
        Hashtbl.add test_logs name (Format.flush_str_formatter ()) ) )
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
  Analyze.merge ols instances results

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
  Format.printf "%s\n" (Yojson.Safe.to_string js_output) ;
  Format.printf "\n-------------------\n" ;
  Hashtbl.iter
    (fun k v ->
      Format.printf "name: %s\n" k ;
      Format.printf "output:\n%s\n\n" v )
    test_logs ;
  Hashtbl.clear test_logs
