open Bechamel
open Toolkit
open Ocamlformat_lib

type range = int * int

type input =
  { name: string
  ; input_name: string
  ; kind: Syntax.t
  ; source: string
  ; conf: Conf.t
  ; action: [`Format | `Numeric of range] }

let inputs =
  let source_ml = Stdio.In_channel.read_all "test/source_bench.ml" in
  [ { name= "format:conventional"
    ; input_name= "source.ml"
    ; kind= Syntax.Structure
    ; source= source_ml
    ; conf= Conf.default_profile
    ; action= `Format } ]

let opts = Conf.{debug= false; margin_check= false}

let tests =
  List.map
    (fun {name; input_name; kind; source; conf; action} ->
      Test.make
        ~name:(Caml.Format.sprintf "%s (%s)" name input_name)
        ( Staged.stage
        @@ fun () ->
        match action with
        | `Format ->
            ignore
              (Translation_unit.parse_and_format kind ~input_name ~source
                 conf opts )
        | `Numeric range ->
            ignore
              (Translation_unit.numeric kind ~input_name ~source ~range conf
                 opts ) ) )
    inputs

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|]
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
  (results, raw_results)

let nothing _ = Ok ()

let () =
  let open Bechamel_js in
  let results = benchmark () in
  let dst = Channel stdout in
  let x_label = Measure.run in
  let y_label = Measure.label Instance.monotonic_clock in
  match emit ~dst nothing ~compare ~x_label ~y_label results with
  | Ok () -> ()
  | Error (`Msg err) -> invalid_arg err
