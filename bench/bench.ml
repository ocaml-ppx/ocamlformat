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

let source_from_file f =
  Stdio.In_channel.with_file f ~f:Stdio.In_channel.input_all

let inputs =
  let source_ml = source_from_file "test/passing/tests/source.ml" in
  [ { name= "format:conventional"
    ; input_name= "source.ml"
    ; kind= Syntax.Structure
    ; source= source_ml
    ; conf= Conf.default_profile
    ; action= `Format }
  ; { name= "numeric:conventional"
    ; input_name= "source.ml"
    ; kind= Syntax.Structure
    ; source= source_ml
    ; conf= Conf.default_profile
    ; action= `Numeric (100, 200) } ]

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
  let results = benchmark () in
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      results
  in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err
