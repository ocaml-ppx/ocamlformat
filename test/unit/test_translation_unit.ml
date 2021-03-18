open! Base
open Ocamlformat_lib

let test_parse_and_format kind_name ~fg test_name ~input ~expected =
  let test_name =
    Stdlib.Format.sprintf "parse_and_format %s: %s" kind_name test_name
  in
  ( test_name
  , `Quick
  , fun () ->
      let actual =
        Translation_unit.parse_and_format fg ~input_name:"<test>"
          ~source:input Conf.default_profile
          Conf.
            {debug= false; margin_check= false; format_invalid_files= false}
        |> Result.map_error ~f:(fun e ->
               Translation_unit.Error.print Stdlib.Format.str_formatter e ;
               Stdlib.Format.flush_str_formatter () )
      in
      Alcotest.(check (result string string)) test_name expected actual )

let test_parse_and_format_signature =
  let make_test = test_parse_and_format "signature" ~fg:Signature in
  [make_test "val" ~input:"val x :\n \nint" ~expected:(Ok "val x : int\n")]

let test_parse_and_format_use_file =
  let make_test = test_parse_and_format "use_file" ~fg:Use_file in
  [make_test "let" ~input:"let x =\n\n y" ~expected:(Ok "let x = y\n")]

let test_parse_and_format_core_type =
  let make_test = test_parse_and_format "core_type" ~fg:Core_type in
  [ make_test "string" ~input:"string" ~expected:(Ok "string\n")
  ; make_test "int" ~input:"int" ~expected:(Ok "int\n")
  ; make_test "arrow" ~input:"int -> int" ~expected:(Ok "int -> int\n")
  ; make_test "arrow2" ~input:"  int    (* foo *) \n\n ->     int  (* bar *)"
      ~expected:(Ok "int (* foo *) -> int (* bar *)\n")
  ; make_test ";;" ~input:";;"
      ~expected:
        (Error
           {|test_unit: ignoring "<test>" (syntax error)
File "<test>", line 1, characters 0-2:
Error: Syntax error
|}
        ) ]

let test_parse_and_format_module_type =
  let make_test = test_parse_and_format "module_type" ~fg:Module_type in
  [ make_test "sig end" ~input:"sig end" ~expected:(Ok "sig end\n")
  ; make_test "sig end 2"
      ~input:
        "sig\n\n\
        \ val x : foo -> bar\n\
        \  (** this does something *)\n\n\
        \ val f : a -> b -> c ->\n\n\
        \ d     end"
      ~expected:
        (Ok
           "sig\n\
           \  val x : foo -> bar\n\
           \  (** this does something *)\n\n\
           \  val f : a -> b -> c -> d\n\
            end\n" )
  ; make_test "sig" ~input:"sig"
      ~expected:
        (Error
           {|test_unit: ignoring "<test>" (syntax error)
File "<test>", line 1, characters 14-14:
Error: Syntax error: 'end' expected
File "<test>", line 1, characters 11-14:
  This 'sig' might be unmatched
|}
        )
  ; make_test "full sig"
      ~input:
        "sig\n\
        \  type serverInfo =\n\
        \    InitializeResult.serverInfo = {\n\
        \    name : string;\n\
        \    version : string option;\n\
        \  }\n\
        \  val create_serverInfo :\n\
        \    name:string -> ?version:string -> unit -> serverInfo\n\
        \  type t =\n\
        \    InitializeResult.t = {\n\
        \    capabilities : ServerCapabilities.t;\n\
        \    serverInfo : serverInfo option;\n\
        \  }\n\
        \  val t_of_yojson : Json.t -> t\n\
        \  val yojson_of_t : t -> Json.t\n\
        \  val create :\n\
        \    capabilities:ServerCapabilities.t -> ?serverInfo:serverInfo -> \
         unit -> t\n\
         end"
      ~expected:
        (Ok
           "sig\n\
           \  type serverInfo = InitializeResult.serverInfo = {\n\
           \    name : string;\n\
           \    version : string option;\n\
           \  }\n\n\
           \  val create_serverInfo : name:string -> ?version:string -> \
            unit -> serverInfo\n\n\
           \  type t = InitializeResult.t = {\n\
           \    capabilities : ServerCapabilities.t;\n\
           \    serverInfo : serverInfo option;\n\
           \  }\n\n\
           \  val t_of_yojson : Json.t -> t\n\n\
           \  val yojson_of_t : t -> Json.t\n\n\
           \  val create :\n\
           \    capabilities:ServerCapabilities.t -> ?serverInfo:serverInfo \
            -> unit -> t\n\
            end\n" ) ]

let test_parse_and_format_expression =
  let make_test = test_parse_and_format "expression" ~fg:Expression in
  [ make_test "List.map" ~input:"List.map (fun x->\nx*x) [(1 + 9); 2;3] "
      ~expected:(Ok "List.map (fun x -> x * x) [ 1 + 9; 2; 3 ]\n") ]

let tests =
  List.concat
    [ test_parse_and_format_signature @ test_parse_and_format_use_file
      @ test_parse_and_format_core_type @ test_parse_and_format_module_type
      @ test_parse_and_format_expression ]
