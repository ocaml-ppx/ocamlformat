open! Base
open Ocamlformat

let normalize_eol = Eol_compat.normalize_eol ~line_endings:`Lf

let test_parse_and_format kind_name ~fg test_name ~input ~expected =
  let test_name =
    Stdlib.Format.sprintf "parse_and_format %s: %s" kind_name test_name
  in
  ( test_name
  , `Quick
  , fun () ->
      let actual =
        Translation_unit.parse_and_format fg ~input_name:"<test>"
          ~source:input Conf.default
        |> Result.map_error ~f:(fun e ->
               Translation_unit.Error.print Stdlib.Format.str_formatter e ;
               Stdlib.Format.flush_str_formatter () )
      in
      let expected = Result.map_error expected ~f:normalize_eol in
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
File "<test>", line 1, characters 3-3:
Error: Syntax error: 'end' expected
File "<test>", line 1, characters 0-3:
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
           \  val t_of_yojson : Json.t -> t\n\
           \  val yojson_of_t : t -> Json.t\n\n\
           \  val create :\n\
           \    capabilities:ServerCapabilities.t -> ?serverInfo:serverInfo \
            -> unit -> t\n\
            end\n" ) ]

let test_parse_and_format_expression =
  let make_test = test_parse_and_format "expression" ~fg:Expression in
  [ make_test "List.map" ~input:"List.map (fun x->\nx*x) [(1 + 9); 2;3] "
      ~expected:(Ok "List.map (fun x -> x * x) [ 1 + 9; 2; 3 ]\n") ]

let reindent ~source ~range indents =
  let low, high = Range.get range in
  let lines = String.split_lines source in
  let low = low - 1 and high = high - 1 in
  String.concat ~sep:"\n"
  @@ List.mapi lines ~f:(fun i line ->
         if i < low then line
         else if low <= i && i <= high then
           let indent = List.nth_exn indents (i - low) in
           let line = String.lstrip line in
           let spaces = String.make indent ' ' in
           spaces ^ line
         else line )

let test_numeric =
  let make_test name ~source ~range expected =
    let test_name = "numeric: " ^ name in
    ( test_name
    , `Quick
    , fun () ->
        let range = Range.make ~range source in
        let got =
          Translation_unit.numeric Use_file ~input_name:"_" ~source ~range
            Conf.default
          |> reindent ~source ~range
        in
        let expected = normalize_eol expected in
        Alcotest.check Alcotest.string test_name expected got )
  in
  [ make_test "empty buffer" ~source:"" ~range:(1, 1) ""
  ; make_test "last buffer line" ~source:"foo\nbar" ~range:(2, 3)
      "foo\n  bar"
  ; make_test "already formatted"
      ~source:
        {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
      ~range:(1, 7)
      {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar|}
  ; make_test "not already formatted"
      ~source:
        {|let foooooooooooo = let foooooooo = foooooooooooo in foooooooooooo

let foooooooooooo = let foooooooooooooo =
let woooooooooooooooo = koooooooooooooooo in
baaaaaar
in
hooohoooo
|}
      ~range:(1, 7)
      {|let foooooooooooo = let foooooooo = foooooooooooo in foooooooooooo

let foooooooooooo = let foooooooooooooo =
    let woooooooooooooooo = koooooooooooooooo in
    baaaaaar
  in
  hooohoooo|}
  ; make_test "with parens and begin/end"
      ~source:
        {|let x = begin
    let y =
      (if (k = x)
       then
         begin match k,v with [x; y] -> ( foo;
                                        (if (z) then foo else bar) )
         end
       else
         foo)
    in
    foooooo
  end|}
      ~range:(1, 12)
      {|let x = begin
  let y =
    (if (k = x)
       then
         begin match k,v with [x; y] -> ( foo;
          (if (z) then foo else bar) )
    end
       else
         foo)
  in
    foooooo
end|}
  ; make_test "split over multiple lines"
      ~source:{|let fooooo =
[
foooooo ;
foooooooo ;
fooooooo
]|}
      ~range:(1, 6)
      {|let fooooo =
  [
    foooooo ;
    foooooooo ;
    fooooooo
  ]|}
  ; make_test "invalid file"
      ~source:{|let foooooo =
let foooooooooooo =
(
[
fun x ->
foooooo|}
      ~range:(1, 6)
      {|let foooooo =
  let foooooooooooo =
    (
      [
        fun x ->
          foooooo|}
  ; make_test "already formatted function"
      ~source:
        {|let fmt_expressions c width sub_exp exprs fmt_expr
    (p : Params.elements_collection) =
  match c.conf.break_collection_expressions with
  | `Fit_or_vertical -> fmt_elements_collection p fmt_expr exprs
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        fmt_elements_collection ~first_sep:first_grp ~last_sep:last_grp p
          fmt_expr exprs
      in
      list_fl grps fmt_grp|}
      ~range:(7, 7)
      {|let fmt_expressions c width sub_exp exprs fmt_expr
    (p : Params.elements_collection) =
  match c.conf.break_collection_expressions with
  | `Fit_or_vertical -> fmt_elements_collection p fmt_expr exprs
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        fmt_elements_collection ~first_sep:first_grp ~last_sep:last_grp p
          fmt_expr exprs
      in
      list_fl grps fmt_grp|}
  ; make_test "partial let" ~range:(2, 14)
      ~source:
        {|   let () =
 ffff;
     hhhhhh;
        fff;
 let (quot, _rem) =
   let quot_rem n k =
     let (d, m) = (n / k, n mod k) in
     if d < 0 && m > 0 then (d+1, m-k)
else (d, m)
    in
    let quot n k = fst (quot_rem n k) in
    let rem n k = snd (quot_rem n k) in

quot, rem
|}
      {|   let () =
     ffff;
     hhhhhh;
     fff;
     let (quot, _rem) =
       let quot_rem n k =
         let (d, m) = (n / k, n mod k) in
         if d < 0 && m > 0 then (d+1, m-k)
         else (d, m)
       in
       let quot n k = fst (quot_rem n k) in
       let rem n k = snd (quot_rem n k) in

       quot, rem|}
  ; make_test "fit on 1 line" ~range:(2, 5)
      ~source:{|
let x =
  3
     in
   x + y|}
      {|
let x =
  3
in
x + y|} ]

let read_file f = Stdio.In_channel.with_file f ~f:Stdio.In_channel.input_all

let test_numeric_file =
  let _fmt_ast_source = read_file "../../lib/Fmt_ast.ml" in
  let partial_source = read_file "../passing/tests/partial.ml" in
  let inv_with_loc_source =
    read_file "../passing/tests/format_invalid_files_with_locations.ml"
  in
  let make_test name ~source ~range ~expected =
    let test_name = "numeric (file): " ^ name in
    ( test_name
    , `Quick
    , fun () ->
        let range = Range.make ~range source in
        let got =
          Translation_unit.numeric Use_file ~input_name:"_" ~source ~range
            Conf.default
        in
        Alcotest.check Alcotest.(list int) test_name expected got )
  in
  [ make_test "partial.ml (1-14)" ~source:partial_source ~range:(1, 14)
      ~expected:[0; 2; 2; 2; 2; 4; 6; 6; 6; 4; 4; 4; 0; 4]
  ; make_test "partial.ml (2-14)" ~source:partial_source ~range:(2, 14)
      ~expected:[5; 5; 5; 5; 7; 9; 9; 9; 7; 7; 7; 0; 7]
  ; make_test "format_invalid_files_with_locations.ml (7-11)" ~range:(7, 11)
      ~source:inv_with_loc_source ~expected:[21; 21; 0; 21; 21]
  ; make_test "format_invalid_files_with_locations.ml (10-11)"
      ~range:(10, 11) ~source:inv_with_loc_source ~expected:[0; 0] ]

let tests =
  List.concat
    [ test_parse_and_format_signature @ test_parse_and_format_use_file
      @ test_parse_and_format_core_type @ test_parse_and_format_module_type
      @ test_parse_and_format_expression @ test_numeric @ test_numeric_file
    ]
