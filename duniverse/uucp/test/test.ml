(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests the properties against the XML Unicode character database. *)

let str = Format.asprintf
let exec = Filename.basename Sys.executable_name
let log fmt = Format.eprintf (fmt ^^ "%!")

let uchar_dump ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u)

(* UCD loading and access *)

let load_ucd inf =
  try
    log "Loading Unicode character database.@\n";
    let inf = match inf with None -> "support/ucd.xml" | Some inf -> inf in
    let ic = if inf = "-" then stdin else open_in inf in
    let d = Uucd.decoder (`Channel ic) in
    match Uucd.decode d with
    | `Ok db -> db
    | `Error e ->
        let (l0, c0), (l1, c1) = Uucd.decoded_range d in
        log "%s:%d.%d-%d.%d: %s@\n" inf l0 c0 l1 c1 e;
        exit 1
  with Sys_error e -> log "%s@\n" e; exit 1

let ucd_get p ucd u = match Uucd.cp_prop ucd (Uchar.to_int u) p with
| None -> invalid_arg (str "no property for %a" uchar_dump u)
| Some v -> v

(* Assert properties *)

let prop ucd mname fname ucd_get prop =
  let do_assert u =
    if ucd_get ucd u = prop u then () else
    failwith (str "assertion failure on %a" uchar_dump u)
  in
  log "Asserting %s.%s@\n" mname fname;
  for u = 0 to 0xD7FF do do_assert (Uchar.of_int u) done;
  for u = 0xE000 to 0x10FFFF do do_assert (Uchar.of_int u) done;
  ()

(* Assert modules *)

let assert_age ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Age" fname (ucd_get ucd_p) p in
  prop "age" Uucd.age Uucp.Age.age;
  ()

let assert_alpha ucd =
  let prop fname ucd_p p = prop ucd "Uucd.Alpha" fname (ucd_get ucd_p) p in
  prop "is_alphabetic" Uucd.alphabetic Uucp.Alpha.is_alphabetic;
  ()

let assert_block ucd =
  let block_prop ucd u = match ucd_get Uucd.block ucd u with
  | `High_Surrogates -> assert false
  | `Low_Surrogates -> assert false
  | `High_PU_Surrogates -> assert false
  | #Uucp.Block.t as b ->
      try
        (* Test Uucp.Block.blocks at the same time *)
        let (is, ie) = List.assoc b Uucp.Block.blocks in
        if u < is || u > ie then assert false else
        b
      with Not_found -> assert (b = `NB); b
  in
  prop ucd "Uucd.Block" "block" block_prop Uucp.Block.block;
  ()

let assert_break ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Break" fname (ucd_get ucd_p) p in
  prop "line" Uucd.line_break Uucp.Break.line;
  prop "grapheme_cluster" Uucd.grapheme_cluster_break
    Uucp.Break.grapheme_cluster;
  prop "word" Uucd.word_break Uucp.Break.word;
  prop "sentence" Uucd.sentence_break Uucp.Break.sentence;
  prop "indic_conjunct_break"
    Uucd.indic_conjunct_break Uucp.Break.indic_conjunct_break;
  ()

let assert_case ucd =
  let map fname ucd_p p =
    let assert_map ucd u = match ucd_get ucd_p ucd u with
    | `Self -> `Self
    | `Cps cps -> `Uchars (List.map Uchar.of_int cps)
    in
    prop ucd "Uucd.Case" fname assert_map p
  in
  let prop fname ucd_p p = prop ucd "Uucd.Case" fname (ucd_get ucd_p) p in
  prop "is_upper" Uucd.uppercase Uucp.Case.is_upper;
  prop "is_lower" Uucd.lowercase Uucp.Case.is_lower;
  prop "is_cased" Uucd.cased  Uucp.Case.is_cased;
  prop "is_case_ignorable" Uucd.case_ignorable Uucp.Case.is_case_ignorable;
  map "Map.to_upper" Uucd.uppercase_mapping Uucp.Case.Map.to_upper;
  map "Map.to_lower" Uucd.lowercase_mapping Uucp.Case.Map.to_lower;
  map "Map.to_title" Uucd.titlecase_mapping Uucp.Case.Map.to_title;
  map "Fold.fold" Uucd.case_folding Uucp.Case.Fold.fold;
  map "Nfkc_fold.fold" Uucd.nfkc_casefold Uucp.Case.Nfkc_fold.fold;
  map "Nfkc_simple_fold.fold" Uucd.nfkc_simple_casefold
    Uucp.Case.Nfkc_simple_fold.fold;
  ()

let assert_cjk ucd =
  let prop fname ucd_p p = prop ucd "Uucd.Cjk" fname (ucd_get ucd_p) p in
  prop "ideographic" Uucd.ideographic Uucp.Cjk.is_ideographic;
  prop "ids_unary_operator"
    Uucd.ids_unary_operator Uucp.Cjk.is_ids_unary_operator;
  prop "ids_binary_operator"
    Uucd.ids_binary_operator Uucp.Cjk.is_ids_binary_operator;
  prop "ids_trinary_operator"
    Uucd.ids_trinary_operator Uucp.Cjk.is_ids_trinary_operator;
  prop "radical" Uucd.radical Uucp.Cjk.is_radical;
  prop "unified_ideograph" Uucd.unified_ideograph Uucp.Cjk.is_unified_ideograph;
  ()

let assert_emoji ucd =
  let prop fname ucd_p p = prop ucd "Uucd.Emoji" fname (ucd_get ucd_p) p in
  prop "is_emoji" Uucd.emoji Uucp.Emoji.is_emoji;
  prop "is_emoji_presentation"
    Uucd.emoji_presentation Uucp.Emoji.is_emoji_presentation;
  prop "is_emoji_modifier" Uucd.emoji_modifier Uucp.Emoji.is_emoji_modifier;
  prop "is_emoji_modifier_base"
    Uucd.emoji_modifier_base Uucp.Emoji.is_emoji_modifier_base;
  prop "is_emoji_component" Uucd.emoji_component Uucp.Emoji.is_emoji_component;
  prop "is_extended_pictographic"
    Uucd.extended_pictographic Uucp.Emoji.is_extended_pictographic;
  ()

let assert_func ucd =
  let prop fname ucd_p p = prop ucd "Uucd.Func" fname (ucd_get ucd_p) p in
  prop "is_dash" Uucd.dash Uucp.Func.is_dash;
  prop "is_diacritic" Uucd.diacritic Uucp.Func.is_diacritic;
  prop "is_extender" Uucd.extender Uucp.Func.is_extender;
  prop "is_grapheme_base" Uucd.grapheme_base Uucp.Func.is_grapheme_base;
  prop "is_grapheme_extend" Uucd.grapheme_extend Uucp.Func.is_grapheme_extend;
  prop "is_math" Uucd.math Uucp.Func.is_math;
  prop "is_quotation_mark" Uucd.quotation_mark Uucp.Func.is_quotation_mark;
  prop "is_soft_dotted" Uucd.soft_dotted Uucp.Func.is_soft_dotted;
  prop "is_terminal_punctuation" Uucd.terminal_punctuation
    Uucp.Func.is_terminal_punctuation;
  prop "is_regional_indicator" Uucd.regional_indicator
    Uucp.Func.is_regional_indicator;
  ()

let assert_gc ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Gc" fname (ucd_get ucd_p) p in
  prop "general_category" Uucd.general_category Uucp.Gc.general_category;
  ()

let assert_gen ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Gen" fname (ucd_get ucd_p) p in
  prop "is_default_ignorable" Uucd.default_ignorable_code_point
    Uucp.Gen.is_default_ignorable;
  prop "is_deprecated" Uucd.deprecated Uucp.Gen.is_deprecated ;
  prop "is_logical_order_exception" Uucd.logical_order_exception
    Uucp.Gen.is_logical_order_exception;
  prop "is_non_character" Uucd.noncharacter_code_point
    Uucp.Gen.is_non_character;
  prop "is_variation_selector" Uucd.variation_selector
    Uucp.Gen.is_variation_selector;
  ()

let assert_hangul ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Hangul" fname (ucd_get ucd_p) p in
  prop "syllable_type" Uucd.hangul_syllable_type Uucp.Hangul.syllable_type;
  ()

let assert_id ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Id" fname (ucd_get ucd_p) p in
  prop "is_id_start" Uucd.id_start Uucp.Id.is_id_start;
  prop "is_id_continue" Uucd.id_continue Uucp.Id.is_id_continue;
  prop "is_xid_start" Uucd.xid_start Uucp.Id.is_xid_start;
  prop "is_xid_continue" Uucd.xid_continue Uucp.Id.is_xid_continue;
  prop "is_id_compat_math_start"
    Uucd.id_compat_math_start Uucp.Id.is_id_compat_math_start;
  prop "is_id_compat_math_continue"
    Uucd.id_compat_math_continue Uucp.Id.is_id_compat_math_continue;
  prop "is_pattern_syntax" Uucd.pattern_syntax Uucp.Id.is_pattern_syntax;
  prop "is_pattern_white_space" Uucd.pattern_white_space
    Uucp.Id.is_pattern_white_space;
  ()

let assert_name ucd =
  let buf = Buffer.create 244 in
  let name_prop ucd u = match (ucd_get Uucd.name ucd u) with
  | `Name n -> n
  | `Pattern n ->
      Buffer.clear buf;
      for i = 0 to String.length n - 1 do
        if n.[i] = '#'
        then Buffer.add_string buf (str "%04X" (Uchar.to_int u))
        else Buffer.add_char buf n.[i]
      done;
      Buffer.contents buf
  in
  prop ucd "Uucd.Name" "name" name_prop Uucp.Name.name;
  let alias_prop ucd u =
    let permute (n, t) = (t, n) in
    List.map permute (ucd_get Uucd.name_alias ucd u)
  in
  prop ucd "Uucd.Name" "name_alias" alias_prop Uucp.Name.name_alias;
  ()

let assert_num ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Num" fname (ucd_get ucd_p) p in
  prop "is_ascii_hex_digit" Uucd.ascii_hex_digit Uucp.Num.is_ascii_hex_digit;
  prop "is_hex_digit" Uucd.hex_digit Uucp.Num.is_hex_digit;
  prop "numeric_type" Uucd.numeric_type Uucp.Num.numeric_type;
  prop "numeric_value" Uucd.numeric_value Uucp.Num.numeric_value;
  ()

let assert_script ucd =
  let prop fname ucd_p p = prop ucd "Uucp.Script" fname (ucd_get ucd_p) p in
  prop "script" Uucd.script Uucp.Script.script;
  prop "script_extensions"
    Uucd.script_extensions Uucp.Script.script_extensions;
  ()

let assert_white ucd =
  let prop fname ucd_p p = prop ucd "Uucd.White" fname (ucd_get ucd_p) p in
  prop "is_white_space" Uucd.white_space Uucp.White.is_white_space;
  ()

let test inf mods =
  let do_assert m = mods = [] || List.mem m mods in
  let ucd = load_ucd inf in
  if do_assert `Age    then assert_age ucd;
  if do_assert `Alpha  then assert_alpha ucd;
  if do_assert `Block  then assert_block ucd;
  if do_assert `Break  then assert_break ucd;
  if do_assert `Case   then assert_case ucd;
  if do_assert `Cjk    then assert_cjk ucd;
  if do_assert `Emoji  then assert_emoji ucd;
  if do_assert `Func   then assert_func ucd;
  if do_assert `Gc     then assert_gc ucd;
  if do_assert `Gen    then assert_gen ucd;
  if do_assert `Hangul then assert_hangul ucd;
  if do_assert `Id     then assert_id ucd;
  if do_assert `Name   then assert_name ucd;
  if do_assert `Num    then assert_num ucd;
  if do_assert `Script then assert_script ucd;
  if do_assert `White  then assert_white ucd;
  log "Done.@\n";
  ()

let main () =
  let usage = str
      "Usage: %s [OPTION]... [DBFILE]\n\
      \ Asserts Uucp's data against the Unicode character database DBFILE.\n\
      \ DBFILE defaults to support/ucd.xml, without any option asserts all\n\
      \ modules.\n\
       Options:" exec
  in
  let inf = ref None in
  let set_inf f =
    if !inf = None then inf := Some f else
    raise (Arg.Bad "only one Unicode character database file can be specified")
  in
  let mods = ref [] in
  let add v = Arg.Unit (fun () -> mods := v :: !mods) in
  let options = [
    "-age",    add `Age,    " assert the Age module";
    "-alpha",  add `Alpha,  " assert the Alpha module";
    "-block",  add `Block,  " assert the Block module";
    "-break",  add `Break,  " assert the Break module";
    "-case",   add `Case,   " assert the Case module";
    "-cjk",    add `Cjk,    " assert the CJK module";
    "-emoji",  add `Emoji,  " assert the Emoji module";
    "-func",   add `Func,   " assert the Func module";
    "-gc",     add `Gc,     " assert the Gc module";
    "-gen",    add `Gen,    " assert the Gen module";
    "-hangul", add `Hangul, " assert the Hangul module";
    "-id",     add `Id,     " assert the Id module";
    "-name",   add `Name,   " assert the Name module";
    "-num",    add `Num,    " assert the Num module";
    "-script", add `Script, " assert the Script module";
    "-white",  add `White,  " assert the White module"; ]
  in
  Arg.parse (Arg.align options) set_inf usage;
  test !inf !mods

let () = main ()
