(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Reapeatedly looks up properties for the whole character set. *)

let str = Format.sprintf
let exec = Filename.basename Sys.executable_name
let log fmt = Format.eprintf (fmt ^^ "%!")

let prop count mname fname prop =
  log "Lookup %s.%s for each uchar (%dx)@\n" mname fname count;
  for i = 1 to count do
    for u = 0 to 0xD7FF do ignore (prop (Uchar.unsafe_of_int u)) done;
    for u = 0xE000 to 0x10FFFF do ignore (prop (Uchar.unsafe_of_int u)) done;
  done;
  ()

let lookup_age count =
  let prop fname p = prop count "Uucp.Age" fname p in
  prop "age" Uucp.Age.age;
  ()

let lookup_alpha count =
  let prop fname p = prop count "Uucd.Alpha" fname p in
  prop "is_alphabetic" Uucp.Alpha.is_alphabetic;
  ()

let lookup_block count =
  let prop fname p = prop count "Uucd.Block" fname p in
  prop "block" Uucp.Block.block;
  ()

let lookup_break count =
  let prop fname p = prop count "Uucp.Break" fname p in
  prop "line" Uucp.Break.line;
  prop "grapheme_cluster" Uucp.Break.grapheme_cluster;
  prop "word" Uucp.Break.word;
  prop "sentence" Uucp.Break.sentence;
  prop "indic_conjunct_break" Uucp.Break.indic_conjunct_break;
  prop "east_asian_width" Uucp.Break.east_asian_width;
  ()

let lookup_case count =
  let prop fname p = prop count "Uucd.Case" fname p in
  prop "is_upper" Uucp.Case.is_upper;
  prop "is_lower" Uucp.Case.is_lower;
  prop "is_cased"  Uucp.Case.is_cased;
  prop "is_case_ignorable" Uucp.Case.is_case_ignorable;
  prop "Map.to_upper" Uucp.Case.Map.to_upper;
  prop "Map.to_lower" Uucp.Case.Map.to_lower;
  prop "Map.to_title" Uucp.Case.Map.to_title;
  prop "Fold.fold" Uucp.Case.Fold.fold;
  prop "Fold.Nfkc.fold" Uucp.Case.Nfkc_fold.fold;
  ()

let lookup_cjk count =
  let prop fname p = prop count "Uucd.Cjk" fname p in
  prop "ideographic" Uucp.Cjk.is_ideographic;
  prop "ids_unary_operator" Uucp.Cjk.is_ids_unary_operator;
  prop "ids_binary_operator" Uucp.Cjk.is_ids_binary_operator;
  prop "ids_trinary_operator" Uucp.Cjk.is_ids_trinary_operator;
  prop "radical" Uucp.Cjk.is_radical;
  prop "unified_ideograph" Uucp.Cjk.is_unified_ideograph;
  ()

let lookup_emoji count =
  let prop fname p = prop count "Uucp.Emoji" fname p in
  prop "is_emoji" Uucp.Emoji.is_emoji;
  prop "is_emoji_presentation" Uucp.Emoji.is_emoji_presentation;
  prop "is_emoji_modifier" Uucp.Emoji.is_emoji_modifier;
  prop "is_emoji_modifier_base" Uucp.Emoji.is_emoji_modifier_base;
  prop "is_emoji_component" Uucp.Emoji.is_emoji_component;
  prop "is_extended_pictographic" Uucp.Emoji.is_emoji_component;
  ()

let lookup_func count =
  let prop fname p = prop count "Uucp.Func" fname p in
  prop "is_dash" Uucp.Func.is_dash;
  prop "is_diacritic" Uucp.Func.is_diacritic;
  prop "is_extender" Uucp.Func.is_extender;
  prop "is_grapheme_base" Uucp.Func.is_grapheme_base;
  prop "is_grapheme_extend" Uucp.Func.is_grapheme_extend;
  prop "is_math" Uucp.Func.is_math;
  prop "is_quotation_mark" Uucp.Func.is_quotation_mark;
  prop "is_soft_dotted" Uucp.Func.is_soft_dotted;
  prop "is_terminal_punctuation" Uucp.Func.is_terminal_punctuation;
  prop "is_regional_indicator" Uucp.Func.is_regional_indicator;
  ()

let lookup_gc count =
  let prop fname p = prop count "Uucp.Gc" fname p in
  prop "general_category" Uucp.Gc.general_category;
  ()

let lookup_gen count =
  let prop fname p = prop count "Uucp.Gen" fname p in
  prop "is_default_ignorable" Uucp.Gen.is_default_ignorable;
  prop "is_deprecated" Uucp.Gen.is_deprecated ;
  prop "is_logical_order_exception" Uucp.Gen.is_logical_order_exception;
  prop "is_non_character" Uucp.Gen.is_non_character;
  prop "is_variation_selector" Uucp.Gen.is_variation_selector;
  ()

let lookup_hangul count =
  let prop fname p = prop count "Uucp.Hangul" fname p in
  prop "syllable_type" Uucp.Hangul.syllable_type;
  ()

let lookup_id count =
  let prop fname p = prop count "Uucp.Id" fname p in
  prop "is_id_start" Uucp.Id.is_id_start;
  prop "is_id_continue" Uucp.Id.is_id_continue;
  prop "is_xid_start" Uucp.Id.is_xid_start;
  prop "is_xid_continue" Uucp.Id.is_xid_continue;
  prop "is_pattern_syntax" Uucp.Id.is_pattern_syntax;
  prop "is_pattern_white_space"  Uucp.Id.is_pattern_white_space;
  ()

let lookup_name count =
  let prop fname p = prop count "Uucp.Name" fname p in
  prop "name" Uucp.Name.name;
  prop "name_alias" Uucp.Name.name_alias;
  ()

let lookup_num count =
  let prop fname p = prop count "Uucp.Num" fname p in
  prop "is_ascii_hex_digit" Uucp.Num.is_ascii_hex_digit;
  prop "is_hex_digit" Uucp.Num.is_hex_digit;
  prop "numeric_type" Uucp.Num.numeric_type;
  prop "numeric_value" Uucp.Num.numeric_value;
  ()

let lookup_script count =
  let prop fname p = prop count "Uucp.Script" fname p in
  prop "script" Uucp.Script.script;
  prop "script_extensions" Uucp.Script.script_extensions;
  ()

let lookup_white count =
  let prop fname p = prop count "Uucp.White" fname p in
  prop "is_white_space" Uucp.White.is_white_space;
  ()

let lookup count mods =
  let do_lookup m = mods = [] || List.mem m mods in
  if do_lookup `Age    then lookup_age count;
  if do_lookup `Alpha  then lookup_alpha count;
  if do_lookup `Block  then lookup_block count;
  if do_lookup `Break  then lookup_break count;
  if do_lookup `Case   then lookup_case count;
  if do_lookup `Cjk    then lookup_cjk count;
  if do_lookup `Emoji  then lookup_emoji count;
  if do_lookup `Func   then lookup_func count;
  if do_lookup `Gc     then lookup_gc count;
  if do_lookup `Gen    then lookup_gen count;
  if do_lookup `Hangul then lookup_hangul count;
  if do_lookup `Id     then lookup_id count;
  if do_lookup `Name   then lookup_name count;
  if do_lookup `Num    then lookup_num count;
  if do_lookup `Script then lookup_script count;
  if do_lookup `White  then lookup_white count;
  log "Done.@\n";
  ()

let main () =
  let usage = str
    "Usage: %s [OPTION]...\n\
     \ Tests lookup performance, without any option tests all properties.\n\
     Options:" exec
  in
  let count = ref 10 in
  let mods = ref [] in
  let add v = Arg.Unit (fun () -> mods := v :: !mods) in
  let pos p = raise (Arg.Bad ("don't know what to to with " ^ p)) in
  let options = [
    "-count", Arg.Set_int count,
    "N number of full character set traversals (default 10)";
    "-age",    add `Age, " test the Age module";
    "-alpha",  add `Alpha, " test the Alpha module";
    "-block",  add `Block, " test the Block module";
    "-break",  add `Break, " test the Break module";
    "-case",   add `Case, " test the Case module";
    "-cjk",    add `Cjk, " test the CJK module";
    "-emoji",  add `Emoji, " test the Emoji module";
    "-func",   add `Func, " test the Func module";
    "-gc",     add `Gc, " test the Gc module";
    "-gen",    add `Gen, " test the Gen module";
    "-hangul", add `Hangul, " test the Hangul module";
    "-id",     add `Id, " test the Id module";
    "-name",   add `Name, " test the Name module";
    "-num",    add `Num, " test the Num module";
    "-script", add `Script, " test the Script module";
    "-white",  add `White, " test the White module"; ]
  in
  Arg.parse (Arg.align options) pos usage;
  lookup !count !mods

let () = main ()
