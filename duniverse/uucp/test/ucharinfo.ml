(*---------------------------------------------------------------------------
   Copyright (c) 2017 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let log_err s =
  Printf.printf "%s: %s\n%!" (Filename.basename Sys.executable_name) s

(* Char and String and char utilities *)

let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_hex_digit = function
| '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
| _ -> false

let hex_digit_value c = (* assert (is_hex_digit c) *)
  let c = Char.code c in
  if c <= 0x39 then c - 48 else
  if c <= 0x46 then c - 55 else
  c - 87

let strf = Format.asprintf
let str_starts s pre =
  let plen = String.length pre in
  let slen = String.length s in
  if plen > slen then false else
  try for i = 0 to plen - 1 do if pre.[i] <> s.[i] then raise Exit; done; true
  with Exit -> false

let str_is_ascii s =
  let max = String.length s - 1 in
  let rec loop s i =
    if i > max then true else
    if Char.code s.[i] > 0x7F then false else loop s (i + 1)
  in
  loop s 0

(* Data conversion to strings and UTF-X *)

let esc_non_ascii s =
  let b = Buffer.create 255 in
  let add_byte = function
  | c when Char.code c > 0x7F ->
      Buffer.add_string b (strf "\\x%02X" (Char.code c))
  | c -> Buffer.add_char b c
  in
  String.iter add_byte s; Buffer.contents b

let uchars_to_utf_bytes utf uchars =
  let b = Buffer.create 255 in
  let add_utf = match utf with
  | `UTF_8 -> Uutf.Buffer.add_utf_8
  | `UTF_16BE -> Uutf.Buffer.add_utf_16be
  | `UTF_16LE -> Uutf.Buffer.add_utf_16le
  in
  List.iter (add_utf b) uchars; Buffer.contents b

let uchars_to_utf_bytes_esc utf uchars =
  let utf = uchars_to_utf_bytes utf uchars in
  let b = Buffer.create 255 in
  let add_byte c = Buffer.add_string b (strf "\\x%02X" (Char.code c)) in
  String.iter add_byte utf; Buffer.contents b

let uchars_to_cps uchars =
  let to_str u = strf "U+%04X" (Uchar.to_int u) in
  String.concat " " (List.map to_str uchars)

let uchars_to_named_cps uchars =
  let to_str u = strf "%s (U+%04X)" (Uucp.Name.name u) (Uchar.to_int u) in
  String.concat " " (List.map to_str uchars)

(* Character specification *)

let str_of_spec_fmt = function
| `UTF_8 -> "UTF-8"
| `UTF_16BE -> "UTF-16BE"
| `UTF_16LE -> "UTF-16LE"
| `Uchar_esc -> "an Unicode character escape"
| `Bytes_esc -> "a byte sequence escape"
| `Guess -> "a character specification"

let uchar_of_utf utf s =
  let fold = match utf with
  | `UTF_8 -> Uutf.String.fold_utf_8
  | `UTF_16BE -> Uutf.String.fold_utf_16be
  | `UTF_16LE -> Uutf.String.fold_utf_16le
  in
  match fold (fun acc _ decode -> decode :: acc) [] s with
  | [ `Uchar u ] -> Some u
  | _ -> None

let try_uchar_of_utfs s =
  let rec try_decs s = function
  | [] -> None
  | enc :: encs ->
      match uchar_of_utf enc s with
      | None -> try_decs s encs
      | Some _ as u -> u
  in
  try_decs s [`UTF_8; `UTF_16BE; `UTF_16LE]

let parse_decimal_esc s = (* prefix has been removed from [s] *)
  let max = String.length s - 1 in
  let rec loop s i acc = match i = max with
  | true ->
      if s.[max] = ';' && Uchar.is_valid acc then Some (Uchar.of_int acc) else
      None
  | false ->
      if not (is_digit s.[i]) then None else
      loop s (i + 1) (acc * 10 + (Char.code s.[i] - 48))
  in
  loop s 0 0

let parse_hex_esc s ~finish = (* prefix has been removed from [s] *)
  let max = String.length s - 1 in
  let parse_hex s start =
    let rec loop s i acc =
      if i > max then acc, max else
      if not (is_hex_digit s.[i]) then acc, i - 1 else
      loop s (i + 1) (acc * 16 + hex_digit_value s.[i])
    in
    loop s start 0
  in
  let hex, last = parse_hex s 0 in
  let to_uchar hex =
    if Uchar.is_valid hex then Some (Uchar.of_int hex) else None
  in
  match finish with
  | `Empty -> if last = max then to_uchar hex else None
  | `Char c -> if last = max - 1 && s.[max] = c then to_uchar hex else None
  | `Maybe_surrogate ->
      if last = max then to_uchar hex else
      match 0xD800 <= hex && hex <= 0xDBFF && last <= max - 3 &&
            s.[last + 1] = '\\' && s.[last + 2] = 'u'
      with
      | false -> None
      | true ->
          let lo, last' = parse_hex s (last + 3) in
          match last' = max && 0xDC00 <= lo && lo <= 0xDFFF with
          | false -> None
          | true ->
              Some (Uchar.of_int @@
                    (((hex land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000)

let uchar_of_uchar_esc s =
  let prefixes =
    [ "U+"; "u+"; "U"; "u"; "\\u{"; "\\U{"; "\\u"; "\\U"; "&#x"; "&#" ]
  in
  match try Some (List.find (str_starts s) prefixes) with Not_found -> None with
  | None -> None
  | Some pre ->
      let pre_len = String.length pre in
      let s = String.sub s pre_len (String.length s - pre_len) in
      match pre with
      | "&#" -> parse_decimal_esc s
      | "&#x" -> parse_hex_esc s ~finish:(`Char ';')
      | "\\u" -> parse_hex_esc s ~finish:`Maybe_surrogate
      | "\\u{" | "\\U{" -> parse_hex_esc s ~finish:(`Char '}')
      | _ -> parse_hex_esc s ~finish:`Empty

let uchar_of_utf_8_bytes_esc s =
  let b = Buffer.create 255 in
  let max = String.length s - 1 in
  let parse_hex_byte s i =
    match i + 1 <= max && is_hex_digit s.[i] && is_hex_digit s.[i+1] with
    | false -> None
    | true -> Some (16 * (hex_digit_value s.[i]) + (hex_digit_value s.[i+1]))
  in
  let rec loop s i =
    if i > max then uchar_of_utf `UTF_8 (Buffer.contents b) else
    if is_white s.[i] then loop s (i + 1) else
    if i + 1 > max then None else
    let i = match s.[i] with
    | ('0' | '\\') when s.[i+1] = 'x' -> i + 2
    | 'x' -> i + 1
    | _ -> i
    in
    match parse_hex_byte s i with
    | None -> None
    | Some byte -> Buffer.add_char b (Char.chr byte); loop s (i + 2)
  in
  loop s 0

let guess_spec s = match str_is_ascii s with
| false -> try_uchar_of_utfs s
| true when String.length s = 1 -> Some (Uchar.of_int (Char.code s.[0]))
| true ->
    match uchar_of_uchar_esc s with
    | Some _ as u -> u
    | None -> uchar_of_utf_8_bytes_esc s

let parse_spec spec_fmt s = match spec_fmt with
| `UTF_8 -> uchar_of_utf `UTF_8 s
| `UTF_16BE -> uchar_of_utf `UTF_16BE s
| `UTF_16LE -> uchar_of_utf `UTF_16LE s
| `Uchar_esc -> uchar_of_uchar_esc s
| `Bytes_esc -> uchar_of_utf_8_bytes_esc s
| `Guess -> guess_spec s

(* Version *)

let unicode_version () = Printf.printf "%s\n%!" Uucp.unicode_version

(* Character information *)

type out_fmt =
  { ascii : bool;
    no_labels : bool;
    raw_bytes : bool; }

let str (pp, key) (out_fmt : out_fmt) u = strf "%a" pp (key u)
let str_bool key (out_fmt : out_fmt) u = strf "%b" (key u)
let str_int key (out_fmt : out_fmt) u = strf "%d" (key u)
let str_uchar (out_fmt : out_fmt) u = uchars_to_cps [u]
let str_str key (out_fmt : out_fmt) u = key u
let str_utf utf out_fmt u = match out_fmt.raw_bytes with
| true -> uchars_to_utf_bytes utf [u]
| false -> uchars_to_utf_bytes_esc utf [u]

let str_cps out_fmt uchars =
  let cps = uchars_to_cps uchars in
  match out_fmt.ascii with
  | true -> cps
  | false ->
      let utf_8 = uchars_to_utf_bytes `UTF_8 uchars in
      strf "%s (%s)" utf_8 cps

let str_case_map map (out_fmt : out_fmt) u =
  let uchars = match map u with `Self -> [u] | `Uchars uchars -> uchars in
  str_cps out_fmt uchars

let str_decomposition_mapping (out_fmt : out_fmt) u =
  let uchars = match Uunf.decomp u with
  | [||] -> [u]
  | a ->
      let to_uchar i u = match i = 0 with
      | true -> Uunf.d_uchar u
      | false -> Uchar.of_int u
      in
      Array.(to_list @@ mapi to_uchar a)
  in
  str_cps out_fmt uchars

let str_name_alias key (out_fmt : out_fmt) u =
  let str_alias (t, n) = strf "%s (%a)" n Uucp.Name.pp_alias_tag t in
  String.concat ", " (List.map str_alias (key u))

let str_script_extensions key (out_fmt : out_fmt) u =
  String.concat ", " (List.map (strf "%a" Uucp.Script.pp) (key u))

let all_keys = [
  `P "Age", str Uucp.Age.(pp, age);
  `P "Alphabetic", str_bool Uucp.Alpha.is_alphabetic;
  `P "Block", str Uucp.Block.(pp, block);
  (* Break *)
  `P "Line_Break", str Uucp.Break.(pp_line, line);
  `P "Grapheme_Cluster_Break",
    str Uucp.Break.(pp_grapheme_cluster, grapheme_cluster);
  `P "Word_Break", str Uucp.Break.(pp_word, word);
  `P "Sentence_Break", str Uucp.Break.(pp_sentence, sentence);
  `P "East_Asian_Width", str Uucp.Break.(pp_east_asian_width, east_asian_width);
  (* Case *)
  `P "Lowercase", str_bool Uucp.Case.is_lower;
  `P "Uppercase", str_bool Uucp.Case.is_upper;
  `P "Cased", str_bool Uucp.Case.is_cased;
  `P "Case_ignorable", str_bool Uucp.Case.is_case_ignorable;
  `P "Lowercase_Mapping", str_case_map Uucp.Case.Map.to_lower;
  `P "Uppercase_Mapping", str_case_map Uucp.Case.Map.to_upper;
  `P "Titlecase_Mapping", str_case_map Uucp.Case.Map.to_title;
  `P "Case_Folding", str_case_map Uucp.Case.Fold.fold;
  `P "NFKC_Casefold", str_case_map Uucp.Case.Nfkc_fold.fold;
  (* CJK *)
  `P "Ideographic", str_bool Uucp.Cjk.is_ideographic;
  `P "IDS_Binary_Operator", str_bool Uucp.Cjk.is_ids_bin_op;
  `P "IDS_Trinary_Operator", str_bool Uucp.Cjk.is_ids_tri_op;
  `P "Radical", str_bool Uucp.Cjk.is_radical;
  `P "Unified_Ideograph", str_bool Uucp.Cjk.is_unified_ideograph;
  (* Emoji *)
  `P "Emoji", str_bool Uucp.Emoji.is_emoji;
  `P "Emoji_Presentation", str_bool Uucp.Emoji.is_emoji_presentation;
  `P "Emoji_Modifier", str_bool Uucp.Emoji.is_emoji_modifier;
  `P "Emoji_Modifier_Base", str_bool Uucp.Emoji.is_emoji_modifier_base;
  `P "Emoji_Component", str_bool Uucp.Emoji.is_emoji_component;
  `P "Extended_Pictographic", str_bool Uucp.Emoji.is_extended_pictographic;
  (* Func *)
  `P "Dash", str_bool Uucp.Func.is_dash;
  `P "Diacritic", str_bool Uucp.Func.is_diacritic;
  `P "Extender", str_bool Uucp.Func.is_extender;
  `P "Grapheme_Base", str_bool Uucp.Func.is_grapheme_base;
  `P "Grapheme_Extend", str_bool Uucp.Func.is_grapheme_extend;
  `P "Math", str_bool Uucp.Func.is_math;
  `P "Quotation_Mark", str_bool Uucp.Func.is_quotation_mark;
  `P "Soft_Dotted", str_bool Uucp.Func.is_soft_dotted;
  `P "Terminal_Punctuation", str_bool Uucp.Func.is_terminal_punctuation;
  `P "Regional_Indicator", str_bool Uucp.Func.is_regional_indicator;
  `P "Join_Control", str_bool Uucp.Func.is_join_control;
  (* Gc *)
  `P "General_Category", str Uucp.Gc.(pp, general_category);
  (* Gen *)
  `P "Default_Ignorable_Code_Point", str_bool Uucp.Gen.is_default_ignorable;
  `P "Deprecated", str_bool Uucp.Gen.is_deprecated;
  `P "Logical_Order_Exception", str_bool Uucp.Gen.is_logical_order_exception;
  `P "Noncharacter_Code_Point", str_bool Uucp.Gen.is_non_character;
  `P "Variation_Selector", str_bool Uucp.Gen.is_variation_selector;
  (* Hangul *)
  `P "Hangul_Syllable_Type", str Uucp.Hangul.(pp_syllable_type, syllable_type);
  (* Id *)
  `P "ID_Start", str_bool Uucp.Id.is_id_start;
  `P "ID_Continue", str_bool Uucp.Id.is_id_continue;
  `P "XID_Start", str_bool Uucp.Id.is_xid_start;
  `P "XID_Continue", str_bool Uucp.Id.is_xid_continue;
  `P "Pattern_Syntax", str_bool Uucp.Id.is_pattern_syntax;
  `P "Pattern_White_Space", str_bool Uucp.Id.is_pattern_white_space;
  (* Name *)
  `P "Name", str_str Uucp.Name.name;
  `P "Name_alias", str_name_alias Uucp.Name.name_alias;
  (* Num *)
  `P "ASCII_Hex_Digit", str_bool Uucp.Num.is_ascii_hex_digit;
  `P "Hex_Digit", str_bool Uucp.Num.is_hex_digit;
  `P "Numeric_Type", str Uucp.Num.(pp_numeric_type, numeric_type);
  `P "Numeric_Value", str Uucp.Num.(pp_numeric_value, numeric_value);
  (* Script *)
  `P "Script", str Uucp.Script.(pp, script);
  `P "Script_extensions", str_script_extensions Uucp.Script.script_extensions;
  (* White *)
  `P "White_Space", str_bool Uucp.White.is_white_space;
  (* Uunf *)
  `P "Canonical_Combining_Class", str_int Uunf.ccc;
  `P "Decomposition_Mapping", str_decomposition_mapping;
  (* Non UCD keys *)
  `N "Uchar", str_uchar;
  `N "UTF-8", str_utf `UTF_8;
  `N "UTF-16BE", str_utf `UTF_16BE;
  `N "UTF-16LE", str_utf `UTF_16LE;
  `H "Tty_width_hint", str_int Uucp.Break.tty_width_hint; ]

let compare_key (k0, _) (k1, _) = match k0, k1 with
| (`P k0 | `N k0 | `H k0), (`P k1 | `N k1 | `H k1) -> compare k0 k1

let eq_key k (k', _) = match k' with
| `P k' | `N k' | `H k' ->
    String.(compare (lowercase_ascii k) (lowercase_ascii k') = 0)

let exist_key k = List.exists (eq_key k) all_keys
let find_key k = List.find (eq_key k) all_keys
let key_id (k, _) = match k with `P k | `N k | `H k -> k

let list_keys () =
  let keys = List.sort compare_key all_keys in
  let pr_key_name (n, _) = match n with
  | `P k -> Printf.printf "%s (UCD)\n" k
  | `N k -> Printf.printf "%s\n" k
  | `H k -> Printf.printf "%s (Uucp library heuristic)\n" k
  in
  List.iter pr_key_name keys

let get_keys keys = try Ok (List.map find_key keys) with
| Not_found ->
    let _, not = List.partition exist_key keys in
    match not with
    | [p] -> Error (strf "Unknown key: %s" p)
    | ps -> Error (strf "Unknown keys: %s" (String.concat ", " ps))

(* Key sets *)

let default_keys =
  [ "Name"; "Uchar"; "Age"; "Block"; "Script"; "Script_extensions";
    "General_Category"; "Decomposition_Mapping";
    "UTF-8"; "UTF-16BE"; "UTF-16LE"]

let case_keys =
  [ "Lowercase"; "Uppercase"; "Cased"; "Case_ignorable"; "Lowercase_Mapping";
    "Uppercase_Mapping"; "Titlecase_Mapping"; "Case_Folding"; "NFKC_Casefold" ]

let cjk_keys =
  [ "Ideographic"; "IDS_Binary_Operator"; "IDS_Trinary_Operator"; "Radical";
    "Unified_Ideograph" ]

let emoji_keys =
  [ "Emoji"; "Emoji_Presentation"; "Emoji_Modifier"; "Emoji_Modifier_Base";
    "Emoji_Component"; "Extended_Pictographic" ]

let id_keys =
  [ "ID_Start"; "ID_Continue"; "XID_Start"; "XID_Continue"; "Pattern_Syntax";
    "Pattern_White_Space" ]

let num_keys =
  [ "ASCII_Hex_Digit"; "Hex_Digit"; "Numeric_Type"; "Numeric_Value" ]

(* Query keys *)

let output_key out_fmt uchar (k, str) = match out_fmt.no_labels with
| true -> Printf.printf "%s\n" (str out_fmt uchar)
| false ->
    let l = match k with `P k | `N k | `H k -> k in
    Printf.printf "%s: %s\n" l (str out_fmt uchar)

let query_keys keys out_fmt uchar = match get_keys keys with
| Error _ as e -> e
| Ok keys -> List.iter (output_key out_fmt uchar) keys; Ok ()

(* Cmd *)

let ucharinfo cmd keys spec_fmt out_fmt uspec = match cmd with
| `Unicode_version -> unicode_version (); 0
| `List_keys -> list_keys (); 0
| `Query ->
    match uspec with
    | None -> log_err "No character specified."; 1
    | Some uspec ->
        match parse_spec spec_fmt uspec with
        | None ->
            log_err (strf "Could not parse %s from '%s'\n%!"
                       (str_of_spec_fmt spec_fmt) (esc_non_ascii uspec)); 1
        | Some uchar ->
            match query_keys keys out_fmt uchar with
            | Error e -> log_err e; 2
            | Ok () -> (); 0

(* Cmdline interface *)

open Cmdliner

let cmd =
  let doc = "List available keys." in
  let list_keys = `List_keys, Arg.info ["l"; "key-list"] ~doc in
  let doc = "Output supported Unicode version." in
  let unicode_version = `Unicode_version, Arg.info ["unicode-version"] ~doc in
  Arg.(value & vflag `Query [unicode_version; list_keys])

let spec_fmt =
  let spec_fmt =
    [ "UTF-8", `UTF_8; "UTF-16BE", `UTF_16BE; "UTF-16LE", `UTF_16LE;
      "uchar-esc", `Uchar_esc; "bytes-esc", `Bytes_esc; "guess", `Guess ]
  in
  let doc =
    strf "The character specification format. $(docv) must be one of %s.
          See CHARACTER SPECIFICATION for details."
      (Arg.doc_alts_enum spec_fmt)
  in
  let spec_format = Arg.enum spec_fmt in
  Arg.(value & opt spec_format `Guess &
       info ["s"; "spec-format"] ~doc ~docv:"FMT")

let out_fmt =
  let no_labels =
    let doc = "Output key values without labels." in
    Arg.(value & flag & info ["n"; "no-labels"] ~doc)
  in
  let raw_bytes =
    let doc = "Output byte sequences unescaped." in
    Arg.(value & flag & info ["b"; "raw-bytes"] ~doc)
  in
  let ascii =
    let doc = "Output information using only the US-ASCII charset." in
    Arg.(value & flag & info ["a"; "ascii" ] ~doc) in
  let out_fmt ascii no_labels raw_bytes = { ascii; no_labels; raw_bytes } in
  Term.(const out_fmt $ ascii $ no_labels $ raw_bytes)

let keys =
  let all =
    let doc = "Output value of all keys in alphabetic order." in
    Arg.(value & flag & info ["all"] ~doc)
  in
  let default =
    let doc = "Output default information."  in
    Arg.(value & flag & info ["d"; "default"] ~doc)
  in
  let case =
    let doc = "Output values of Unicode case keys." in
    Arg.(value & flag & info ["case"] ~doc)
  in
  let cjk =
    let doc = "Output values of Unicode CJK keys." in
    Arg.(value & flag & info ["cjk"] ~doc)
  in
  let emoji =
    let doc = "Output values of Unicode emoji keys." in
    Arg.(value & flag & info ["emoji"] ~doc)
  in
  let id =
    let doc = "Output values of Unicode identifier keys." in
    Arg.(value & flag & info ["id"] ~doc)
  in
  let num =
    let doc = "Output values of Unicode numeric keys." in
    Arg.(value & flag & info ["num"] ~doc)
  in
  let keys =
    let doc = "Output the value of $(docv). Use $(b,-l) to list available keys."
    in
    Arg.(value & opt_all string [] & info ["k"; "key"] ~doc ~docv:"KEY")
  in
  let choose all default case cjk emoji id num keys = match all with
  | true -> List.sort compare (List.map key_id all_keys)
  | false ->
      let keys = if num then num_keys @ keys else keys in
      let keys = if id then id_keys @ keys else keys in
      let keys = if cjk then cjk_keys @ keys else keys in
      let keys = if emoji then emoji_keys @ keys else keys in
      let keys = if case then case_keys @ keys else keys in
      let keys = if default then default_keys @ keys else keys in
      if keys = [] then default_keys else keys
  in
  Term.(const choose $ all $ default $ case $ cjk $ emoji $ id $ num $ keys)

let uspec =
  let doc = "The character specification. See CHARACTER SPECIFICATION
             for details."
  in
  Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"UCHAR")

let doc = "Query Unicode character information"
let exits =
  Term.exit_info 1 ~doc:"if the character specification is invalid." ::
  Term.exit_info 2 ~doc:"if a key doesn't exist." ::
  Term.default_exits

let man = [
  `S Manpage.s_description;
  `P "$(mname) outputs information about an Unicode character specified
      on the command line.";
  `P "The information to output is selected by specifying keys, use
      the $(b,-l) option to list them. By default the tool outputs a
      selection of keys for a character. To output all its keys in
      alphabetic order use the $(b,--all) option. To output specific
      keys use the repeatable $(b,-k) option.";
  `S "CHARACTER SPECIFICATION";
  `P "The character specification must represent an Unicode scalar
      value, that is a code point in the range U+0000..U+D7FF or
      U+E000..U+10FFFF. $(mname) errors on the textually meaningless range
      U+D800..U+DFFF of surrogate code points.";
  `P "You need to make sure the character specification is passed in
      a single command line argument by adding appropriate shell quotes if
      needed. Then, unless the $(b,--spec-format) option is used the following
      heuristic is applied, in order, to guess the input format:";
  `I ("1. UTF-X decode.",
      "If the specification has a non US-ASCII byte, the byte sequence
       is decoded as UTF-8, UTF-16BE or UTF-16LE (in that order)
       to a single Unicode character, taking the first decode that succeeds.");
  `I ("2. US-ASCII character.",
      "If the specification is made of a single US-ASCII character, the
       corresponding Unicode character is used.");
  `I ("3. Unicode escapes.", "If all the bytes are US-ASCII and have
       one of the following Unicode escape form, the unescaped Unicode
       character is used: U+H u+H uH UH \\\\uH \\\\uH\\\\uH \\\\UH \\\\u{H}
       \\\\U{H} &#D; &#xH; with H and D respectively denoting non empty
       sequence of caseless hexadecimal and decimal digits.");
  `I ("4. UTF-8 byte sequence escapes.", "If all the bytes are US-ASCII and
       match an escaped hexadecimal byte sequence of the form:
       ((0|\\\\)?x)?hh[ ]*)+ with h denoting a caseless hexadecimal digit,
       the unescaped byte sequence is decoded from UTF-8 to a single Unicode
       character.");
  `P "If none of this succeeds the tool errors.";
  `P "For example all the following specifications, appropriately quoted
      for your shell, are acceptable specifications for the Unicode character
      U+1F42B:";
  `Pre "  $(b,U+1F42B) $(b,u+1F42B) $(b,U1F42B) $(b,u1F42B)\n\
       \  $(b,\\\\u1F42B) $(b,\\\\u{1F42B}) $(b,\\\\U{1F42B})\n\
       \  $(b,\\\\uD83D\\\\uDC2B)        \
          # Surrogate escape (e.g. JSON/Java[Script])\n\
       \  $(b,&#x1f42b;)           # HTML/XML hexadecimal character reference\n\
       \  $(b,&#128043;)           # HTML/XML decimal character reference\n\
       \  $(b,f0 9f 90 ab)         # UTF-8 escaped byte sequence\n\
       \  $(b,F09F90AB)            # Idem\n\
       \  $(b,0xF0 0x9F 0x90 0xAB) # Idem\n\
       \  $(b,\\\\xF0\\\\x9f\\\\x90\\\\xAB)    # Idem";
  `S Manpage.s_examples;
  `Pre "  $(mname) $(b,--all f09F90ab)     # \
          All keys of this UTF-8 byte sequence\n\
       \  $(mname) $(b,-k utf-8 U+1F42B)       # Escaped UTF-8 for U+1F42B\n\
       \  $(mname) $(b,-k utf-8 -n -b U+1F42B) # UTF-8 for U+1F42B\n\
       \  $(mname) $(b,-k name U+1F42B)        # Name of U+1F42B\n\
       \  $(mname) $(b,-k name '\\\\uD83D\\\\uDC2B')   # Idem\n\
       \  $(mname) $(b,-k name -k age U+1f42B) # Name and age of U+1F42B\n\
       \  $(mname) $(b,-l)                     # List keys";
  `S Manpage.s_bugs;
  `P "This program is distributed with the Uucp OCaml library.
      See $(i,%%PKG_HOMEPAGE%%) for contact information.";
  `S "REFERENCES";
  `P "UAX #44 Unicode Character Database.
     $(i,http://www.unicode.org/reports/tr44/)"; ]

let ucharinfo =
  Term.(const ucharinfo $ cmd $ keys $ spec_fmt $ out_fmt $ uspec),
  Term.info "ucharinfo" ~version:"%%VERSION%%" ~doc ~exits ~man

let () = Term.(exit_status @@ eval ucharinfo)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The uucp programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
