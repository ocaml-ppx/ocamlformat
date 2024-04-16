(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The name property being distinct for most scalar value it doesn't
   compress well using any of Uucp's maps, especially on 64-bit
   platforms, where a pointer costs 8 bytes. To give an idea having
   one pointer per scalar value already uses around 8 Mo of memory
   ((0x10_FFFF - 0x07FF) * 8 / (1024 * 1024)).

   For a more efficient encoding of names we cut the name property of
   a scalar value in two trying to share the prefix with the name of
   the two next scalar value. This results in two tokens. Tokens are
   stored uniquely in a large strings separated by \x00 bytes. To map
   a scalar value to a name we use a trie that maps scalars values to
   5 bytes stored as string chunks (see uucp_tmap5bytes.ml). These
   five bytes encode two 20-bit unsigned integers which are the
   offsets of the two tokens making up the name in the string of
   tokens.

   The lookup procedure simply gets the two 20-bit integers from the
   trie map, looks up the corresponding tokens in the string of tokens
   concatenates them to form the name. We also keep the pattern naming
   mechanism inherited from the UCD XML for CJK names (see prop in
   pp_name below and Uucp_name.name). *)

let split_in_two_tokens ~other ~other' n =
  let find_right_sp s = try String.rindex s ' ' with Not_found -> 0 in
  let find_prefix_len s0 s1 =
    let len_s0 = String.length s0 in
    let len_s1 = String.length s1 in
    let max_idx = if len_s0 < len_s1 then len_s0 - 1 else len_s1 -1 in
    let rec loop i = if i > max_idx || s0.[i] <> s1.[i] then i else loop (i + 1)
    in loop 0
  in
  let n_len = String.length n in
  let cut_len = find_prefix_len other n in
  let cut_len' = find_prefix_len other' n in
  let cut_len = max cut_len cut_len' in
  let cut_len = if cut_len < 4 then find_right_sp n else cut_len in
  let cut_len =
    (* This refinement makes slightly less tokens and space *)
    if cut_len <> 0 &&  n.[cut_len - 1] = ' ' then cut_len - 1 else cut_len
  in
  String.sub n 0 cut_len, String.sub n cut_len (n_len - cut_len)

let has_final_sharp s =
  try ignore (String.index s '#' = String.length s - 1); true with
  | Not_found -> false

let name_prop ucd =
  let tok_buffer = Buffer.create (1024 * 1024) in
  let tok_index = Hashtbl.create 30_000 in
  let tok_offset = ref 0 in
  let get_tok_idx tok = try Hashtbl.find tok_index tok with
  | Not_found ->
      let offset = !tok_offset in
      Hashtbl.add tok_index tok offset;
      Buffer.add_string tok_buffer tok;
      Buffer.add_char tok_buffer '\x00';
      tok_offset := !tok_offset + String.length tok + 1;
      offset
  in
  let prop u = match Gen.ucd_get ucd u Uucd.name with
  | `Name n ->
      assert (not (has_final_sharp n));
      let u = Uchar.unsafe_of_int u in
      let bound_succ u =
        if Uchar.equal u Uchar.max then Uchar.max else Uchar.succ u
      in
      let u' = bound_succ u in
      let u'' = bound_succ u' in
      let other = match Gen.ucd_get ucd (Uchar.to_int u') Uucd.name with
      | `Name n -> n | `Pattern n -> n
      in
      let other' = match Gen.ucd_get ucd (Uchar.to_int u'') Uucd.name with
      | `Name n -> n | `Pattern n -> n
      in
      let l, r = split_in_two_tokens ~other ~other' n in
      (* empty on the left is used for patterns so don't let this happen *)
      let l, r = if l = "" then r, "" else l, r in
      get_tok_idx l, get_tok_idx r
  | `Pattern n ->
      assert (has_final_sharp n);
      (* empty on the left and non-empty on right implies pattern *)
      get_tok_idx "", get_tok_idx (String.sub n 0 (String.length n - 1))
  in
  ignore (get_tok_idx ""); (* assign idx 0 to "" *)
  let default = (0, 0) in
  Buffer.contents tok_buffer,
  Hashtbl.length tok_index,
  prop, Gen.prop_tmap5bytes_uint20_pair prop default

let pp_name ppf ucd =
  Gen.log "* name property, 5 bytes trie map and token table@\n";
  let toks, tok_count, prop, (m, get) = name_prop ucd in
  let size = Uucp_tmap5bytes.word_size m in
  Gen.log "  size: %a" Gen.pp_size size;
  Gen.log " token count: %d tokens size: %a@\n"
    tok_count Gen.pp_size (String.length toks / (Sys.word_size / 8));
  Gen.log "  asserting"; Gen.assert_prop_map prop get;
  Gen.log ", generating@\n";
  Gen.pp ppf "@[<2>let name_toks : string =@ %a@]@\n@\n" Uucp_fmt.string_X toks;
  Gen.pp_tmap5byte ppf "name" m;
  ()

let pp_name_alias ppf ucd =
  let size v =
    3 * (List.length v) +
    List.fold_left (fun acc (_, n) -> 3 + String.length n) 0 v
  in
  let pp_tag ppf t = Gen.pp ppf "`%a" Uucp_name_base.pp_alias_tag t in
  let pp_alias ppf (t, n) = Gen.pp ppf "@[<1>(%a,@,%S)@]" pp_tag t n in
  let pp_alist = Gen.pp_list pp_alias in
  let prop u =
    let permute (n, t) = (t, n) in
    List.map permute (Gen.ucd_get ucd u Uucd.name_alias)
  in
  Gen.pp_prop_cmap ppf prop
    "name_alias" "(Uucp_name_base.alias_tag * string) list"
    pp_alist ~default:[] size

let pp_props ppf ucd =
  pp_name ppf ucd;
  pp_name_alias ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
