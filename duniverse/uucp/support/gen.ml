(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let log fmt = Format.eprintf (fmt ^^ "%!")
let pp = Format.fprintf
let str = Format.asprintf
let pp_size ppf s =
  let b = s * (Sys.word_size / 8) in
  if b < 1_048_576 then pp ppf "%.1f Ko" (float b /. 1024.) else
  if b < 1_073_741_824 then pp ppf "%.1f Mo" (float b /. (1024. ** 2.)) else
  pp ppf "%.1f Go" (float b /. (1024. ** 3.))

let pp_uchar ppf u = pp ppf "Uchar.unsafe_of_int 0x%04X" u

let pp_list pp_v ppf vs =
  let rec loop = function
  | v :: vs -> pp ppf "@[%a@];@," pp_v v; loop vs
  | [] -> ()
  in
  pp ppf "@[<1>["; loop vs; pp ppf "]@]"

let uchar_iter_ints f =
  let rec loop u f =
    f (Uchar.to_int u);
    if Uchar.equal u Uchar.max then () else loop (Uchar.succ u) f
  in
  loop Uchar.min f

(* Generate the Unicode version *)

let pp_version ppf ucd =
  let version = match String.split_on_char ' ' ucd.Uucd.description with
  | [tok] -> tok
  | [_; tok] -> tok
  | _ -> ucd.Uucd.description
  in
  pp ppf "@[<2>let unicode_version = \"%s\"@]@\n@\n" version

(* Property lookup *)

let ucd_find ucd u p = Uucd.cp_prop ucd u p
let ucd_get ucd u p = match Uucd.cp_prop ucd u p with
| None -> invalid_arg (str "miss property for U+%04X in character database" u)
| Some v -> v

(* Generic map builders *)

let assert_prop_map prop get =
  let assert_u u =
    if prop u = get u then () else
    failwith (str "map failure for U+%04X" u)
  in
  uchar_iter_ints assert_u

let prop_map create set get prop default =
  let m = create default in
  let add_u u = set m u (prop u) in
  uchar_iter_ints add_u; m, (get m)

(* Structure sharing *)

let intern (type a) ?eqh iter pp_v ppf x =
  let module H = Hashtbl.Make (struct
    type t = a
    let equal, hash = match eqh with Some fg -> fg | _ -> (=), Hashtbl.hash
  end) in
  let t = H.create 23 and n = ref 0 in
  x |> iter (fun v -> if not (H.mem t v) then begin
    let name = str "v%03d" !n in
    H.add t v name; incr n;
    pp ppf "@[<2>let %s =@ %a@]@\n" name pp_v v
  end);
  (fun ppf v -> match H.find_opt t v with
  | Some name -> pp ppf "%s" name
  | None -> pp_v ppf v)

(* Generate Uucp_cmap.t values *)

let prop_cmap ~default prop =
  let m = ref [] in
  let add_u u = m := (`C (u, prop u)) :: !m in
  uchar_iter_ints add_u; Uucp_cmap.of_sorted_list default (List.rev !m)

let pp_prop_cmap ppf prop pname ptype pp_prop ~default size_v =
  log "* %s property, binary tree character map@\n" pname;
  let m = prop_cmap ~default prop in
  let size = Uucp_cmap.word_size size_v m in
  let h = Uucp_cmap.height m in
  log "  size (default %a): %a height: %d@\n" pp_prop default pp_size size h;
  log "  asserting"; assert_prop_map prop (Uucp_cmap.get m);
  log ", generating@\n";
  pp ppf "open Uucp_cmap@\n";
  pp ppf "@[<2>let %s_map : %s t =@ %a@]@\n@\n"
         pname ptype (Uucp_cmap.dump pp_prop) m;
  ()

let pp_prop_cmap_ucd ppf ucd prop pname ptype pp_prop ~default size_v =
  let prop u = ucd_get ucd u prop in
  pp_prop_cmap ppf prop pname ptype pp_prop ~default size_v

(* Generate Uucp_rmap.t value *)

let prop_find_ranges prop =
  let u_max = Uchar.(to_int max) in
  let current = ref None in
  let start = ref 0 in
  let ranges = ref [] in
  let rec add_u u =
    let p = prop u in
    let add_range v max =
      ranges := (`R (!start, max, v)) :: !ranges;
      current := None; add_u u
    in
    match !current with
    | None -> current := Some p; start := u
    | Some v ->
        if v = p then (if u = u_max then add_range v u) else
        add_range v (u - 1)
  in
  uchar_iter_ints add_u; (List.rev !ranges)

let pp_prop_rmap ?(share = true) ppf prop pname ptype pp_prop ~default size_v =
  log "* %s property, binary tree range code point map@\n" pname;
  let m = Uucp_rmap.of_sorted_list default (prop_find_ranges prop) in
  let size = Uucp_rmap.word_size size_v m in
  let h = Uucp_rmap.height m in
  log "  size (default %a): %a height: %d@\n" pp_prop default pp_size size h;
  log "  asserting"; assert_prop_map prop (Uucp_rmap.get m);
  log ", generating@\n";
  pp ppf "open Uucp_rmap@\n";
  let pp_prop =
    if share then intern Uucp_rmap.iter_values pp_prop ppf m else
    pp_prop
  in
  pp ppf "@[<2>let %s_map : %s t =@ %a@]@\n@\n"
    pname ptype (Uucp_rmap.dump pp_prop) m;
  ()

let pp_prop_rmap_ucd ?share ppf ucd prop pname ptype pp_prop ~default size_v =
  let prop u = ucd_get ucd u prop in
  pp_prop_rmap ?share ppf prop pname ptype pp_prop ~default size_v

(* Generate Uucp_tmap.t values *)

let prop_tmap prop default =
  prop_map
    Uucp_tmap.create
    Uucp_tmap.set
    Uucp_tmap.get
    prop default

let pp_prop_tmap ppf prop pname ptype pp_prop ~default size_v =
  log "* %s property, trie map@\n" pname;
  let m, get = prop_tmap prop default in
  let t_size = Uucp_tmap.word_size size_v m in
  log "  size (default %a): %a@\n" pp_prop default pp_size t_size;
  log "  asserting"; assert_prop_map prop get;
  log ", generating@\n";
  pp ppf "open Uucp_tmap@\n";
  pp ppf "@[<2>let %s_map : %s t =@ %a@]@\n@\n" pname ptype (Uucp_tmap.dump pp_prop) m;
  ()

let pp_prop_tmap_ucd ppf ucd prop pname ptype pp_prop ~default size_v =
  let prop u = ucd_get ucd u prop in
  pp_prop_tmap ppf prop pname ptype pp_prop ~default size_v

(* Generate Uucp_tmapbool.t value *)

let prop_tmapbools prop =
  let tm = Uucp_tmapbool.create true in
  let fm = Uucp_tmapbool.create false in
  let add_u u =
    let b = prop u in
    Uucp_tmapbool.set tm u b;
    Uucp_tmapbool.set fm u b;
  in
  uchar_iter_ints add_u; tm, fm

let assert_tmapbools prop tm fm =
  let assert_u u =
    let fail () = failwith (str "bool prop map failure for U+%04X" u) in
    let b = prop u in
    if b <> Uucp_tmapbool.get tm u then fail ();
    if b <> Uucp_tmapbool.get fm u then fail ();
  in
  uchar_iter_ints assert_u

let pp_prop_tmapbool ppf prop pname =
  log "* %s property, boolean trie map@\n" pname;
  let tm, fm = prop_tmapbools prop in
  let tm_size, fm_size =
    Uucp_tmapbool.word_size tm, Uucp_tmapbool.word_size fm
  in
  let use_fm = tm_size > fm_size in
  log "  size (default true): %a, size (default false): %a@\n"
    pp_size tm_size pp_size fm_size;
  log "  using default %b map"  (not use_fm);
  log ", asserting"; assert_tmapbools prop tm fm;
  log ", generating@\n";
  let m = if use_fm then fm else tm in
  pp ppf "open Uucp_tmapbool@\n";
  let pp_v = intern Uucp_tmapbool.iter_blobs Uucp_tmapbool.pp_v ppf m in
  pp ppf "@[<2>let %s_map =@ %a@]@\n@\n" pname (Uucp_tmapbool.dump_pp pp_v) m;
  ()

let pp_prop_tmapbool_ucd ppf ucd prop pname =
  let prop u = ucd_get ucd u prop in
  pp_prop_tmapbool ppf prop pname

(* Genenerate Uucp_tmapbyte.t values *)

let prop_tmapbyte prop default =
  prop_map
    Uucp_tmapbyte.create
    Uucp_tmapbyte.set
    Uucp_tmapbyte.get
    prop default

let pp_prop_tmapbyte ppf prop pname ~default default_str =
  log "* %s property, trie byte map@\n" pname;
  let m, get = prop_tmapbyte prop default in
  let size = Uucp_tmapbyte.word_size m in
  log " size (default %s): %a@\n" default_str pp_size size;
  log " asserting"; assert_prop_map prop get;
  log ", generating@\n";
  pp ppf "open Uucp_tmapbyte@\n";
  let pp_v = intern Uucp_tmapbyte.iter_blobs Uucp_tmapbyte.pp_v ppf m in
  pp ppf "@[<2>let %s_map : t =@ %a@]@\n@\n" pname (Uucp_tmapbyte.dump_pp pp_v) m;
  ()

let pp_prop_tmapbyte_ucd ppf ucd prop pname ~default =
  let prop u = ucd_get ucd u prop in
  pp_prop_tmapbyte ppf prop pname ~default (str "%d" default)

let pp_code_prop_tmapbyte_ucd ppf ucd code prop pname ~default pp_prop =
  let prop u = code (ucd_get ucd u prop) in
  pp_prop_tmapbyte ppf prop pname ~default:(code default)
    (str "`%a" pp_prop default)

(* Generate Uucp_tmap5bytes.t values. *)

let prop_tmap5bytes_uint20_pair prop default =
  prop_map
    Uucp_tmap5bytes.create_uint20_pair
    Uucp_tmap5bytes.set_uint20_pair
    Uucp_tmap5bytes.get_uint20_pair
    prop default

let pp_tmap5byte ppf pname m =
  pp ppf "open Uucp_tmap5bytes@\n";
  let pp_v = intern Uucp_tmap5bytes.iter_blobs Uucp_tmap5bytes.pp_v ppf m in
  pp ppf "@[<2>let %s_map : t =@ %a@]@\n@\n" pname
    (Uucp_tmap5bytes.dump_pp pp_v) m;
  ()

(* Generate a module *)

let year = (Unix.gmtime (Unix.gettimeofday ())).Unix.tm_year + 1900

let pp_mod pp_mod ppf m =
  pp ppf
"\
(*---------------------------------------------------------------------------
   Copyright (c) %d The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated. *)
@\n@[%a@]@\n" year pp_mod m
