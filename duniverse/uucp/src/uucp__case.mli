(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Case properties, mappings and foldings.

    These properties can implement Unicode's default case detection,
    case conversion and caseless equality over Unicode text, see the
    {{!Case.caseexamples}examples}.

    {b References.}
    {ul
    {- {{:http://unicode.org/faq/casemap_charprop.html#casemap}
        The Unicode case mapping FAQ.}}
    {- {{:http://www.unicode.org/charts/case/}The Unicode case mapping
       charts.}}} *)

(** {1:caseprops Case properties} *)

val is_lower : Uchar.t -> bool
(** [is_lower u] is [true] iff [u] has the
    {{:http://www.unicode.org/reports/tr44/#Lowercase}Lowercase} derived
    property. *)

val is_upper : Uchar.t -> bool
(** [is_upper u] is [true] iff [u] has the
    {{:http://www.unicode.org/reports/tr44/#Uppercase}Uppercase} derived
    property. *)

val is_cased : Uchar.t -> bool
(** [is_cased u] is [true] iff [u] has the
    {{:http://www.unicode.org/reports/tr44/#Cased}Cased} derived property. *)

val is_case_ignorable : Uchar.t -> bool
(** [is_case_ignorable] is [true] iff [u] has the
    {{:http://www.unicode.org/reports/tr44/#Case_Ignorable}Case_Ignorable}
    derived property. *)

(** {1:casemapfold Case mappings and foldings}

    These character mapping functions return [`Self]
    whenever a character maps to itself. *)

module Map = Uucp__case_map
module Fold = Uucp__case_fold
module Nfkc_fold = Uucp__case_nfkc
module Nfkc_simple_fold = Uucp__case_nfkc_simple

(** {1:caseexamples Examples}

    All these examples replace invalid UTF-8 decodes by an {!Uchar.rep}.

    {2:caseconversion Default case conversion on UTF-8 strings}

    The value [casemap_utf_8 cmap s] is the UTF-8 encoded string
    resulting from applying the character map [cmap] to every
    character of the UTF-8 encoded string [s].

{[
let cmap_utf_8 cmap s =
  let rec loop buf s i max =
    if i > max then Buffer.contents buf else
    let dec = String.get_utf_8_uchar s i in
    let u = Uchar.utf_decode_uchar dec in
    begin match cmap u with
    | `Self -> Buffer.add_utf_8_uchar buf u
    | `Uchars us -> List.iter (Buffer.add_utf_8_uchar buf) us
    end;
    loop buf s (i + Uchar.utf_decode_length dec) max
  in
  let buf = Buffer.create (String.length s * 2) in
  loop buf s 0 (String.length s - 1)
]}

    Using the function [cmap_utf_8], Unicode's default case
    conversions can be implemented with:

{[
let lowercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_lower s
let uppercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_upper s
]}

    However strictly speaking [lowercase_utf_8] is not conformant
    as it doesn't handle the context sensitive mapping of capital
    sigma U+03A3 to final sigma U+03C2.

    Note that applying Unicode's default case algorithms to a normalized
    string does not preserve its normalization form.

    {2:caselesseq Default caseless matching (equality) on UTF-8 strings}

    These examples use {!Uunf} to normalize character sequences

    Unicode canonical caseless matching (D145) is defined by
    normalizing to NFD, applying the Case_Folding mapping, normalizing
    again to NFD and test the result for binary equality:

{[
let canonical_caseless_key s =
  let buf = Buffer.create (String.length s * 3) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u -> Buffer.add_utf_8_uchar buf u; add `Await
    in
    add
  in
  let add =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u ->
        begin match Uucp.Case.Fold.fold u with
        | `Self -> to_nfd_and_utf_8 (`Uchar u)
        | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us
        end;
        add `Await
    in
    add
  in
  let rec loop buf s i max =
    if i > max then (add `End; to_nfd_and_utf_8 `End; Buffer.contents buf) else
    let dec = String.get_utf_8_uchar s i in
    add (`Uchar (Uchar.utf_decode_uchar dec));
    loop buf s (i + Uchar.utf_decode_length dec) max
  in
  loop buf s 0 (String.length s - 1)

let canonical_caseless_eq s0 s1 =
  canonical_caseless_key s0 = canonical_caseless_key s1
]}

    Unicode's caseless matching for identifiers (D147, see also
    {{:http://www.unicode.org/reports/tr31/}UAX 31}) is defined
    by normalizing to NFD, applying the NFKC_Casefold mapping and test
    the result for binary equality:

{[
let id_caseless_key s =
  let rec add buf normalizer v = match Uunf.add normalizer v with
  | `Await | `End -> ()
  | `Uchar u ->
      match Uucp.Case.Nfkc_fold.fold u with
      | `Self -> Buffer.add_utf_8_uchar buf u; add buf normalizer `Await
      | `Uchars us ->
          List.iter (Buffer.add_utf_8_uchar buf) us; add buf normalizer `Await
  in
  let rec loop buf s i max normalizer =
    if i > max then (add buf normalizer `End; Buffer.contents buf) else
    let dec = String.get_utf_8_uchar s i in
    add buf normalizer (`Uchar (Uchar.utf_decode_uchar dec));
    loop buf s (i + Uchar.utf_decode_length dec) max normalizer
  in
  let buf = Buffer.create (String.length s * 3) in
  let normalizer = Uunf.create `NFD in
  loop buf s 0 (String.length s - 1) normalizer

let id_caseless_eq s0 s1 = id_caseless_key s0 = id_caseless_key s1
]}
*)
