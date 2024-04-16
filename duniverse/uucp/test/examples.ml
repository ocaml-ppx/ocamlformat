(* This code is in the public domain *)

(* Case conversion on UTF-8 strings *)

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

let lowercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_lower s
let uppercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_upper s

(* Canonical caseless equality on UTF-8 strings *)

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

(* Caseless equality for identifiers on UTF-8 strings. *)

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
