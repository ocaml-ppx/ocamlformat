(* This code is in the public domain *)

(* Case conversion on UTF-8 strings *)

let cmap_utf_8 cmap s =
  let b = Buffer.create (String.length s * 2) in
  let rec add_map _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    match cmap u with
    | `Self -> Uutf.Buffer.add_utf_8 b u
    | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
  in
  Uutf.String.fold_utf_8 add_map () s; Buffer.contents b

let lowercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_lower s
let uppercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_upper s

(* Canonical caseless equality on UTF-8 strings *)

let canonical_caseless_key s =
  let b = Buffer.create (String.length s * 2) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add `Await
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
  let add_uchar _ _ = function
  | `Malformed  _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  to_nfd_and_utf_8 `End;
  Buffer.contents b

let canonical_caseless_eq s0 s1 =
  canonical_caseless_key s0 = canonical_caseless_key s1

(* Caseless equality for identifiers on UTF-8 strings. *)

let id_caseless_key s =
  let b = Buffer.create (String.length s * 3) in
  let n = Uunf.create `NFD in
  let rec add v = match Uunf.add n v with
  | `Await | `End -> ()
  | `Uchar u ->
      begin match Uucp.Case.Nfkc_fold.fold u with
      | `Self -> Uutf.Buffer.add_utf_8 b u; add `Await
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us; add `Await
      end
  in
  let add_uchar _ _ = function
  | `Malformed  _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  Buffer.contents b

let id_caseless_eq s0 s1 = id_caseless_key s0 = id_caseless_key s1
