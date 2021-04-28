(* This code is in the public domain *)

let utf_8_segments seg s =
  let b = Buffer.create 42 in
  let flush_segment acc =
    let segment = Buffer.contents b in
    Buffer.clear b; if segment = "" then acc else segment :: acc
  in
  let seg = Uuseg.create (seg :> Uuseg.boundary) in
  let rec add acc v = match Uuseg.add seg v with
  | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add acc `Await
  | `Boundary -> add (flush_segment acc) `Await
  | `Await | `End -> acc
  in
  let rec uchar acc _ = function
  | `Uchar _ as u -> add acc u
  | `Malformed _ -> add acc (`Uchar Uutf.u_rep)
  in
  List.rev (flush_segment (add (Uutf.String.fold_utf_8 uchar [] s) `End))
