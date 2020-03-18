open Migrate_ast
open Parsetree

module Left = struct
  let rec core_type typ =
    match typ.ptyp_desc with
    | Ptyp_arrow (_, t, _) -> core_type t
    | Ptyp_tuple l -> core_type (List.hd_exn l)
    | Ptyp_object _ -> true
    | Ptyp_alias (typ, _) -> core_type typ
    | _ -> false
end

module Right = struct
  let list ~elt l = match List.last l with None -> false | Some x -> elt x

  let rec core_type = function
    | {ptyp_attributes= _ :: _; _} -> false
    | {ptyp_desc; _} -> (
      match ptyp_desc with
      | Ptyp_arrow (_, _, t) -> core_type t
      | Ptyp_tuple l -> core_type (List.last_exn l)
      | Ptyp_object _ -> true
      | _ -> false )

  let label_declaration = function
    | {pld_attributes= _ :: _; _} -> false
    | {pld_type; _} -> core_type pld_type

  let row_field = function
    | {prf_attributes= _ :: _; _} -> false
    | {prf_desc= Rinherit _; _} -> false
    | {prf_desc= Rtag (_, _, cs); _} -> (
      match List.last cs with None -> false | Some x -> core_type x )
end
