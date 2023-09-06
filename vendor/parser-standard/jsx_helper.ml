open Printf
open Asttypes
open Longident
open Parsetree
open Ast_helper

let make_loc (startpos, endpos) =
  {
    Location.loc_start = startpos;
    Location.loc_end = endpos;
    Location.loc_ghost = false;
  }

let mkloc = Location.mkloc
let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d

let mkjsxexp ~loc:loc' e =
  let e = mkexp ~loc:loc' e in
  let loc = make_loc loc' in
  let pexp_attributes = [ Attr.mk ~loc { txt = "JSX"; loc } (PStr []) ] in
  { e with pexp_attributes }

let rec equal_longindent a b =
  match a, b with
  | Longident.Lident a, Longident.Lident b -> String.equal a b
  | Ldot (pa, a), Ldot (pb, b) ->
      String.equal a.txt b.txt && equal_longindent pa.txt pb.txt
  | Lapply _, _ | _, Lapply _ -> assert false
  | _ -> false

let make_jsx_element ~raise ~loc:_ ~tag ~end_tag ~props ~children () =
  let () =
    match end_tag with
    | None -> ()
    | Some (end_tag, (_, end_loc_e)) ->
        let eq =
          match tag, end_tag with
          | (`Module, _, s), (`Module, _, e) -> equal_longindent s e
          | (`Value, _, s), (`Value, _, e) -> equal_longindent s e
          | _ -> false
        in
        if not eq then
          let _, (end_loc_s, _), _ = end_tag in
          let end_loc = end_loc_s, end_loc_e in
          let _, start_loc, tag = tag in
          let tag = Longident.flatten tag |> String.concat "." in
          raise
            Syntaxerr.(
              Error
                (Unclosed
                   ( make_loc start_loc,
                     sprintf "<%s>" tag,
                     make_loc end_loc,
                     sprintf "</%s>" tag )))
  in
  let tag =
    match tag with
    | `Value, loc, txt ->
        mkexp ~loc (Pexp_ident { loc = make_loc loc; txt })
    | `Module, loc, txt ->
        let txt = Longident.Ldot (mkloc txt (make_loc loc), mkloc "createElement" (make_loc loc)) in
        mkexp ~loc (Pexp_ident { loc = make_loc loc; txt })
  in
  let props =
    let prop_exp ~loc name =
      let id = mkloc (Lident name) (make_loc loc) in
      mkexp ~loc (Pexp_ident id)
    in
    List.map
      (function
        | loc, `Prop_punned name -> Labelled name, prop_exp ~loc name
        | loc, `Prop_opt_punned name -> Optional name, prop_exp ~loc name
        | _loc, `Prop (name, expr) -> Labelled name, expr
        | _loc, `Prop_opt (name, expr) -> Optional name, expr)
      props
  in
  let unit =
    Exp.mk ~loc:Location.none
      (Pexp_construct ({ txt = Lident "()"; loc = Location.none }, None))
  in
  let props = (Labelled "children", children) :: props in
  Pexp_apply (tag, (Nolabel, unit) :: props)
