/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The parser definition */

/* The commands [make list-parse-errors] and [make generate-parse-errors]
   run Menhir on a modified copy of the parser where every block of
   text comprised between the markers [BEGIN AVOID] and -----------
   [END AVOID] has been removed. This file should be formatted in
   such a way that this results in a clean removal of certain
   symbols, productions, or declarations. */

%{

open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings
open Docstrings.WithMenhir

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}

let mk_mv ?mut ?virt () = { mv_mut= mut; mv_virt= virt }
let mk_pv ?priv ?virt () = { pv_priv= priv; pv_virt= virt }

let mv_of_mut = function
  | Immutable -> mk_mv ()
  | Mutable mut -> mk_mv ~mut ()

let pv_of_priv = function
  | Public -> mk_pv ()
  | Private priv -> mk_pv ~priv ()

let mkvarinj s l = mkloc s (make_loc l)
let mktyp ~loc ?attrs d = Typ.mk ~loc:(make_loc loc) ?attrs d
let mkpat ~loc d = Pat.mk ~loc:(make_loc loc) d
let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d
let mkmty ~loc ?attrs d = Mty.mk ~loc:(make_loc loc) ?attrs d
let mksig ~loc d = Sig.mk ~loc:(make_loc loc) d
let mkmod ~loc ?attrs d = Mod.mk ~loc:(make_loc loc) ?attrs d
let mkstr ~loc d = Str.mk ~loc:(make_loc loc) d
let mkclass ~loc ?attrs d = Cl.mk ~loc:(make_loc loc) ?attrs d
let mkcty ~loc ?attrs d = Cty.mk ~loc:(make_loc loc) ?attrs d
let mkconst ~loc c = Const.mk ~loc:(make_loc loc) c

let pstr_typext (te, ext) =
  (Pstr_typext te, ext)
let pstr_primitive (vd, ext) =
  (Pstr_primitive vd, ext)
let pstr_type ((nr, ext), tys) =
  (Pstr_type (nr, tys), ext)
let pstr_exception (te, ext) =
  (Pstr_exception te, ext)

let psig_typext (te, ext) =
  (Psig_typext te, ext)
let psig_value (vd, ext) =
  (Psig_value vd, ext)
let psig_type ((nr, ext), tys) =
  (Psig_type (nr, tys), ext)
let psig_typesubst ((nr, ext), tys) =
  assert (nr = Recursive); (* see [no_nonrec_flag] *)
  (Psig_typesubst tys, ext)
let psig_exception (te, ext) =
  (Psig_exception te, ext)

let mkctf ~loc ?attrs ?docs d =
  Ctf.mk ~loc:(make_loc loc) ?attrs ?docs d
let mkcf ~loc ?attrs ?docs d =
  Cf.mk ~loc:(make_loc loc) ?attrs ?docs d

let mkrhs rhs loc = mkloc rhs (make_loc loc)
let ghrhs rhs loc = mkloc rhs (ghost_loc loc)

let mk_optional lbl loc = Optional (mkrhs lbl loc)
let mk_labelled lbl loc = Labelled (mkrhs lbl loc)

let push_loc x acc =
  if x.Location.loc_ghost
  then acc
  else x :: acc

let reloc_pat ~loc x =
  { x with ppat_loc = make_loc loc;
           ppat_loc_stack = push_loc x.ppat_loc x.ppat_loc_stack }
let reloc_exp ~loc x =
  { x with pexp_loc = make_loc loc;
           pexp_loc_stack = push_loc x.pexp_loc x.pexp_loc_stack }
let reloc_typ ~loc x =
  { x with ptyp_loc = make_loc loc;
           ptyp_loc_stack = push_loc x.ptyp_loc x.ptyp_loc_stack }

let mkexpvar ~loc (name : string) =
  mkexp ~loc (Pexp_ident(mkrhs (Lident name) loc))

let mkoperator ~loc (name : string) =
  mkrhs name loc

let mkpatvar ~loc name =
  mkpat ~loc (Ppat_var (mkrhs name loc))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp ~loc d = Exp.mk ~loc:(ghost_loc loc) d
let ghpat ~loc d = Pat.mk ~loc:(ghost_loc loc) d
let ghtyp ~loc d = Typ.mk ~loc:(ghost_loc loc) d
(*
let ghloc ~loc d = { txt = d; loc = ghost_loc loc }
*)
let ghstr ~loc d = Str.mk ~loc:(ghost_loc loc) d
let ghsig ~loc d = Sig.mk ~loc:(ghost_loc loc) d

let mkinfix arg1 op arg2 =
  Pexp_infix(op, arg1, arg2)

(* Jane Street extension *)
let flip_sign = function
  | Positive -> Negative
  | Negative -> Positive
(* End Jane Street extension *)

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus ~oploc name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant({pconst_desc= Pconst_integer (n,m); _} as c) ->
      Pexp_constant({c with pconst_desc= Pconst_integer(neg_string n,m)})
  | ("-" | "-."), Pexp_constant({pconst_desc= Pconst_float (f, m); _} as c) ->
      Pexp_constant({c with pconst_desc= Pconst_float(neg_string f, m)})

  (* Jane Street extension *)
  | "-", Pexp_constant({pconst_desc= Pconst_unboxed_integer (s,n,m); _} as c) ->
      Pexp_constant({c with
                     pconst_desc= Pconst_unboxed_integer(flip_sign s,n,m)})
  | ("-" | "-."), Pexp_constant({pconst_desc=
                                   Pconst_unboxed_float (s, f, m); _} as c) ->
      Pexp_constant({c with
                     pconst_desc= Pconst_unboxed_float(flip_sign s, f, m)})
  (* End Jane Street extension *)

  | _ ->
      Pexp_prefix(mkoperator ~loc:oploc ("~" ^ name), arg)

let mkuplus ~oploc name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant({pconst_desc= Pconst_integer _; _})
  | ("+" | "+."), Pexp_constant({pconst_desc= Pconst_float _; _}) -> desc

  (* Jane Street extension *)
  | "+", Pexp_constant({pconst_desc= Pconst_unboxed_integer _; _})
  | ("+" | "+."), Pexp_constant({pconst_desc= Pconst_unboxed_float _; _})
    -> desc
  (* End Jane Street extension *)

  | _ ->
      Pexp_prefix(mkoperator ~loc:oploc ("~" ^ name), arg)

let mkpat_with_modes ~loc ~pat ~cty ~modes =
  match cty, modes with
  | None, [] -> pat
  | cty, modes -> mkpat ~loc (Ppat_constraint (pat, cty, modes))

let local_ext_loc = mknoloc "extension.local"

let local_attr =
  Attr.mk ~loc:Location.none local_ext_loc (PStr [])

let local_extension =
  Exp.mk ~loc:Location.none (Pexp_extension(local_ext_loc, PStr []))

let mkexp_stack ~loc exp =
  if Erase_jane_syntax.should_erase () then exp else
  ghexp ~loc (Pexp_apply(local_extension, [Nolabel, exp]))

let mkpat_stack pat =
  if Erase_jane_syntax.should_erase () then pat else
  {pat with ppat_attributes = local_attr :: pat.ppat_attributes}

let mktyp_stack typ =
  if Erase_jane_syntax.should_erase () then typ else
  {typ with ptyp_attributes = local_attr :: typ.ptyp_attributes}

let wrap_exp_stack exp =
  if Erase_jane_syntax.should_erase () then exp else
  {exp with pexp_attributes = local_attr :: exp.pexp_attributes}

let mkexp_local_if p ~loc exp =
  if p then mkexp_stack ~loc exp else exp

let mkpat_local_if p pat =
  if p then mkpat_stack pat else pat

let mktyp_local_if p typ =
  if p then mktyp_stack typ else typ

let mktyp_modes modes =
  (* Jane Street: This is horrible temporary code until we properly add
     support for more modes. *)
  let is_local =
    match modes with
    | [] -> false
    | _ :: _ -> true
  in
  mktyp_local_if is_local

let exclave_ext_loc = mknoloc "extension.exclave"

let exclave_extension =
  Exp.mk ~loc:Location.none
    (Pexp_extension(exclave_ext_loc, PStr []))

let mkexp_exclave ~loc exp =
  if Erase_jane_syntax.should_erase () then exp else
  ghexp ~loc (Pexp_apply(exclave_extension, [Nolabel, exp]))

let curry_attr =
  Attr.mk ~loc:Location.none (mknoloc "extension.curry") (PStr [])

let is_curry_attr attr =
  attr.attr_name.txt = "extension.curry"

let mktyp_curry typ =
  if Erase_jane_syntax.should_erase () then typ else
  {typ with ptyp_attributes = curry_attr :: typ.ptyp_attributes}

let maybe_curry_typ typ =
  match typ.ptyp_desc with
  | Ptyp_arrow _ ->
      if List.exists is_curry_attr typ.ptyp_attributes then typ
      else mktyp_curry typ
  | _ -> typ

let global_loc = mknoloc "extension.global"

let global_attr =
  Attr.mk ~loc:Location.none (global_loc) (PStr [])

let globalized_attr = Attr.mk ~loc:Location.none (mknoloc "globalized") (PStr [])

let mkld_global ld =
  let attr =
    if Erase_jane_syntax.should_erase ()
    then globalized_attr
    else global_attr
  in
  { ld with pld_attributes = attr :: ld.pld_attributes }

let mkld_global_maybe gbl ld =
  match gbl with
  | Global -> mkld_global ld
  | Nothing -> ld

let mkcty_global cty =
  let attr =
    if Erase_jane_syntax.should_erase ()
    then globalized_attr
    else global_attr
  in
  { cty with ptyp_attributes = attr :: cty.ptyp_attributes }

let mkcty_global_maybe gbl cty =
  match gbl with
  | Global -> mkcty_global cty
  | Nothing -> cty

(* TODO define an abstraction boundary between locations-as-pairs
   and locations-as-Location.t; it should be clear when we move from
   one world to the other *)

(*
let mkexp_cons_desc consloc args =
  Pexp_construct(mkrhs (Lident "::") consloc, Some args)
let mkexp_cons ~loc consloc args =
  mkexp ~loc (mkexp_cons_desc consloc args)

let mkpat_cons_desc consloc args =
  Ppat_construct(mkrhs (Lident "::") consloc, Some ([], args))
let mkpat_cons ~loc consloc args =
  mkpat ~loc (mkpat_cons_desc consloc args)

let ghexp_cons_desc consloc args =
  Pexp_construct(ghrhs (Lident "::") consloc, Some args)
let ghpat_cons_desc consloc args =
  Ppat_construct(ghrhs (Lident "::") consloc, Some ([], args))

let rec mktailexp nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (Lident "[]") in
      Pexp_construct (nil, None), nilloc
  | e1 :: el ->
      let exp_el, el_loc = mktailexp nilloc el in
      let loc = (e1.pexp_loc.loc_start, snd el_loc) in
      let arg = ghexp ~loc (Pexp_tuple [e1; ghexp ~loc:el_loc exp_el]) in
      ghexp_cons_desc loc arg, loc

let rec mktailpat nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (Lident "[]") in
      Ppat_construct (nil, None), nilloc
  | p1 :: pl ->
      let pat_pl, el_loc = mktailpat nilloc pl in
      let loc = (p1.ppat_loc.loc_start, snd el_loc) in
      let arg = ghpat ~loc (Ppat_tuple [p1; ghpat ~loc:el_loc pat_pl]) in
      ghpat_cons_desc loc arg, loc
*)

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_desc_constraint ~modes e t =
  match t with
  | Pconstraint t -> Pexp_constraint(e, Some t, modes)
  | Pcoerce(t1, t2)  -> Pexp_coerce(e, t1, t2)

let mkexp_constraint ~loc ~modes e t =
  mkexp ~loc (mkexp_desc_constraint ~modes e t)

let erase_str_items str =
  if not (Erase_jane_syntax.should_erase ())
  then str
  else (
    List.filter
      (function
        | { pstr_desc = Pstr_kind_abbrev _; _ } -> false
        | _ -> true)
      str
  )

let erase_sig_items sig_ =
  if not (Erase_jane_syntax.should_erase ())
  then sig_
  else
    List.filter
      (function
        | { psig_desc = Psig_kind_abbrev _; _ } -> false
        | _ -> true)
      sig_

let erase_toplevel_phrases phrases =
  if not (Erase_jane_syntax.should_erase ())
  then phrases
  else
    List.map
      (function
        | Ptop_def str -> Ptop_def (erase_str_items str)
        | Ptop_dir _ as phrase -> phrase)
      phrases

(*
let mkexp_opt_constraint ~loc e = function
  | None -> e
  | Some constraint_ -> mkexp_constraint ~loc e constraint_

let mkpat_opt_constraint ~loc p = function
  | None -> p
  | Some typ -> mkpat ~loc (Ppat_constraint(p, typ))
*)

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(make_loc opening_loc, opening_name,
                                           make_loc closing_loc, closing_name)))

(* Normal mutable arrays and immutable arrays are parsed identically, just with
   different delimiters.  The parsing is done by the [array_exprs] rule, and the
   [Generic_array] module provides (1) a type representing the possible results,
   and (2) a function for going from that type to an AST fragment representing
   an array. *)
module Generic_array = struct
  (** The three possible ways to parse an array (writing [[? ... ?]] for either
      [[| ... |]] or [[: ... :]]): *)
  type (_, _) t =
    | Literal : 'ast list -> ('ast, 'ast_desc) t
    (** A plain array literal/pattern, [[? x; y; z ?]] *)
    | Opened_literal : Longident.t Location.loc *
                       Lexing.position *
                       Lexing.position *
                       expression list
                     -> (expression, expression_desc) t
    (** An array literal with a local open, [Module.[? x; y; z ?]] (only valid in
        expressions) *)
    | Unclosed : (Lexing.position * Lexing.position) *
                 (Lexing.position * Lexing.position)
               -> (_, _) t
    (** Parse error: an unclosed array literal, [\[? x; y; z] with no closing
        [?\]]. *)

  let to_ast (type ast ast_desc)
             (open_ : string) (close : string)
             (array : ast list -> ast_desc)
        : (ast, ast_desc) t -> ast_desc = function
    | Literal elts ->
        array elts
    | Opened_literal(od, startpos, endpos, elts) ->
        (Pexp_open(od, mkexp ~loc:(startpos, endpos) (array elts)) : ast_desc)
    | Unclosed(startpos, endpos) ->
        unclosed open_ startpos close endpos

  let expression : _ -> _ -> _ -> (expression, expression_desc) t -> _ = to_ast
  let pattern    : _ -> _ -> _ -> (pattern,    pattern_desc)    t -> _ = to_ast
end

let expecting_loc (loc : Location.t) (nonterm : string) =
    raise Syntaxerr.(Error(Expecting(loc, nonterm)))
let expecting (loc : Lexing.position * Lexing.position) nonterm =
     expecting_loc (make_loc loc) nonterm

(* Continues to parse removed syntax
let removed_string_set loc =
  raise(Syntaxerr.Error(Syntaxerr.Removed_string_set(make_loc loc)))
*)

(* Using the function [not_expecting] in a semantic action means that this
   syntactic form is recognized by the parser but is in fact incorrect. This
   idiom is used in a few places to produce ad hoc syntax error messages. *)

(* This idiom should be used as little as possible, because it confuses the
   analyses performed by Menhir. Because Menhir views the semantic action as
   opaque, it believes that this syntactic form is correct. This can lead
   [make generate-parse-errors] to produce sentences that cause an early
   (unexpected) syntax error and do not achieve the desired effect. This could
   also lead a completion system to propose completions which in fact are
   incorrect. In order to avoid these problems, the productions that use
   [not_expecting] should be marked with AVOID. *)

let not_expecting loc nonterm =
    raise Syntaxerr.(Error(Not_expecting(make_loc loc, nonterm)))

let mk_builtin_indexop_expr ~loc (pia_lhs, _dot, pia_paren, idx, pia_rhs) =
  mkexp ~loc
    (Pexp_indexop_access
       { pia_lhs; pia_kind= Builtin idx; pia_paren; pia_rhs })

let mk_dotop_indexop_expr ~loc (pia_lhs, (path, op), pia_paren, idx, pia_rhs) =
  mkexp ~loc
    (Pexp_indexop_access
       { pia_lhs; pia_kind= Dotop (path, op, idx); pia_paren; pia_rhs })

let paren_to_strings = function
  | Paren -> "(", ")"
  | Bracket -> "[", "]"
  | Brace -> "{", "}"

let indexop_unclosed_error loc_s s loc_e =
  let left, right = paren_to_strings s in
  unclosed left loc_s right loc_e

let lapply ~loc p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(
                  Syntaxerr.Applicative_path (make_loc loc)))

let make_ghost x = { x with loc = { x.loc with loc_ghost = true }}

let loc_last (id : Longident.t Location.loc) : string Location.loc =
  Location.map Longident.last id

let loc_lident (id : string Location.loc) : Longident.t Location.loc =
  Location.map (fun x -> Lident x) id

(*
let exp_of_longident lid =
  let lid = loc_map (fun id -> Lident (Longident.last id)) lid in
  Exp.mk ~loc:lid.loc (Pexp_ident lid)
*)

let exp_of_label lbl =
  Exp.mk ~loc:lbl.loc (Pexp_ident (loc_lident lbl))

(*
let pat_of_label lbl =
  Pat.mk ~loc:lbl.loc  (Ppat_var (loc_last lbl))
*)

let mk_newtypes ~loc newtypes exp =
  let mkexp = mkexp ~loc in
  List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
    newtypes exp

let wrap_type_annotation ~loc ~modes newtypes core_type body =
  let mkexp, ghtyp = mkexp ~loc, ghtyp ~loc in
  let mk_newtypes = mk_newtypes ~loc in
  let exp = mkexp(Pexp_constraint(body,Some core_type,modes)) in
  let exp = mk_newtypes newtypes exp in
  (exp, ghtyp(Ptyp_poly(newtypes, core_type)))

let wrap_exp_attrs ~loc body (ext, attrs) =
  let ghexp = ghexp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs ~loc d attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) attrs

let wrap_typ_attrs ~loc typ (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let typ = {typ with ptyp_attributes = attrs @ typ.ptyp_attributes} in
  match ext with
  | None -> typ
  | Some id -> ghtyp ~loc (Ptyp_extension (id, PTyp typ))

let wrap_pat_attrs ~loc pat (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let pat = {pat with ppat_attributes = attrs @ pat.ppat_attributes} in
  match ext with
  | None -> pat
  | Some id -> ghpat ~loc (Ppat_extension (id, PPat (pat, None)))

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let wrap_class_attrs ~loc:_ body attrs =
  {body with pcl_attributes = attrs @ body.pcl_attributes}
(*
let wrap_mod_attrs ~loc:_ attrs body =
  {body with pmod_attributes = attrs @ body.pmod_attributes}
let wrap_mty_attrs ~loc:_ attrs body =
  {body with pmty_attributes = attrs @ body.pmty_attributes}
*)

let wrap_str_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghstr ~loc (Pstr_extension ((id, PStr [body]), []))

let wrap_mkstr_ext ~loc (item, ext) =
  wrap_str_ext ~loc (mkstr ~loc item) ext

let wrap_sig_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghsig ~loc (Psig_extension ((id, PSig [body]), []))

let wrap_mksig_ext ~loc (item, ext) =
  wrap_sig_ext ~loc (mksig ~loc item) ext

let mk_quotedext ~loc (id, idloc, str, strloc, delim) =
  let exp_id = mkloc id idloc in
  let const = Const.mk ~loc:strloc (Pconst_string (str, strloc, delim)) in
  let e = ghexp ~loc (Pexp_constant const) in
  (exp_id, PStr [mkstrexp e []])

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos =
  List.map (fun def -> Ptop_def [def]) (Str.text (rhs_text pos))

let extra_text startpos endpos text items =
  match items with
  | [] ->
      let post = rhs_post_text endpos in
      let post_extras = rhs_post_extra_text endpos in
      text post @ text post_extras
  | _ :: _ ->
      let pre_extras = rhs_pre_extra_text startpos in
      let post_extras = rhs_post_extra_text endpos in
        text pre_extras @ items @ text post_extras

let extra_str p1 p2 items = extra_text p1 p2 Str.text items
let extra_sig p1 p2 items = extra_text p1 p2 Sig.text items
let extra_cstr p1 p2 items = extra_text p1 p2 Cf.text items
let extra_csig p1 p2 items = extra_text p1 p2 Ctf.text  items
let extra_def p1 p2 items =
  extra_text p1 p2
    (fun txt -> List.map (fun def -> Ptop_def [def]) (Str.text txt))
    items

let extra_rhs_core_type ct ~pos =
  let docs = rhs_info pos in
  { ct with ptyp_attributes = add_info_attrs docs ct.ptyp_attributes }

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_constraint: value_constraint option;
    lb_is_pun: bool;
    lb_modes: mode Location.loc list;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings' =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }

let mklb first ~loc (p, e, typ, modes, is_pun) attrs =
  {
    lb_pattern = p;
    lb_expression = e;
    lb_constraint=typ;
    lb_is_pun = is_pun;
    lb_modes = modes;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy loc;
    lb_text = (if first then empty_text_lazy
               else symbol_text_lazy (fst loc));
    lb_loc = make_loc loc;
  }

let addlb lbs lb =
  if lb.lb_is_pun && lbs.lbs_extension = None then syntax_error ();
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

let mklbs ext rf lb =
  let lbs = {
    lbs_bindings = [];
    lbs_rec = rf;
    lbs_extension = ext;
  } in
  addlb lbs lb

let mk_let_bindings { lbs_bindings; lbs_rec; lbs_extension } =
  let pvbs_bindings =
    List.rev_map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~modes:lb.lb_modes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           ?value_constraint:lb.lb_constraint ~is_pun:lb.lb_is_pun
           lb.lb_pattern lb.lb_expression)
      lbs_bindings
  in
  { pvbs_bindings; pvbs_rec = lbs_rec; pvbs_extension = lbs_extension }

let val_of_let_bindings ~loc lbs =
  mkstr ~loc (Pstr_value (mk_let_bindings lbs))

let expr_of_let_bindings ~loc lbs body =
  mkexp_attrs ~loc (Pexp_let (mk_let_bindings lbs, body)) (None, [])

let class_of_let_bindings ~loc lbs body =
  (* Our use of let_bindings(no_ext) guarantees the following: *)
  assert (lbs.lbs_extension = None);
  mkclass ~loc (Pcl_let (mk_let_bindings lbs, body))

(* Alternatively, we could keep the generic module type in the Parsetree
   and extract the package type during type-checking. In that case,
   the assertions below should be turned into explicit checks. *)
let package_type_of_module_type pmty =
  let err loc s =
    raise (Syntaxerr.Error (Syntaxerr.Invalid_package_type (loc, s)))
  in
  let map_cstr = function
    | Pwith_type (lid, ptyp) ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> [] then
          err loc Parameterized_types;
        if ptyp.ptype_cstrs <> [] then
          err loc Constrained_types;
        if ptyp.ptype_private <> Public then
          err loc Private_types;

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = []);
        let ty =
          match ptyp.ptype_manifest with
          | Some ty -> ty
          | None -> assert false
        in
        (lid, ty)
    | _ ->
        err pmty.pmty_loc Not_with_type
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> (lid, [], pmty.pmty_attributes)
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
      (lid, List.map map_cstr cstrs, pmty.pmty_attributes)
  | _ ->
      err pmty.pmty_loc Neither_identifier_nor_with_type

let mk_directive_arg ~loc k =
  { pdira_desc = k;
    pdira_loc = make_loc loc;
  }

let mk_directive ~loc name arg =
  Ptop_dir {
      pdir_name = name;
      pdir_arg = arg;
      pdir_loc = make_loc loc;
    }

let convert_jkind_to_legacy_attr =
  let mk ~loc name = [Attr.mk ~loc (mkloc name loc) (PStr [])] in
  function
  | {txt = Abbreviation {txt = "immediate"; loc}; loc = _} -> mk ~loc "immediate"
  | {txt = Abbreviation {txt = "immediate64"; loc}; loc = _} -> mk ~loc "immediate64"
  | _ -> []

(* NOTE: An alternate approach for performing the erasure of %call_pos and %src_pos
   could have been doing it as a ppx transformation instead of performing the erasing
   inside of ocamlformat. *)
let transl_label ~pattern ~arg_label ~loc =
  if not (Erase_jane_syntax.should_erase ())
  then arg_label, pattern, None
  else (
    match arg_label, pattern.ppat_desc with
    | ( Labelled l
      , Ppat_constraint
          (pat, Some { ptyp_desc = Ptyp_extension ({ txt = "call_pos"; loc = _ }, _); _ }, _) ) ->
      ( Optional l
      , pat
      , Some
          (Ast_helper.Exp.ident
             { loc; txt = Ldot (Ldot (Lident "Stdlib", "Lexing"), "dummy_pos") }) )
    | _ -> arg_label, pattern, None)
;;

%}

/* Tokens */

/* The alias that follows each token is used by Menhir when it needs to
   produce a sentence (that is, a sequence of tokens) in concrete syntax. */

/* Some tokens represent multiple concrete strings. In most cases, an
   arbitrary concrete string can be chosen. In a few cases, one must
   be careful: e.g., in PREFIXOP and INFIXOP2, one must choose a concrete
   string that will not trigger a syntax error; see how [not_expecting]
   is used in the definition of [type_variance]. */

%token AMPERAMPER             "&&"
%token AMPERSAND              "&"
%token AND                    "and"
%token AS                     "as"
%token ASSERT                 "assert"
%token BACKQUOTE              "`"
%token BANG                   "!"
%token BAR                    "|"
%token BARBAR                 "||"
%token BARRBRACKET            "|]"
%token BEGIN                  "begin"
%token <char * string> CHAR   "'a'" (* just an example *)
%token CLASS                  "class"
%token COLON                  ":"
%token COLONCOLON             "::"
%token COLONEQUAL             ":="
%token COLONGREATER           ":>"
%token COLONRBRACKET          ":]"
%token COMMA                  ","
%token CONSTRAINT             "constraint"
%token DO                     "do"
%token DONE                   "done"
%token DOT                    "."
%token DOTDOT                 ".."
%token DOTHASH                ".#"
%token DOWNTO                 "downto"
%token ELSE                   "else"
%token END                    "end"
%token EOF                    ""
%token EQUAL                  "="
%token EXCEPTION              "exception"
%token EXCLAVE                "exclave_"
%token EXTERNAL               "external"
%token FALSE                  "false"
%token <string * char option> FLOAT "42.0" (* just an example *)
%token FOR                    "for"
%token FUN                    "fun"
%token FUNCTION               "function"
%token FUNCTOR                "functor"
%token GLOBAL                 "global_"
%token GREATER                ">"
%token GREATERRBRACE          ">}"
%token GREATERRBRACKET        ">]"
%token HASHLPAREN             "#("
%token HASHLBRACE             "#{"
%token IF                     "if"
%token IN                     "in"
%token INCLUDE                "include"
%token <string> INFIXOP0      "!="   (* just an example *)
%token AT                     "@"    (* mode expression *)
%token ATAT                   "@@"   (* mode expression *)
%token <string> INFIXOP1      "^"    (* just an example *)
%token <string> INFIXOP2      "+!"   (* chosen with care; see above *)
%token <string> INFIXOP3      "land" (* just an example *)
%token <string> INFIXOP4      "**"   (* just an example *)
%token <string> DOTOP         ".+"
%token <string> LETOP         "let*" (* just an example *)
%token <string> ANDOP         "and*" (* just an example *)
%token INHERIT                "inherit"
%token INITIALIZER            "initializer"
%token <string * char option> INT "42"  (* just an example *)
%token KIND_ABBREV            "kind_abbrev_"
%token KIND_OF                "kind_of_"
%token <string> LABEL         "~label:" (* just an example *)
%token LAZY                   "lazy"
%token LBRACE                 "{"
%token LBRACELESS             "{<"
%token LBRACKET               "["
%token LBRACKETBAR            "[|"
%token LBRACKETCOLON          "[:"
%token LBRACKETLESS           "[<"
%token LBRACKETGREATER        "[>"
%token LBRACKETPERCENT        "[%"
%token LBRACKETPERCENTPERCENT "[%%"
%token LESS                   "<"
%token LESSMINUS              "<-"
%token LET                    "let"
%token <string> LIDENT        "lident" (* just an example *)
%token LOCAL                  "local_"
%token LPAREN                 "("
%token LBRACKETAT             "[@"
%token LBRACKETATAT           "[@@"
%token LBRACKETATATAT         "[@@@"
%token MATCH                  "match"
%token METHOD                 "method"
%token MINUS                  "-"
%token MINUSDOT               "-."
%token MINUSGREATER           "->"
%token MOD                    "mod"
%token MODULE                 "module"
%token MUTABLE                "mutable"
%token NEW                    "new"
%token NONREC                 "nonrec"
%token OBJECT                 "object"
%token OF                     "of"
%token OPEN                   "open"
%token <string> OPTLABEL      "?label:" (* just an example *)
%token OR                     "or"
/* %token PARSER              "parser" */
%token PERCENT                "%"
%token PLUS                   "+"
%token PLUSDOT                "+."
%token PLUSEQ                 "+="
%token <string> PREFIXOP      "!+" (* chosen with care; see above *)
%token PRIVATE                "private"
%token QUESTION               "?"
%token QUOTE                  "'"
%token RBRACE                 "}"
%token RBRACKET               "]"
%token REC                    "rec"
%token RPAREN                 ")"
%token SEMI                   ";"
%token SEMISEMI               ";;"
%token HASH                   "#"
%token <string> HASHOP        "##" (* just an example *)
%token SIG                    "sig"
%token SLASH                  "/"
%token STACK                  "stack_"
%token STAR                   "*"
%token <string * Location.t * string option>
       STRING                 "\"hello\"" (* just an example *)
%token <string * Location.t * string * Location.t * string option>
       QUOTED_STRING_EXPR     "{%hello|world|}"  (* just an example *)
%token <string * Location.t * string * Location.t * string option>
       QUOTED_STRING_ITEM     "{%%hello|world|}" (* just an example *)
%token STRUCT                 "struct"
%token THEN                   "then"
%token TILDE                  "~"
%token TO                     "to"
%token TRUE                   "true"
%token TRY                    "try"
%token TYPE                   "type"
%token <string> UIDENT        "UIdent" (* just an example *)
%token UNDERSCORE             "_"
%token VAL                    "val"
%token VIRTUAL                "virtual"
%token WHEN                   "when"
%token WHILE                  "while"
%token WITH                   "with"
%token <string * Location.t> COMMENT    "(* comment *)"
%token <Docstrings.docstring> DOCSTRING "(** documentation *)"

%token EOL                    "\\n"      (* not great, but EOL is unused *)

%token <string> TYPE_DISAMBIGUATOR "2" (* just an example *)

(* Jane Street extension *)
%token <string * char option> HASH_FLOAT "#42.0" (* just an example *)
%token <string * char option> HASH_INT "#42l" (* just an example *)
%token                        HASH_SUFFIX "# "
(* End Jane Street extension *)

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET FOR                       /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%nonassoc below_FUNCTOR                 /* include M */
%nonassoc FUNCTOR                       /* include functor M */
%right    MINUSGREATER                  /* function_type (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%nonassoc below_AMPERSAND
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    ATAT AT INFIXOP1                      /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT SLASH INFIXOP3 MOD STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unboxed_product_kind
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_HASH
%nonassoc HASH HASH_SUFFIX              /* simple_expr/toplevel_directive */
%left     HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTHASH DOTOP
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT HASH_FLOAT INT HASH_INT OBJECT
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LBRACKETCOLON LIDENT LPAREN
          NEW PREFIXOP STRING TRUE UIDENT UNDERSCORE
          LBRACKETPERCENT QUOTED_STRING_EXPR STACK HASHLBRACE HASHLPAREN


/* Entry points */

/* Several start symbols are marked with AVOID so that they are not used by
   [make generate-parse-errors]. The three start symbols that we keep are
   [implementation], [use_file], and [toplevel_phrase]. The latter two are
   of marginal importance; only [implementation] really matters, since most
   states in the automaton are reachable from it. */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
/* BEGIN AVOID */
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
/* END AVOID */
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
/* BEGIN AVOID */
%start parse_module_type
%type <Parsetree.module_type> parse_module_type
%start parse_module_expr
%type <Parsetree.module_expr> parse_module_expr
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%start parse_constr_longident
%type <Longident.t> parse_constr_longident
%start parse_val_longident
%type <Longident.t> parse_val_longident
%start parse_mty_longident
%type <Longident.t> parse_mty_longident
%start parse_mod_ext_longident
%type <Longident.t> parse_mod_ext_longident
%start parse_mod_longident
%type <Longident.t> parse_mod_longident
%start parse_any_longident
%type <Longident.t> parse_any_longident
/* END AVOID */

%%

/* macros */
%inline extra_str(symb): symb { extra_str $startpos $endpos $1 };
%inline extra_sig(symb): symb { extra_sig $startpos $endpos $1 };
%inline extra_cstr(symb): symb { extra_cstr $startpos $endpos $1 };
%inline extra_csig(symb): symb { extra_csig $startpos $endpos $1 };
%inline extra_def(symb): symb { extra_def $startpos $endpos $1 };
%inline extra_text(symb): symb { extra_text $startpos $endpos $1 };
%inline extra_rhs(symb): symb { extra_rhs_core_type $1 ~pos:$endpos($1) };
%inline mkrhs(symb): symb
    { mkrhs $1 $sloc }
;

%inline text_str(symb): symb
  { text_str $startpos @ [$1] }
%inline text_str_SEMISEMI: SEMISEMI
  { text_str $startpos }
%inline text_sig(symb): symb
  { text_sig $startpos @ [$1] }
%inline text_sig_SEMISEMI: SEMISEMI
  { text_sig $startpos }
%inline text_def(symb): symb
  { text_def $startpos @ [$1] }
%inline top_def(symb): symb
  { Ptop_def [$1] }
%inline text_cstr(symb): symb
  { text_cstr $startpos @ [$1] }
%inline text_csig(symb): symb
  { text_csig $startpos @ [$1] }

(* Using this %inline definition means that we do not control precisely
   when [mark_rhs_docs] is called, but I don't think this matters. *)
%inline mark_rhs_docs(symb): symb
  { mark_rhs_docs $startpos $endpos;
    $1 }

%inline op(symb): symb
   { mkoperator ~loc:$sloc $1 }

%inline mkloc(symb): symb
    { mkloc $1 (make_loc $sloc) }

%inline mkexp(symb): symb
    { mkexp ~loc:$sloc $1 }
%inline mkpat(symb): symb
    { mkpat ~loc:$sloc $1 }
%inline mktyp(symb): symb
    { mktyp ~loc:$sloc $1 }
%inline mkstr(symb): symb
    { mkstr ~loc:$sloc $1 }
%inline mksig(symb): symb
    { mksig ~loc:$sloc $1 }
%inline mkmod(symb): symb
    { mkmod ~loc:$sloc $1 }
%inline mkmty(symb): symb
    { mkmty ~loc:$sloc $1 }
%inline mkcty(symb): symb
    { mkcty ~loc:$sloc $1 }
%inline mkctf(symb): symb
    { mkctf ~loc:$sloc $1 }
%inline mkcf(symb): symb
    { mkcf ~loc:$sloc $1 }
%inline mkclass(symb): symb
    { mkclass ~loc:$sloc $1 }

%inline wrap_mkstr_ext(symb): symb
    { wrap_mkstr_ext ~loc:$sloc $1 }
%inline wrap_mksig_ext(symb): symb
    { wrap_mksig_ext ~loc:$sloc $1 }

%inline mk_directive_arg(symb): symb
    { mk_directive_arg ~loc:$sloc $1 }

/* Generic definitions */

(* [iloption(X)] recognizes either nothing or [X]. Assuming [X] produces
   an OCaml list, it produces an OCaml list, too. *)

%inline iloption(X):
  /* nothing */
    { [] }
| x = X
    { x }

(* [llist(X)] recognizes a possibly empty list of [X]s. It is left-recursive. *)

reversed_llist(X):
  /* empty */
    { [] }
| xs = reversed_llist(X) x = X
    { x :: xs }

%inline llist(X):
  xs = rev(reversed_llist(X))
    { xs }

(* [reversed_nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces
   an OCaml list in reverse order -- that is, the last element in the input text
   appears first in this list. Its definition is left-recursive. *)

reversed_nonempty_llist(X):
  x = X
    { [ x ] }
| xs = reversed_nonempty_llist(X) x = X
    { x :: xs }

(* [nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces an OCaml
   list in direct order -- that is, the first element in the input text appears
   first in this list. *)

%inline nonempty_llist(X):
  xs = rev(reversed_nonempty_llist(X))
    { xs }

(* [reversed_separated_nonempty_llist(separator, X)] recognizes a nonempty list
   of [X]s, separated with [separator]s, and produces an OCaml list in reverse
   order -- that is, the last element in the input text appears first in this
   list. Its definition is left-recursive. *)

(* [inline_reversed_separated_nonempty_llist(separator, X)] is semantically
   equivalent to [reversed_separated_nonempty_llist(separator, X)], but is
   marked %inline, which means that the case of a list of length one and
   the case of a list of length more than one will be distinguished at the
   use site, and will give rise there to two productions. This can be used
   to avoid certain conflicts. *)

%inline inline_reversed_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_llist(separator, X)
  separator
  x = X
    { x :: xs }

reversed_separated_nonempty_llist(separator, X):
  xs = inline_reversed_separated_nonempty_llist(separator, X)
    { xs }

(* [separated_nonempty_llist(separator, X)] recognizes a nonempty list of [X]s,
   separated with [separator]s, and produces an OCaml list in direct order --
   that is, the first element in the input text appears first in this list. *)

%inline separated_nonempty_llist(separator, X):
  xs = rev(reversed_separated_nonempty_llist(separator, X))
    { xs }

%inline inline_separated_nonempty_llist(separator, X):
  xs = rev(inline_reversed_separated_nonempty_llist(separator, X))
    { xs }

(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }

(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  x = X ioption(delimiter)
    { [x] }
| x = X
  delimiter
  xs = separated_or_terminated_nonempty_list(delimiter, X)
    { x :: xs }

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [bar_llist(X)] recognizes a nonempty list of [X]'s, separated with BARs,
   with an optional leading BAR. We assume that [X] is itself parameterized
   with an opening symbol, which can be [epsilon] or [BAR]. *)

(* This construction may seem needlessly complicated: one might think that
   using [preceded_or_separated_nonempty_llist(BAR, X)], where [X] is *not*
   itself parameterized, would be sufficient. Indeed, this simpler approach
   would recognize the same language. However, the two approaches differ in
   the footprint of [X]. We want the start location of [X] to include [BAR]
   when present. In the future, we might consider switching to the simpler
   definition, at the cost of producing slightly different locations. TODO *)

reversed_bar_llist(X):
    (* An [X] without a leading BAR. *)
    x = X(epsilon)
      { [x] }
  | (* An [X] with a leading BAR. *)
    x = X(BAR)
      { [x] }
  | (* An initial list, followed with a BAR and an [X]. *)
    xs = reversed_bar_llist(X)
    x = X(BAR)
      { x :: xs }

%inline bar_llist(X):
  xs = reversed_bar_llist(X)
    { List.rev xs }

(* [xlist(A, B)] recognizes [AB*]. We assume that the semantic value for [A]
   is a pair [x, b], while the semantic value for [B*] is a list [bs].
   We return the pair [x, b :: bs]. *)

%inline xlist(A, B):
  a = A bs = B*
    { let (x, b) = a in x, b :: bs }

(* [listx(delimiter, X, Y)] recognizes a nonempty list of [X]s, optionally
   followed with a [Y], separated-or-terminated with [delimiter]s. The
   semantic value is a pair of a list of [X]s and an optional [Y]. *)

listx(delimiter, X, Y):
| x = X ioption(delimiter)
    { [x], None }
| x = X delimiter y = mkloc(Y) delimiter?
    { [x], Some y }
| x = X
  delimiter
  tail = listx(delimiter, X, Y)
    { let xs, y = tail in
      x :: xs, y }

(* -------------------------------------------------------------------------- *)

(* Entry points. *)

(* An .ml file. *)
implementation:
  structure EOF
    { $1 }
;

/* BEGIN AVOID */
(* An .mli file. *)
interface:
  signature EOF
    { $1 }
;
/* END AVOID */

(* A toplevel phrase. *)
toplevel_phrase:
  (* An expression with attributes, ended by a double semicolon. *)
  extra_str(text_str(str_exp))
  SEMISEMI
    { Ptop_def $1 }
| (* A list of structure items, ended by a double semicolon. *)
  extra_str(flatten(text_str(structure_item)*))
  SEMISEMI
    { Ptop_def (erase_str_items $1) }
| (* A directive, ended by a double semicolon. *)
  toplevel_directive
  SEMISEMI
    { $1 }
| (* End of input. *)
  EOF
    { raise End_of_file }
;

(* An .ml file that is read by #use. *)
use_file:
  (* An optional standalone expression,
     followed with a series of elements,
     followed with EOF. *)
  extra_def(append(
    optional_use_file_standalone_expression,
    flatten(use_file_element*)
  ))
  EOF
    { erase_toplevel_phrases $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_use_file_standalone_expression:
  iloption(text_def(top_def(str_exp)))
    { $1 }
;

(* An element in a #used file is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item;
   - a toplevel directive.
 *)
%inline use_file_element:
  preceded(SEMISEMI, optional_use_file_standalone_expression)
| text_def(top_def(structure_item))
| text_def(mark_rhs_docs(toplevel_directive))
      { $1 }
;

/* BEGIN AVOID */
parse_module_type:
  module_type EOF
    { $1 }
;

parse_module_expr:
  module_expr EOF
    { $1 }
;

parse_core_type:
  core_type EOF
    { $1 }
;

parse_expression:
  seq_expr EOF
    { $1 }
;

parse_pattern:
  pattern EOF
    { $1 }
;

parse_mty_longident:
  mty_longident EOF
    { $1 }
;

parse_val_longident:
  val_longident EOF
    { $1 }
;

parse_constr_longident:
  constr_longident EOF
    { $1 }
;

parse_mod_ext_longident:
  mod_ext_longident EOF
    { $1 }
;

parse_mod_longident:
  mod_longident EOF
    { $1 }
;

parse_any_longident:
  any_longident EOF
    { $1 }
;
/* END AVOID */

(* -------------------------------------------------------------------------- *)

(* Functor arguments appear in module expressions and module types. *)

(* Compared to upstream, [functor_args] can be empty and is not in reverse
   order. *)
%inline functor_args:
  llist(functor_arg)
    { $1 }
;

%inline nonempty_functor_args:
  nonempty_llist(functor_arg)
    { $1 }
;

functor_arg:
    (* An anonymous and untyped argument. *)
    LPAREN RPAREN
      { mkloc Unit (make_loc $sloc) }
  | (* An argument accompanied with an explicit type. *)
    LPAREN x = mkrhs(module_name) COLON mty = module_type RPAREN
      { mkloc (Named (x, mty)) (make_loc $sloc) }
;

module_name:
    (* A named argument. *)
    x = UIDENT
      { Some x }
  | (* An anonymous argument. *)
    UNDERSCORE
      { None }
;

(* -------------------------------------------------------------------------- *)

(* Module expressions. *)

(* The syntax of module expressions is not properly stratified. The cases of
   functors, functor applications, and attributes interact and cause conflicts,
   which are resolved by precedence declarations. This is concise but fragile.
   Perhaps in the future an explicit stratification could be used. *)

module_expr:
  | STRUCT attrs = attributes s = structure END
      { mkmod ~loc:$sloc ~attrs (Pmod_structure s) }
  | STRUCT attributes structure error
      { unclosed "struct" $loc($1) "end" $loc($4) }
  | SIG error
      { expecting $loc($1) "struct" }
  | FUNCTOR attrs = attributes args = nonempty_functor_args MINUSGREATER me = module_expr
      { let me =
          match attrs, me with
          | [], {pmod_desc= Pmod_functor (args', me'); pmod_attributes= []; _} ->
              Pmod_functor (args @ args', me')
          | _ -> Pmod_functor (args, me)
        in
        mkmod ~loc:$sloc ~attrs me }
  | me = paren_module_expr
      { me }
  | me = module_expr attr = attribute
      { Mod.attr me attr }
  | mkmod(
      (* A module identifier. *)
      x = mkrhs(mod_longident)
        { Pmod_ident x }
    | (* In a functor application, the actual argument must be parenthesized. *)
      me1 = module_expr me2 = paren_module_expr
        { Pmod_apply(me1, me2) }
    | (* Functor applied to unit. *)
      me = module_expr LPAREN RPAREN
        { Pmod_apply_unit (me, make_loc ($startpos($2), $endpos($3))) }
    | (* An extension. *)
      ex = extension
        { Pmod_extension ex }
    | (* A hole. *)
      UNDERSCORE
        { Pmod_hole }
    )
    { $1 }
;

(* A parenthesized module expression is a module expression that begins
   and ends with parentheses. *)

paren_module_expr:
    (* A module expression annotated with a module type. *)
    LPAREN me = module_expr COLON mty = module_type RPAREN
      { mkmod ~loc:$sloc (Pmod_constraint(me, mty)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | (* A module expression within parentheses. *)
    LPAREN me = module_expr RPAREN
      { me (* TODO consider reloc *) }
  | LPAREN module_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | (* A core language expression that produces a first-class module.
       This expression can be annotated in various ways. *)
    LPAREN VAL attrs = attributes e = expr_colon_package_type RPAREN
      { let (e, ty1, ty2) = e in
        mkmod ~loc:$sloc ~attrs (Pmod_unpack (e, ty1, ty2)) }
  | LPAREN VAL attributes expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr COLONGREATER error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr error
      { unclosed "(" $loc($1) ")" $loc($5) }
;

(* The various ways of annotating a core language expression that
   produces a first-class module that we wish to unpack. *)
%inline expr_colon_package_type:
    e = expr
      { e, None, None }
  | e = expr COLON ty1 = package_type
      { e, Some ty1, None }
  | e = expr COLON ty1 = package_type COLONGREATER ty2 = package_type
      { e, Some ty1, Some ty2 }
  | e = expr COLONGREATER ty2 = package_type
      { e, None, Some ty2 }
;

(* A structure, which appears between STRUCT and END (among other places),
   begins with an optional standalone expression, and continues with a list
   of structure elements. *)
structure:
  extra_str(append(
    optional_structure_standalone_expression,
    flatten(structure_element*)
  ))
  { erase_str_items $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_structure_standalone_expression:
  items = iloption(mark_rhs_docs(text_str(str_exp)))
    { items }
;

(* An expression with attributes, wrapped as a structure item. *)
%inline str_exp:
  e = seq_expr
  attrs = post_item_attributes
    { mkstrexp e attrs }
;

(* A structure element is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item. *)
%inline structure_element:
    append(text_str_SEMISEMI, optional_structure_standalone_expression)
  | text_str(structure_item)
      { $1 }
;

(* A structure item. *)
structure_item:
    let_bindings(ext)
      { val_of_let_bindings ~loc:$sloc $1 }
  | mkstr(
      item_extension post_item_attributes
        { let docs = symbol_docs $sloc in
          Pstr_extension ($1, add_docs_attrs docs $2) }
    | floating_attribute
        { Pstr_attribute $1 }
    | module_binding
        { $1 }
    | rec_module_bindings
        { Pstr_recmodule $1 }
    | module_type_declaration
        { Pstr_modtype $1 }
    )
  | wrap_mkstr_ext(
      primitive_declaration
        { pstr_primitive $1 }
    | value_description
        { pstr_primitive $1 }
    | type_declarations
        { pstr_type $1 }
    | str_type_extension
        { pstr_typext $1 }
    | str_exception_declaration
        { pstr_exception $1 }
    | open_declaration
        { let (body, ext) = $1 in (Pstr_open body, ext) }
    | class_declarations
        { let (ext, l) = $1 in (Pstr_class l, ext) }
    | class_type_declarations
        { let (ext, l) = $1 in (Pstr_class_type l, ext) }
    )
    { $1 }
  | include_statement(module_expr)
      { let incl, ext = $1 in
        let item = mkstr ~loc:$sloc (Pstr_include incl) in
        wrap_str_ext ~loc:$sloc item ext }
  | kind_abbreviation_decl
      { let name, jkind = $1 in
        mkstr ~loc:$sloc (Pstr_kind_abbrev (name, jkind)) }
;

(* A single module binding. *)
%inline module_binding:
  MODULE
  ext = ext attrs1 = attributes
  name = mkrhs(module_name)
  args = functor_args
  body = module_binding_body
  attrs2 = post_item_attributes
    { let docs = symbol_docs $sloc in
      let loc = make_loc $sloc in
      let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
      let body = Mb.mk name args body ~attrs ~loc ~docs in
      Pstr_module body }
;

(* The body (right-hand side) of a module binding. *)
module_binding_body:
    EQUAL me = module_expr
      { me }
  | COLON error
      { expecting $loc($1) "=" }
  | mkmod(
      COLON mty = module_type EQUAL me = module_expr
        { Pmod_constraint(me, mty) }
(*
    | arg_and_pos = functor_arg body = module_binding_body
        { let (_, arg) = arg_and_pos in
          Pmod_functor(arg, body) }
*)
  ) { $1 }
;

(* A group of recursive module bindings. *)
%inline rec_module_bindings:
  rec_module_binding list(and_module_binding)
    { $1 :: $2 }
;

(* The first binding in a group of recursive module bindings. *)
%inline rec_module_binding:
  MODULE
  ext = ext
  attrs1 = attributes
  REC
  name = mkrhs(module_name)
  args = functor_args
  body = module_binding_body
  attrs2 = post_item_attributes
  {
    let loc = make_loc $sloc in
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let docs = symbol_docs $sloc in
    Mb.mk name args body ~attrs ~loc ~docs
  }
;

(* The following bindings in a group of recursive module bindings. *)
%inline and_module_binding:
  AND
  attrs1 = attributes
  name = mkrhs(module_name)
  args = functor_args
  body = module_binding_body
  attrs2 = post_item_attributes
  {
    let loc = make_loc $sloc in
    let attrs = Attr.ext_attrs ~before:attrs1 ~after:attrs2 () in
    let docs = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Mb.mk name args body ~attrs ~loc ~text ~docs
  }
;

(* -------------------------------------------------------------------------- *)

(* Shared material between structures and signatures. *)

include_kind:
  | INCLUDE %prec below_FUNCTOR
      { Structure }
  | INCLUDE FUNCTOR
      { Functor }
;

(* An [include] statement can appear in a structure or in a signature,
   which is why this definition is parameterized. *)
%inline include_statement(thing):
  include_kind
  ext = ext
  attrs1 = attributes
  thing = thing
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Incl.mk thing ~kind:$1 ~attrs ~loc ~docs, ext
  }
;

(* A module type declaration. *)
module_type_declaration:
  MODULE TYPE
  ext = ext
  attrs1 = attributes
  id = mkrhs(ident)
  typ = preceded(EQUAL, module_type)?
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Mtd.mk id ?typ ~attrs ~loc ~docs
  }
;

(* -------------------------------------------------------------------------- *)

(* Opens. *)

open_declaration:
  OPEN
  override = override_flag
  ext = ext
  attrs1 = attributes
  me = module_expr
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Opn.mk me ~override ~attrs ~loc ~docs, ext
  }
;

open_description:
  OPEN
  override = override_flag
  ext = ext
  attrs1 = attributes
  id = mkrhs(mod_ext_longident)
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Opn.mk id ~override ~attrs ~loc ~docs, ext
  }
;

%inline open_dot_declaration: mkrhs(mod_longident)
  { $1 }
;

(* -------------------------------------------------------------------------- *)

/* Module types */

module_type:
  | SIG attrs = attributes s = signature END
      { mkmty ~loc:$sloc ~attrs (Pmty_signature s) }
  | SIG attributes signature error
      { unclosed "sig" $loc($1) "end" $loc($4) }
  | STRUCT error
      { expecting $loc($1) "sig" }
  | FUNCTOR attrs = attributes args = nonempty_functor_args
    MINUSGREATER mty = module_type
      %prec below_WITH
      { let mty =
          match attrs, mty with
          | [], {pmty_desc= Pmty_functor (args', mty'); pmty_attributes= []; _} ->
              Pmty_functor (args @ args', mty')
          | [], {pmty_desc= Pmty_gen (loc, mty'); pmty_attributes= []; _} ->
              Pmty_functor (args @ [mkloc Unit loc], mty')
          | _ -> Pmty_functor (args, mty)
        in
        mkmty ~loc:$sloc ~attrs mty }
  | MODULE TYPE OF attributes module_expr %prec below_LBRACKETAT
      { mkmty ~loc:$sloc ~attrs:$4 (Pmty_typeof $5) }
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | module_type attribute
      { Mty.attr $1 $2 }
  | mkmty(
      mkrhs(mty_longident)
        { Pmty_ident $1 }
    | LPAREN RPAREN MINUSGREATER module_type
        { let arg_loc = make_loc ($startpos($1), $endpos($2)) in
          Pmty_gen(arg_loc, $4) }
    | module_type MINUSGREATER module_type
        %prec below_WITH
        { let arg_loc = make_loc $loc($1) in
          Pmty_functor([mkloc (Named (mknoloc None, $1)) arg_loc], $3) }
    | module_type WITH separated_nonempty_llist(AND, with_constraint)
        { Pmty_with($1, $3) }
/*  | LPAREN MODULE mkrhs(mod_longident) RPAREN
        { Pmty_alias $3 } */
    | module_type WITH mkrhs(mod_ext_longident)
        { Pmty_strengthen($1,$3) }
    | extension
        { Pmty_extension $1 }
    )
    { $1 }
;
(* A signature, which appears between SIG and END (among other places),
   is a list of signature elements. *)
signature:
  extra_sig(flatten(signature_element*))
    { erase_sig_items $1 }
;

(* A signature element is one of the following:
   - a double semicolon;
   - a signature item. *)
%inline signature_element:
    text_sig_SEMISEMI
  | text_sig(signature_item)
      { $1 }
;

(* A signature item. *)
signature_item:
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mksig ~loc:$sloc (Psig_extension ($1, (add_docs_attrs docs $2))) }
  | mksig(
      floating_attribute
        { Psig_attribute $1 }
    | module_declaration
        { Psig_module $1 }
    | module_alias
        { Psig_module $1 }
    | module_subst
        { Psig_modsubst $1 }
    | rec_module_declarations
        { Psig_recmodule $1 }
    | module_type_declaration
        { Psig_modtype $1 }
    | module_type_subst
        { Psig_modtypesubst $1 }
    )
    { $1 }
  | wrap_mksig_ext(
      value_description
        { psig_value $1 }
    | primitive_declaration
        { psig_value $1 }
    | type_declarations
        { psig_type $1 }
    | type_subst_declarations
        { psig_typesubst $1 }
    | sig_type_extension
        { psig_typext $1 }
    | sig_exception_declaration
        { psig_exception $1 }
    | open_description
        { let (body, ext) = $1 in (Psig_open body, ext) }
    | class_descriptions
        { let (ext, l) = $1 in (Psig_class l, ext) }
    | class_type_declarations
        { let (ext, l) = $1 in (Psig_class_type l, ext) }
    )
    { $1 }
  | include_statement(module_type) modalities = optional_atat_modalities_expr
      { let incl, ext = $1 in
        let item = mksig ~loc:$sloc (Psig_include (incl, modalities)) in
        wrap_sig_ext ~loc:$sloc item ext
      }
  | kind_abbreviation_decl
      { let name, jkind = $1 in
        mksig ~loc:$sloc (Psig_kind_abbrev (name, jkind)) }

(* A module declaration. *)
%inline module_declaration:
  MODULE
  ext = ext attrs1 = attributes
  name = mkrhs(module_name)
  args = functor_args
  COLON
  body = module_type
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name args body ~attrs ~loc ~docs
  }
;

(* Module arguments are attached to declarations
(* The body (right-hand side) of a module declaration. *)
module_declaration_body:
    COLON mty = module_type
      { mty }
  | EQUAL error
      { expecting $loc($1) ":" }
  | mkmty(
      arg_and_pos = functor_arg body = module_declaration_body
        { let (_, arg) = arg_and_pos in
          Pmty_functor(arg, body) }
    )
    { $1 }
;
*)

(* A module alias declaration (in a signature). *)
%inline module_alias:
  MODULE
  ext = ext attrs1 = attributes
  name = mkrhs(module_name)
  EQUAL
  body = module_expr_alias
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name [] body ~attrs ~loc ~docs
  }
;
%inline module_expr_alias:
  id = mkrhs(mod_longident)
    { Mty.alias ~loc:(make_loc $sloc) id }
;
(* A module substitution (in a signature). *)
module_subst:
MODULE
  ext = ext attrs1 = attributes
  uid = mkrhs(UIDENT)
  COLONEQUAL
  body = mkrhs(mod_ext_longident)
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Ms.mk uid body ~attrs ~loc ~docs
  }
| MODULE ext attributes mkrhs(UIDENT) COLONEQUAL error
    { expecting $loc($6) "module path" }
;

(* A group of recursive module declarations. *)
%inline rec_module_declarations:
  rec_module_declaration list(and_module_declaration)
    { $1 :: $2 }
;
%inline rec_module_declaration:
  MODULE
  ext = ext
  attrs1 = attributes
  REC
  name = mkrhs(module_name)
  COLON
  mty = module_type
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name [] mty ~attrs ~loc ~docs
  }
;
%inline and_module_declaration:
  AND
  attrs1 = attributes
  name = mkrhs(module_name)
  COLON
  mty = module_type
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ~before:attrs1 ~after:attrs2 () in
    let docs = symbol_docs $sloc in
    let loc = make_loc $sloc in
    let text = symbol_text $symbolstartpos in
    Md.mk name [] mty ~attrs ~loc ~text ~docs
  }
;

(* A module type substitution *)
module_type_subst:
  MODULE TYPE
  ext = ext
  attrs1 = attributes
  id = mkrhs(ident)
  COLONEQUAL
  typ=module_type
  attrs2 = post_item_attributes
  {
    let attrs = Attr.ext_attrs ?ext ~before:attrs1 ~after:attrs2 () in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Mtd.mk id ~typ ~attrs ~loc ~docs
  }


(* -------------------------------------------------------------------------- *)

(* Class declarations. *)

%inline class_declarations:
  xlist(class_declaration, and_class_declaration)
    { $1 }
;
%inline class_declaration:
  CLASS
  ext = ext
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  body = class_fun_binding
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    ext,
    Ci.mk id body ~virt ~params ~attrs ~loc ~docs
  }
;
%inline and_class_declaration:
  AND
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  body = class_fun_binding
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Ci.mk id body ~virt ~params ~attrs ~loc ~text ~docs
  }
;

class_fun_binding:
    EQUAL class_expr
      { $2 }
  | mkclass(
      COLON class_type EQUAL class_expr
        { Pcl_constraint($4, $2) }
    | labeled_simple_pattern class_fun_binding
      { let (_, l,o,p) = $1 in Pcl_fun(l, o, p, $2) }
    ) { $1 }
;

formal_class_parameters:
  params = class_parameters(type_parameter)
    { params }
;

(* -------------------------------------------------------------------------- *)

(* Class expressions. *)

class_expr:
    class_simple_expr
      { $1 }
  | FUN attributes class_fun_def
      { wrap_class_attrs ~loc:$sloc $3 $2 }
  | let_bindings(no_ext) IN class_expr
      { class_of_let_bindings ~loc:$sloc $1 $3 }
  | LET OPEN override_flag attributes mkrhs(mod_longident) IN class_expr
      { let loc = ($startpos($2), $endpos($5)) in
        let od = Opn.mk ~override:$3 ~loc:(make_loc loc) $5 in
        mkclass ~loc:$sloc ~attrs:$4 (Pcl_open(od, $7)) }
  | class_expr attribute
      { Cl.attr $1 $2 }
  | mkclass(
      class_simple_expr nonempty_llist(labeled_simple_expr)
        { Pcl_apply($1, $2) }
    | extension
        { Pcl_extension $1 }
    ) { $1 }
;
class_simple_expr:
  | LPAREN class_expr RPAREN
      { $2 }
  | LPAREN class_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | mkclass(
      tys = actual_class_parameters cid = mkrhs(class_longident)
        { Pcl_constr(cid, tys) }
    | OBJECT attributes class_structure error
        { unclosed "object" $loc($1) "end" $loc($4) }
    | LPAREN class_expr COLON class_type RPAREN
        { Pcl_constraint($2, $4) }
    | LPAREN class_expr COLON class_type error
        { unclosed "(" $loc($1) ")" $loc($5) }
    ) { $1 }
  | OBJECT attributes class_structure END
    { mkclass ~loc:$sloc ~attrs:$2 (Pcl_structure $3) }
;

class_fun_def:
  mkclass(
    labeled_simple_pattern MINUSGREATER e = class_expr
  | labeled_simple_pattern e = class_fun_def
      { let (_, l,o,p) = $1 in Pcl_fun(l, o, p, e) }
  ) { $1 }
;
%inline class_structure:
  |  class_self_pattern extra_cstr(class_fields)
       { Cstr.mk $1 $2 }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { Some (reloc_pat ~loc:$sloc $2) }
  | mkpat(LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, Some $4, []) })
      { Some $1 }
  | /* empty */
      { None }
;
%inline class_fields:
  flatten(text_cstr(class_field)*)
    { $1 }
;
class_field:
  | INHERIT override_flag attributes class_expr
    self = preceded(AS, mkrhs(LIDENT))?
    post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_inherit ($2, $4, self)) ~attrs:($3@$6) ~docs }
  | VAL value post_item_attributes
      { let v, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_val v) ~attrs:(attrs@$3) ~docs }
  | METHOD method_ post_item_attributes
      { let meth, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_method meth) ~attrs:(attrs@$3) ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_constraint $3) ~attrs:($2@$4) ~docs }
  | INITIALIZER attributes seq_expr post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_initializer $3) ~attrs:($2@$4) ~docs }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_extension $1) ~attrs:$2 ~docs }
  | mkcf(floating_attribute
      { Pcf_attribute $1 })
      { $1 }
;
value:
    no_override_flag
    attrs = attributes
    mutable_ = virtual_with_mutable_flag
    label = mkrhs(label) COLON ty = core_type
      { (label, mutable_, Cfk_virtual ty), attrs }
  | override_flag attributes mutable_flag mkrhs(label) EQUAL seq_expr
      { ($4, mv_of_mut $3, Cfk_concrete ($1, $6)), $2 }
  | override_flag attributes mutable_flag mkrhs(label) type_constraint
    EQUAL seq_expr
      { let e = mkexp_constraint ~loc:$sloc ~modes:[] $7 $5 in
        ($4, mv_of_mut $3, Cfk_concrete ($1, e)), $2
      }
;
method_:
    no_override_flag
    attrs = attributes
    private_ = virtual_with_private_flag
    label = mkrhs(label) COLON ty = poly_type
      { (label, private_, Cfk_virtual ty), attrs }
  | override_flag attributes private_flag mkrhs(label) strict_binding
      { let e = $5 in
        let loc = Location.(e.pexp_loc.loc_start, e.pexp_loc.loc_end) in
        ($4, pv_of_priv $3,
        Cfk_concrete ($1, ghexp ~loc (Pexp_poly (e, None)))), $2 }
  | override_flag attributes private_flag mkrhs(label)
    COLON poly_type EQUAL seq_expr
      { let poly_exp =
          let loc = ($startpos($6), $endpos($8)) in
          ghexp ~loc (Pexp_poly($8, Some $6)) in
        ($4, pv_of_priv $3, Cfk_concrete ($1, poly_exp)), $2 }
  | override_flag attributes private_flag mkrhs(label) COLON TYPE newtypes
    DOT core_type EQUAL seq_expr
      { let poly_exp_loc = ($startpos($7), $endpos($11)) in
        let poly_exp =
          let exp, poly =
            (* it seems odd to use the global ~loc here while poly_exp_loc
               is tighter, but this is what ocamlyacc does;
               TODO improve parser.mly *)
            wrap_type_annotation ~loc:$sloc ~modes:[] $7 $9 $11 in
          ghexp ~loc:poly_exp_loc (Pexp_poly(exp, Some poly)) in
        ($4, pv_of_priv $3,
        Cfk_concrete ($1, poly_exp)), $2 }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | mkcty(
      label = arg_label
      domain = tuple_type
      MINUSGREATER
      codomain = class_type
        { let arrow_type = {
            pap_label = label;
            pap_loc = make_loc $sloc;
            pap_type = domain;
            pap_modes = []
          }
          in
          let params, codomain =
            match codomain.pcty_attributes, codomain.pcty_desc with
            | [], Pcty_arrow (params, codomain) -> params, codomain
            | _, _ -> [], codomain
          in
          Pcty_arrow (arrow_type :: params, codomain) }
    ) { $1 }
 ;
class_signature:
    mkcty(
      tys = actual_class_parameters cid = mkrhs(clty_longident)
        { Pcty_constr (cid, tys) }
    | extension
        { Pcty_extension $1 }
    ) { $1 }
  | OBJECT attributes class_sig_body END
      { mkcty ~loc:$sloc ~attrs:$2 (Pcty_signature $3) }
  | OBJECT attributes class_sig_body error
      { unclosed "object" $loc($1) "end" $loc($4) }
  | class_signature attribute
      { Cty.attr $1 $2 }
  | LET OPEN override_flag attributes mkrhs(mod_longident) IN class_signature
      { let loc = ($startpos($2), $endpos($5)) in
        let od = Opn.mk ~override:$3 ~loc:(make_loc loc) $5 in
        mkcty ~loc:$sloc ~attrs:$4 (Pcty_open(od, $7)) }
;
%inline class_parameters(parameter):
  | /* empty */
      { [] }
  | LBRACKET params = separated_nonempty_llist(COMMA, parameter) RBRACKET
      { params }
;
%inline actual_class_parameters:
  tys = class_parameters(core_type)
    { tys }
;
%inline class_sig_body:
    class_self_type extra_csig(class_sig_fields)
      { Csig.mk $1 $2 }
;
class_self_type:
   ioption (LPAREN core_type RPAREN { $2 })
      { $1 }
;
%inline class_sig_fields:
  flatten(text_csig(class_sig_field)*)
    { $1 }
;
class_sig_field:
    INHERIT attributes class_signature post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_inherit $3) ~attrs:($2@$4) ~docs }
  | VAL attributes value_type post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_val $3) ~attrs:($2@$4) ~docs }
  | METHOD attributes private_virtual_flags mkrhs(label) COLON poly_type
    post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_method ($4, $3, $6)) ~attrs:($2@$7) ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_constraint $3) ~attrs:($2@$4) ~docs }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_extension $1) ~attrs:$2 ~docs }
  | mkctf(floating_attribute
      { Pctf_attribute $1 })
      { $1 }
;
%inline value_type:
  flags = mutable_virtual_flags
  label = mkrhs(label)
  COLON
  ty = core_type
  {
    label, flags, ty
  }
;
%inline constrain:
    core_type EQUAL core_type
    { $1, $3, make_loc $sloc }
;
constrain_field:
  core_type EQUAL core_type
    { $1, $3 }
;
(* A group of class descriptions. *)
%inline class_descriptions:
  xlist(class_description, and_class_description)
    { $1 }
;
%inline class_description:
  CLASS
  ext = ext
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  COLON
  cty = class_type
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      ext,
      Ci.mk id cty ~virt ~params ~attrs ~loc ~docs
    }
;
%inline and_class_description:
  AND
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  COLON
  cty = class_type
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id cty ~virt ~params ~attrs ~loc ~text ~docs
    }
;
class_type_declarations:
  xlist(class_type_declaration, and_class_type_declaration)
    { $1 }
;
%inline class_type_declaration:
  CLASS TYPE
  ext = ext
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  EQUAL
  csig = class_signature
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      ext,
      Ci.mk id csig ~virt ~params ~attrs ~loc ~docs
    }
;
%inline and_class_type_declaration:
  AND
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  EQUAL
  csig = class_signature
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id csig ~virt ~params ~attrs ~loc ~text ~docs
    }
;

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { $1 }
  | mkexp(expr SEMI seq_expr
    { Pexp_sequence($1, $3) })
    { $1 }
  | expr SEMI PERCENT attr_id seq_expr
    { let seq = mkexp ~loc:$sloc (Pexp_sequence ($1, $5)) in
      let payload = PStr [mkstrexp seq []] in
      mkexp ~loc:$sloc (Pexp_extension ($4, payload)) }
;
labeled_simple_pattern:
    QUESTION LPAREN optional_local x=label_let_pattern opt_default RPAREN
      { let lbl, pat, cty, modes = x in
        $3, mk_optional lbl $sloc, $5,
        mkpat_local_if $3 (mkpat_with_modes ~loc:$loc(x) ~pat ~cty ~modes) }
  | QUESTION label_var
      { false, mk_optional (fst $2) $sloc, None, snd $2 }
  | OPTLABEL LPAREN optional_local x=let_pattern opt_default RPAREN
      { let pat, cty, modes = x in
        $3, mk_optional $1 $sloc, $5,
        mkpat_local_if $3 (mkpat_with_modes ~loc:$loc(x) ~pat ~cty ~modes) }
  | OPTLABEL pattern_var
      { false, mk_optional $1 $sloc, None, $2 }
  | TILDE LPAREN optional_local x=label_let_pattern RPAREN
      { let lbl, pat, cty, modes = x in
        let pat = mkpat_with_modes ~loc:$loc(x) ~pat ~cty ~modes in
        let arg_label, pat, default_value =
          transl_label
            ~pattern:pat
            ~arg_label:(mk_labelled lbl $sloc)
            ~loc:(make_loc $sloc)
        in
        $3, arg_label, default_value, mkpat_local_if $3 pat
      }
  | TILDE label_var
      { false, mk_labelled (fst $2) $sloc, None, snd $2 }
  | LABEL simple_pattern
      { let arg_label, pat, default_value =
          transl_label
            ~pattern:($2)
            ~arg_label:(mk_labelled $1 $sloc)
            ~loc:(make_loc $sloc)
        in
        false, arg_label, default_value, pat }
  | LABEL LPAREN LOCAL pattern RPAREN
      { let arg_label, pat, default_value =
          transl_label
            ~pattern:(mkpat_stack $4)
            ~arg_label:(mk_labelled $1 $sloc)
            ~loc:(make_loc $sloc)
        in
        true, arg_label, default_value, pat }
  | simple_pattern
      { false, Nolabel, None, $1 }
  | LPAREN LOCAL x=let_pattern RPAREN
      { let pat, cty, modes = x in
        true, Nolabel, None,
        mkpat_stack (mkpat_with_modes ~loc:$loc(x) ~pat ~cty ~modes) }
  | LABEL LPAREN x=poly_pattern RPAREN
      { let pat, cty, modes = x in
        false, mk_labelled $1 $sloc, None,
        mkpat_with_modes ~loc:$loc(x) ~pat ~cty ~modes }
  | LABEL LPAREN LOCAL x=poly_pattern RPAREN
      { let pat, cty, modes = x in
        true, mk_labelled $1 $sloc, None,
        mkpat_stack (mkpat_with_modes ~loc:$sloc ~pat ~cty ~modes) }
  | LPAREN x=poly_pattern RPAREN
      { let pat, cty, modes = x in
        false, Nolabel, None,
        mkpat_with_modes ~loc:$loc(x) ~pat ~cty ~modes }
;

pattern_var:
  mkpat(
      mkrhs(LIDENT)     { Ppat_var $1 }
    | UNDERSCORE        { Ppat_any }
  ) { $1 }
;

%inline opt_default:
  preceded(EQUAL, seq_expr)?
    { $1 }
;
label_let_pattern:
   x = label_var modes = optional_at_mode_expr
      { let lab, pat = x in
        lab, pat, None, modes
      }
  | x = label_var COLON cty = core_type modes = optional_atat_mode_expr
      { let lab, pat = x in
        lab, pat, Some cty, modes
      }
  | x = label_var COLON
    cty = mktyp (vars = typevar_list DOT ty = core_type { Ptyp_poly (vars, ty) })
    modes = optional_atat_mode_expr
      { let lab, pat = x in
        lab, pat, Some cty, modes
      }
;
%inline label_var:
    mkrhs(LIDENT)
      { ($1.Location.txt, mkpat ~loc:$sloc (Ppat_var $1)) }
;
let_pattern:
    pat=pattern modes=optional_at_mode_expr
      { pat, None, modes }
  | pat=pattern COLON cty=core_type modes=optional_atat_mode_expr
      { pat, Some cty, modes }
  | poly_pattern
      { $1 }
;

%inline poly_pattern:
  pat = pattern
  COLON
  cty = mktyp(vars = typevar_list DOT ty = core_type
          { Ptyp_poly(vars, ty) })
  modes = optional_atat_mode_expr
  { pat, Some cty, modes }
;

%inline indexop_expr(dot, index, right):
  | array=simple_expr d=dot LPAREN i=index RPAREN r=right
    { array, d, Paren,   i, r }
  | array=simple_expr d=dot LBRACE i=index RBRACE r=right
    { array, d, Brace,   i, r }
  | array=simple_expr d=dot LBRACKET i=index RBRACKET r=right
    { array, d, Bracket, i, r }
;

%inline indexop_error(dot, index):
  | simple_expr dot _p=LPAREN index  _e=error
    { indexop_unclosed_error $loc(_p)  Paren $loc(_e) }
  | simple_expr dot _p=LBRACE index  _e=error
    { indexop_unclosed_error $loc(_p) Brace $loc(_e) }
  | simple_expr dot _p=LBRACKET index  _e=error
    { indexop_unclosed_error $loc(_p) Bracket $loc(_e) }
;

%inline qualified_dotop: ioption(DOT mkrhs(mod_longident) {$2}) DOTOP { $1, $2 };

expr:
    simple_expr %prec below_HASH
      { $1 }
  | expr_attrs
      { let desc, attrs = $1 in
        mkexp_attrs ~loc:$sloc desc attrs }
  | expr_
      { $1 }
  | let_bindings(ext) IN seq_expr
      { expr_of_let_bindings ~loc:$sloc $1 $3 }
  | pbop_op = mkrhs(LETOP) bindings = letop_bindings IN body = seq_expr
      { let (pbop_pat, pbop_exp, pbop_is_pun, rev_ands) = bindings in
        let ands = List.rev rev_ands in
        let pbop_loc = make_loc $sloc in
        let let_ = {pbop_op; pbop_pat; pbop_exp; pbop_is_pun; pbop_loc} in
        mkexp ~loc:$sloc (Pexp_letop{ let_; ands; body}) }
  | expr COLONCOLON e = expr
      { match e.pexp_desc, e.pexp_attributes with
        | Pexp_cons l, [] -> Exp.cons ~loc:(make_loc $sloc) ($1 :: l)
        | _ -> Exp.cons ~loc:(make_loc $sloc) [$1; e] }
  | mkrhs(label) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setinstvar($1, $3)) }
  | simple_expr DOT mkrhs(label_longident) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setfield($1, $3, $5)) }
  | indexop_expr(DOT, seq_expr, LESSMINUS v=expr {Some v})
    { mk_builtin_indexop_expr ~loc:$sloc $1 }
  | indexop_expr(qualified_dotop, expr_semi_list, LESSMINUS v=expr {Some v})
    { mk_dotop_indexop_expr ~loc:$sloc $1 }
  | FUN ext_attributes LPAREN TYPE newtypes RPAREN fun_def
    { let loc = $sloc in
      wrap_exp_attrs ~loc (mk_newtypes ~loc $5 $7) $2 }
  | FUN ext_attributes LPAREN TYPE
    name=mkrhs(LIDENT {Some $1}) COLON jkind=jkind_annotation
    RPAREN fun_def
    { let loc = $sloc in
      wrap_exp_attrs ~loc (mk_newtypes ~loc:$sloc [name, jkind] $9) $2 }
  | expr attribute
      { Exp.attr $1 $2 }
/* BEGIN AVOID */
  (* Allowed in exprs. Commented-out to reduce diffs with upstream.
  | UNDERSCORE
     { not_expecting $loc($1) "wildcard \"_\"" }
  *)
/* END AVOID */
  | LOCAL seq_expr
     { mkexp_stack ~loc:$sloc $2 }
  | EXCLAVE seq_expr
     { mkexp_exclave ~loc:$sloc $2 }
;
%inline expr_attrs:
  | LET MODULE ext_attributes mkrhs(module_name) functor_args module_binding_body IN seq_expr
      { Pexp_letmodule($4, $5, $6, $8), $3 }
  | LET EXCEPTION ext_attributes let_exception_declaration IN seq_expr
      { Pexp_letexception($4, $6), $3 }
  | LET OPEN override_flag ext_attributes module_expr IN seq_expr
      { let open_loc = make_loc ($startpos($2), $endpos($5)) in
        let od = Opn.mk $5 ~override:$3 ~loc:open_loc in
        Pexp_letopen(od, $7), $4 }
  | FUNCTION ext_attributes match_cases
      { Pexp_function $3, $2 }
  | FUN ext_attributes fun_param fun_def
      { Pexp_fun($3, $4), $2 }
  | MATCH ext_attributes seq_expr WITH match_cases
      { Pexp_match($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH match_cases
      { Pexp_try($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH error
      { syntax_error() }
  | IF ext_attributes seq_expr THEN expr ELSE else_=expr
      { let ext, attrs = $2 in
        let br = { if_cond = $3; if_body = $5; if_attrs = attrs } in
        let ite =
          match else_.pexp_desc with
          | Pexp_ifthenelse(brs, else_) -> Pexp_ifthenelse(br :: brs, else_)
          | _ -> Pexp_ifthenelse([br], Some else_)
        in
        ite, (ext, []) }
  | IF ext_attributes seq_expr THEN expr
      { let ext, attrs = $2 in
        let br = { if_cond = $3; if_body = $5; if_attrs = attrs } in
        Pexp_ifthenelse ([br], None), (ext, []) }
  | WHILE ext_attributes seq_expr do_done_expr
      { Pexp_while($3, $4), $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
    do_done_expr
      { Pexp_for($3, $5, $7, $6, $8), $2 }
  | ASSERT ext_attributes simple_expr %prec below_HASH
      { Pexp_assert $3, $2 }
  | LAZY ext_attributes simple_expr %prec below_HASH
      { Pexp_lazy $3, $2 }
;
%inline do_done_expr:
  | DO e = seq_expr DONE
      { e }
  | DO seq_expr error
      { unclosed "do" $loc($1) "done" $loc($2) }
;
%inline expr_:
  | simple_expr nonempty_llist(labeled_simple_expr)
      { mkexp ~loc:$sloc (Pexp_apply($1, $2)) }
  | STACK simple_expr
      { if Erase_jane_syntax.should_erase ()
        then $2
        else mkexp ~loc:$sloc (Pexp_stack $2) }
  | labeled_tuple %prec below_COMMA
      { mkexp ~loc:$sloc (Pexp_tuple $1) }
  | mkrhs(constr_longident) simple_expr %prec below_HASH
      { mkexp ~loc:$sloc (Pexp_construct($1, Some $2)) }
  | name_tag simple_expr %prec below_HASH
      { mkexp ~loc:$sloc (Pexp_variant($1, Some $2)) }
  | e1 = expr op = op(infix_operator) e2 = expr
      { mkexp ~loc:$sloc (mkinfix e1 op e2) }
  | subtractive expr %prec prec_unary_minus
      { mkexp ~loc:$sloc (mkuminus ~oploc:$loc($1) $1 $2) }
  | additive expr %prec prec_unary_plus
      { mkexp ~loc:$sloc (mkuplus ~oploc:$loc($1) $1 $2) }
;

simple_expr:
  | LPAREN e = seq_expr RPAREN
      { match e.pexp_desc with
        | Pexp_pack _ ->
            mkexp ~loc:$sloc (Pexp_parens e)
        | _ -> reloc_exp ~loc:$sloc e }
  | LPAREN seq_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN seq_expr type_constraint_with_modes RPAREN
      { let t, m = $3 in
        mkexp_constraint ~loc:$sloc ~modes:m $2 t }
  | indexop_expr(DOT, seq_expr, { None })
      { mk_builtin_indexop_expr ~loc:$sloc $1 }
  (* Immutable array indexing is a regular operator, so it doesn't need its own
     rule and is handled by the next case *)
  | indexop_expr(qualified_dotop, expr_semi_list, { None })
      { mk_dotop_indexop_expr ~loc:$sloc $1 }
  | indexop_error (DOT, seq_expr) { $1 }
  | indexop_error (qualified_dotop, expr_semi_list) { $1 }
  | simple_expr_attrs
    { let desc, attrs = $1 in
      mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(simple_expr_)
      { $1 }
;
%inline simple_expr_attrs:
  | BEGIN ext_attributes seq_expr END
      { Pexp_beginend $3, $2 }
  | BEGIN ext_attributes END
      { Pexp_construct (mkloc (Lident "()") (make_loc $sloc), None), $2 }
  | BEGIN ext_attributes seq_expr error
      { unclosed "begin" $loc($1) "end" $loc($4) }
  | NEW ext_attributes mkrhs(class_longident)
      { Pexp_new($3), $2 }
  | LPAREN MODULE ext_attributes module_expr RPAREN
      { Pexp_pack ($4, None), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { Pexp_pack ($4, Some $6), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | OBJECT ext_attributes class_structure END
      { Pexp_object $3, $2 }
  | OBJECT ext_attributes class_structure error
      { unclosed "object" $loc($1) "end" $loc($4) }
;

comprehension_iterator:
  | EQUAL expr direction_flag expr
      { Range { start = $2 ; stop = $4 ; direction = $3 } }
  | IN expr
      { In $2 }
;

comprehension_clause_binding:
  | attributes pattern comprehension_iterator
      { { pattern = $2 ; iterator = $3 ; attributes = $1 } }
  (* We can't write [[e for local_ x = 1 to 10]], because the [local_] has to
     move to the RHS and there's nowhere for it to move to; besides, you never
     want that [int] to be [local_].  But we can parse [[e for local_ x in xs]].
     We have to have that as a separate rule here because it moves the [local_]
     over to the RHS of the binding, so we need everything to be visible. *)
  | attributes LOCAL pattern IN expr
      {
          { pattern    = $3
          ; iterator   = In (mkexp_stack ~loc:$sloc (* ~kwd_loc:($loc($2)) *) $5)
          ; attributes = $1
          }
      }
;

comprehension_clause:
  | FOR separated_nonempty_llist(AND, comprehension_clause_binding)
      { For $2 }
  | WHEN expr
      { When $2 }

%inline comprehension(lbracket, rbracket):
  lbracket expr nonempty_llist(comprehension_clause) rbracket
    { { comp_body = $2; clauses = $3 } }
;

%inline comprehension_expr:
  | comprehension(LBRACKET,RBRACKET)
      { Pexp_list_comprehension  $1 }
  | comprehension(LBRACKETBAR,BARRBRACKET)
      { Pexp_array_comprehension
          (Mutable Location.none, $1) }
  | comprehension(LBRACKETCOLON,COLONRBRACKET)
      { Pexp_array_comprehension (Immutable, $1) }
;

%inline array_simple(ARR_OPEN, ARR_CLOSE, contents_semi_list):
  | ARR_OPEN contents_semi_list ARR_CLOSE
      { Generic_array.Literal $2 }
  | ARR_OPEN contents_semi_list error
      { Generic_array.Unclosed($loc($1),$loc($3)) }
  | ARR_OPEN ARR_CLOSE
      { Generic_array.Literal [] }
;

%inline array_exprs(ARR_OPEN, ARR_CLOSE):
  | array_simple(ARR_OPEN, ARR_CLOSE, expr_semi_list)
      { $1 }
  | od=open_dot_declaration DOT ARR_OPEN expr_semi_list ARR_CLOSE
      { Generic_array.Opened_literal(od, $startpos($3), $endpos, $4) }
  | od=open_dot_declaration DOT ARR_OPEN ARR_CLOSE
      { (* TODO: review the location of Pexp_array *)
        Generic_array.Opened_literal(od, $startpos($3), $endpos, []) }
  | mod_longident DOT
    ARR_OPEN expr_semi_list error
      { Generic_array.Unclosed($loc($3), $loc($5)) }
;

%inline array_patterns(ARR_OPEN, ARR_CLOSE):
  | array_simple(ARR_OPEN, ARR_CLOSE, pattern_semi_list)
      { $1 }
;

(* Jane Street extension *)
%inline hash:
    | HASH { () }
    | HASH_SUFFIX { () }
;
(* End Jane Street extension *)

%inline simple_expr_:
  | mkrhs(val_longident)
      { Pexp_ident ($1) }
  | constant
      { Pexp_constant $1 }
  | mkrhs(constr_longident) %prec prec_constant_constructor
      { Pexp_construct($1, None) }
  | name_tag %prec prec_constant_constructor
      { Pexp_variant($1, None) }
  | op(PREFIXOP) simple_expr
      { Pexp_prefix($1, $2) }
  | op(BANG {"!"}) simple_expr
      { Pexp_prefix($1, $2) }
  | LBRACELESS object_expr_content GREATERRBRACE
      { Pexp_override $2 }
  | LBRACELESS object_expr_content error
      { unclosed "{<" $loc($1) ">}" $loc($3) }
  | LBRACELESS GREATERRBRACE
      { Pexp_override [] }
  | simple_expr DOT mkrhs(label_longident)
      { Pexp_field($1, $3) }
  | simple_expr DOTHASH mkrhs(label_longident)
      { Pexp_unboxed_field($1, $3) }
  | od=open_dot_declaration DOT LPAREN seq_expr RPAREN
      { Pexp_open(od, $4) }
  | od=open_dot_declaration DOT LBRACELESS object_expr_content GREATERRBRACE
      { (* TODO: review the location of Pexp_override *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_override $4)) }
  | mod_longident DOT LBRACELESS object_expr_content error
      { unclosed "{<" $loc($3) ">}" $loc($5) }
  | simple_expr hash mkrhs(label)
      { Pexp_send($1, $3) }
  | simple_expr op(HASHOP) simple_expr
      { mkinfix $1 $2 $3 }
  | extension
      { let (({ txt = id; _ }, _) as p) = $1 in
        if not (Erase_jane_syntax.should_erase ()) then Pexp_extension p
        else (
          match id with
          | "src_pos" ->
            Pexp_ident
              { loc = make_loc $sloc
              ; txt = Ldot (Ldot (Lident "Stdlib", "Lexing"), "dummy_pos")
              }
          | _ -> Pexp_extension p
        )
      }
  | UNDERSCORE
      { Pexp_hole }
  | od=open_dot_declaration DOT mkrhs(LPAREN RPAREN {Lident "()"})
      { Pexp_open(od, mkexp ~loc:($loc($3)) (Pexp_construct($3, None))) }
  | mod_longident DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | LBRACE record_expr_content RBRACE
      { let (exten, fields) = $2 in
        Pexp_record(fields, exten) }
  | HASHLBRACE record_expr_content RBRACE
      { let (exten, fields) = $2 in
        Pexp_record_unboxed_product(fields, exten) }
  | LBRACE record_expr_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
  | od=open_dot_declaration DOT LBRACE record_expr_content RBRACE
      { let (exten, fields) = $4 in
        Pexp_open(od, mkexp ~loc:($startpos($3), $endpos)
                        (Pexp_record(fields, exten))) }
  | mod_longident DOT LBRACE record_expr_content error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | array_exprs(LBRACKETBAR, BARRBRACKET)
      { Generic_array.expression
          "[|" "|]"
          (fun elts -> Pexp_array (Mutable Location.none, elts))
          $1 }
  | array_exprs(LBRACKETCOLON, COLONRBRACKET)
      { Generic_array.expression
          "[:" ":]"
          (fun elts -> Pexp_array (Immutable, elts))
          $1 }
  | LBRACKET expr_semi_list RBRACKET
      { Pexp_list $2 }
  | LBRACKET expr_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
  | comprehension_expr { $1 }
  | od=open_dot_declaration DOT comprehension_expr
      { Pexp_open(od, mkexp ~loc:($loc($3)) $3) }
  | od=open_dot_declaration DOT LBRACKET expr_semi_list RBRACKET
      { let list_exp = mkexp ~loc:($startpos($3), $endpos) (Pexp_list $4) in
        Pexp_open(od, list_exp) }
  | od=open_dot_declaration DOT mkrhs(LBRACKET RBRACKET {Lident "[]"})
      { Pexp_open(od, mkexp ~loc:$loc($3) (Pexp_construct($3, None))) }
  | mod_longident DOT
    LBRACKET expr_semi_list error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | od=open_dot_declaration DOT LPAREN MODULE ext_attributes module_expr COLON
    package_type RPAREN
      { let modexp =
          mkexp_attrs ~loc:($startpos($3), $endpos)
            (Pexp_pack ($6, Some $8)) $5 in
        Pexp_open(od, modexp) }
  | mod_longident DOT
    LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($3) ")" $loc($8) }
  | HASHLPAREN labeled_tuple RPAREN
      { Pexp_unboxed_tuple $2 }
;
labeled_simple_expr:
    simple_expr %prec below_HASH
      { Nolabel, $1 }
  | LABEL simple_expr %prec below_HASH
      { mk_labelled $1 $sloc, $2 }
  | TILDE label = LIDENT
      { let loc = $loc(label) in
        mk_labelled label $sloc, mkexpvar ~loc label }
  | TILDE LPAREN label = LIDENT ty = type_constraint RPAREN
      { mk_labelled label $sloc,
        mkexp_constraint ~loc:($startpos($2), $endpos) ~modes:[]
          (mkexpvar ~loc:$loc(label) label) ty }
  | QUESTION label = LIDENT
      { let loc = $loc(label) in
        mk_optional label $sloc, mkexpvar ~loc label }
  | OPTLABEL simple_expr %prec below_HASH
      { mk_optional $1 $sloc, $2 }
;
%inline lident_list:
  xs = mkrhs(LIDENT)+
    { xs }
;
%inline let_ident:
    val_ident { mkpatvar ~loc:$sloc $1 }
;
%inline pvc_modes:
  | at_mode_expr {None, $1}
  | COLON core_type optional_atat_mode_expr {
      Some(Pvc_constraint { locally_abstract_univars=[]; typ=$2 }), $3
    }
;
let_binding_body_no_punning:
    let_ident strict_binding
      { ($1, $2, None, []) }
  | optional_local let_ident constraint_ EQUAL seq_expr
      { let v = $2 in (* PR#7344 *)
        let typ, modes = $3 in
        let t =
          Option.map (function
          | Pconstraint typ ->
             Pvc_constraint { locally_abstract_univars = []; typ }
          | Pcoerce (ground, coercion) -> Pvc_coercion { ground; coercion }
          ) typ
        in
        let pat = mkpat_local_if $1 v in
        let exp = mkexp_local_if $1 ~loc:$sloc $5 in
        (pat, exp, t, modes)
        }
  | optional_local let_ident COLON poly(core_type) modes=optional_atat_mode_expr EQUAL seq_expr
    {
      let t = ghtyp ~loc:($loc($4)) $4 in
      let pat = mkpat_local_if $1 $2 in
      let exp = mkexp_local_if $1 ~loc:$sloc $7 in
      (pat, exp, Some (Pvc_constraint { locally_abstract_univars = []; typ=t }), modes)
    }
  | let_ident COLON TYPE newtypes DOT core_type modes=optional_atat_mode_expr EQUAL seq_expr
    { let constraint' =
        Pvc_constraint { locally_abstract_univars=$4; typ = $6}
      in
      ($1, $9, Some constraint', modes) }
  | pattern_no_exn EQUAL seq_expr
      { ($1, $3, None, []) }
  | simple_pattern_not_ident pvc_modes EQUAL seq_expr
    {
      let pvc, modes = $2 in
      ($1, $4, pvc, modes)
    }
  | LOCAL let_ident local_strict_binding
      { ($2, mkexp_stack ~loc:$sloc $3, None, []) }
  | LPAREN let_ident modes=at_mode_expr RPAREN strict_binding
    {
      ($2, $5, None, modes)
    }
;
let_binding_body:
  | let_binding_body_no_punning
      { let p,e,c,modes = $1 in (p,e,c,modes,false) }
/* BEGIN AVOID */
  | val_ident %prec below_HASH
      { (mkpatvar ~loc:$loc $1, mkexpvar ~loc:$loc $1, None, [], true) }
  (* The production that allows puns is marked so that [make list-parse-errors]
     does not attempt to exploit it. That would be problematic because it
     would then generate bindings such as [let x], which are rejected by the
     auxiliary function [addlb] via a call to [syntax_error]. *)
/* END AVOID */
;
(* The formal parameter EXT can be instantiated with ext or no_ext
   so as to indicate whether an extension is allowed or disallowed. *)
let_bindings(EXT):
    let_binding(EXT)                            { $1 }
  | let_bindings(EXT) and_let_binding           { addlb $1 $2 }
;
%inline let_binding(EXT):
  LET
  ext = EXT
  attrs1 = attributes
  rec_flag = rec_flag
  body = let_binding_body
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      mklbs ext rec_flag (mklb ~loc:$sloc true body attrs)
    }
;
and_let_binding:
  AND
  attrs1 = attributes
  body = let_binding_body
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      mklb ~loc:$sloc false body attrs
    }
;
letop_binding_body:
    pat = let_ident exp = strict_binding
      { (pat, exp, false) }
  | val_ident
      (* Let-punning *)
      { (mkpatvar ~loc:$loc $1, mkexpvar ~loc:$loc $1, true) }
  | pat = simple_pattern COLON typ = core_type EQUAL exp = seq_expr
      { let loc = ($startpos(pat), $endpos(typ)) in
        (ghpat ~loc (Ppat_constraint(pat, Some typ, [])), exp, false) }
  | pat = pattern_no_exn EQUAL exp = seq_expr
      { (pat, exp, false) }
;
letop_bindings:
    body = letop_binding_body
      { let let_pat, let_exp, let_is_pun = body in
        let_pat, let_exp, let_is_pun, [] }
  | bindings = letop_bindings pbop_op = mkrhs(ANDOP) body = letop_binding_body
      { let let_pat, let_exp, let_is_pun, rev_ands = bindings in
        let pbop_pat, pbop_exp, pbop_is_pun = body in
        let pbop_loc = make_loc $sloc in
        let and_ = {pbop_op; pbop_pat; pbop_exp; pbop_is_pun; pbop_loc} in
        let_pat, let_exp, let_is_pun, and_ :: rev_ands }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { mkexp_constraint ~loc:$sloc ~modes:[] $3 $1 }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | fun_param fun_binding
      { ghexp ~loc:$sloc (Pexp_fun($1, $2)) }
  | LPAREN TYPE newtypes RPAREN fun_binding
      { mk_newtypes ~loc:$sloc $3 $5 }
  | LPAREN TYPE
    name=mkrhs(LIDENT {Some $1}) COLON jkind=jkind_annotation
    RPAREN fun_binding
      { mk_newtypes ~loc:$sloc [name, jkind] $7 }
;
local_fun_binding:
    local_strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { wrap_exp_stack (mkexp_constraint ~loc:$sloc ~modes:[] $3 $1) }
;
local_strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern local_fun_binding
      {
        let is_local, l, o, p = $1 in
        let pparam = { pparam_loc = make_loc $sloc; pparam_desc = Pparam_val (is_local, l, o, p) } in
        ghexp ~loc:$sloc (Pexp_fun(pparam, $2)) }
  | LPAREN TYPE newtypes RPAREN local_fun_binding
      { mk_newtypes ~loc:$sloc $3 $5 }
  | LPAREN TYPE
    name=mkrhs(LIDENT {Some $1}) COLON jkind=jkind_annotation
    RPAREN fun_binding
      { mk_newtypes ~loc:$sloc [name, jkind] $7 }
;
%inline match_cases:
  xs = preceded_or_separated_nonempty_llist(BAR, match_case)
    { xs }
;
match_case:
    pattern MINUSGREATER seq_expr
      { Exp.case $1 $3 }
  | pattern WHEN seq_expr MINUSGREATER seq_expr
      { Exp.case $1 ~guard:$3 $5 }
  | pattern MINUSGREATER DOT
      { Exp.case $1 (Exp.unreachable ~loc:(make_loc $loc($3)) ()) }
;
fun_param:
  | labeled_simple_pattern
      { let is_local, l, o, p = $1 in
        { pparam_loc = make_loc $sloc; pparam_desc = Pparam_val (is_local, l, o, p) } }
;
fun_def:
    MINUSGREATER seq_expr
      { $2 }
  | mkexp(COLON atomic_type MINUSGREATER seq_expr
      { Pexp_constraint ($4, Some $2, []) })
      { $1 }
/* Cf #5939: we used to accept (fun p when e0 -> e) */
  | fun_param fun_def
      { ghexp ~loc:$sloc (Pexp_fun($1, $2)) }
  | LPAREN TYPE newtypes RPAREN fun_def
      { mk_newtypes ~loc:$sloc $3 $5 }
  | LPAREN TYPE
    name=mkrhs(LIDENT {Some $1}) COLON jkind=jkind_annotation
    RPAREN fun_def
      { mk_newtypes ~loc:$sloc [name, jkind] $7 }
;

(* Parsing labeled tuple expressions

   The grammar we want to parse is something like:

     labeled_tuple_element := expr | ~x:expr | ~x | ~(x:ty)
     labeled_tuple := lt_element [, lt_element]+

   (The last case of [labeled_tuple_element] is a punned label with a type
   constraint, which is allowed for functions, so we allow it here).

   So you might think [labeled_tuple] could therefore just be:

     labeled_tuple :
       separated_nontrivial_llist(COMMA, labeled_tuple_element)

   But this doesn't work:

   - If we don't mark [labeled_tuple_element] %inline, this causes many
     reduce/reduce conflicts (basically just ambiguities) because
     [labeled_tuple_element] trivially reduces to [expr].

   - If we do mark [labeled_tuple_element] %inline, it is not allowed to have
     %prec annotations.  Menhir doesn't permit these on %inline non-terminals
     that are used in non-tail position.

   To get around this, we do mark it inlined, and then because we can only use
   it in tail position it is _manually_ inlined into the occurrences in
   [separated_nontrivial_llist] where it doesn't appear in tail position.  This
   results in [labeled_tuple] and [reversed_labeled_tuple_body] below.  So the
   latter is just a list of comma-separated labeled tuple elements, with length
   at least two, where the first element in the base case is inlined (resulting
   in one base case for each case of [labeled_tuple_element].  *)
%inline labeled_tuple_element :
  | expr
     { None, $1 }
  | LABEL simple_expr %prec below_HASH
     { let label = mkrhs $1 $loc($1) in
       Some label, $2 }
  | TILDE label = LIDENT
     { let loc = $loc(label) in
       let lbl = ghrhs label loc in
       Some lbl, mkexpvar ~loc label }
  | TILDE LPAREN label = LIDENT ty = type_constraint RPAREN %prec below_HASH
      { let lbl = ghrhs label $loc(label) in
        Some lbl,
        mkexp_constraint
          ~loc:($startpos($2), $endpos) ~modes:[] (mkexpvar ~loc:$loc(label) label) ty }
;
reversed_labeled_tuple_body:
  (* > 2 elements *)
  xs = reversed_labeled_tuple_body
  COMMA
  x = labeled_tuple_element
    { x :: xs }
  (* base cases (2 elements) *)
| x1 = expr
  COMMA
  x2 = labeled_tuple_element
    { [ x2; None, x1 ] }
| l1 = LABEL x1 = simple_expr
  COMMA
  x2 = labeled_tuple_element
  { let label = mkrhs l1 $loc(l1) in
    [ x2; Some label, x1 ] }
| TILDE l1 = LIDENT
  COMMA
  x2 = labeled_tuple_element
  { let loc = $loc(l1) in
    let label = ghrhs l1 loc in
    [ x2; Some label, mkexpvar ~loc l1] }
| TILDE LPAREN l1 = LIDENT ty1 = type_constraint RPAREN
  COMMA
  x2 = labeled_tuple_element
  { let x1 =
      mkexp_constraint
        ~loc:($startpos($2), $endpos) ~modes:[] (mkexpvar ~loc:$loc(l1) l1) ty1
    in
    let label = ghrhs l1 $loc(l1) in
    [ x2; Some label, x1] }
;
%inline labeled_tuple:
  xs = rev(reversed_labeled_tuple_body)
    { xs }
;

record_expr_content:
  eo = ioption(terminated(simple_expr, WITH))
  fields = separated_or_terminated_nonempty_list(SEMI, record_expr_field)
    { eo, fields }
;
%inline record_expr_field:
  | label = mkrhs(label_longident)
    c = type_constraint?
    eo = preceded(EQUAL, expr)?
      { label, c, eo }
;
%inline object_expr_content:
  xs = separated_or_terminated_nonempty_list(SEMI, object_expr_field)
    { xs }
;
%inline object_expr_field:
    label = mkrhs(label)
    oe = preceded(EQUAL, expr)?
      { let label, e =
          match oe with
          | None ->
              (* No expression; this is a pun. Desugar it. *)
              make_ghost label, exp_of_label label
          | Some e ->
              label, e
        in
        label, e }
;
%inline expr_semi_list:
  es = separated_or_terminated_nonempty_list(SEMI, expr)
    { es }
;
type_constraint:
  | COLON core_type                             { Pconstraint $2 }
  | COLON core_type COLONGREATER core_type      { Pcoerce (Some $2, $4) }
  | COLONGREATER core_type                      { Pcoerce (None, $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

%inline type_constraint_with_modes:
  | type_constraint optional_atat_mode_expr
    { $1, $2 }
;

%inline constraint_:
  | type_constraint_with_modes
    { let ty, modes = $1 in
      Some ty, modes }
  | at_mode_expr
    { None, $1 }
;

(* the thing between the [type] and the [.] in
   [let : type <<here>>. 'a -> 'a = ...] *)
newtypes: (* : (string with_loc * jkind_annotation option) list *)
  newtype+
    { $1 }

newtype: (* : string with_loc * jkind_annotation option *)
    mkrhs(LIDENT {Some $1}) { $1, None }
  | LPAREN
    name=mkrhs(LIDENT {Some $1}) COLON jkind=jkind_annotation
    RPAREN
      { name, jkind }

/* Patterns */

(* Whereas [pattern] is an arbitrary pattern, [pattern_no_exn] is a pattern
   that does not begin with the [EXCEPTION] keyword. Thus, [pattern_no_exn]
   is the intersection of the context-free language [pattern] with the
   regular language [^EXCEPTION .*].

   Ideally, we would like to use [pattern] everywhere and check in a later
   phase that EXCEPTION patterns are used only where they are allowed (there
   is code in typing/typecore.ml to this end). Unfortunately, in the
   definition of [let_binding_body], we cannot allow [pattern]. That would
   create a shift/reduce conflict: upon seeing LET EXCEPTION ..., the parser
   wouldn't know whether this is the beginning of a LET EXCEPTION construct or
   the beginning of a LET construct whose pattern happens to begin with
   EXCEPTION. The conflict is avoided there by using [pattern_no_exn] in the
   definition of [let_binding_body].

   In order to avoid duplication between the definitions of [pattern] and
   [pattern_no_exn], we create a parameterized definition [pattern_(self)]
   and instantiate it twice. *)

pattern:
    pattern_(pattern)
      { $1 }
  | EXCEPTION ext_attributes pattern %prec prec_constr_appl
      { mkpat_attrs ~loc:$sloc (Ppat_exception $3) $2}
;

pattern_no_exn:
    pattern_(pattern_no_exn)
      { $1 }
;

%inline pattern_(self):
  | self COLONCOLON p = pattern
      { match p.ppat_desc, p.ppat_attributes with
        | Ppat_cons pl, [] -> Pat.cons ~loc:(make_loc $sloc) ($1 :: pl)
        | _ -> Pat.cons ~loc:(make_loc $sloc) [$1; p] }
  | self attribute
      { Pat.attr $1 $2 }
  | pattern_gen
      { $1 }
  | mkpat(
      self AS mkrhs(val_ident)
        { Ppat_alias($1, $3) }
    | self AS error
        { expecting $loc($3) "identifier" }
    | self COLONCOLON error
        { expecting $loc($3) "pattern" }
    | self BAR pattern
        { let rec or_ p =
            match p with
            | {ppat_desc= Ppat_or (x :: t); ppat_attributes= []; _} -> or_ x @ t
            | _ -> [p]
          in
          Ppat_or (or_ $1 @ or_ $3) }
    | self BAR error
        { expecting $loc($3) "pattern" }
  ) { $1 }
  | reversed_labeled_tuple_pattern(self)
      { let closed, pats = $1 in
        mkpat ~loc:$sloc (Ppat_tuple (List.rev pats, closed))
      }
;

(* Parsing labeled tuple patterns

   Here we play essentially the same game we did for expressions - see the
   comment beginning "Parsing labeled tuple expressions".

   One difference is that we would need to manually inline the definition of
   individual elements in two places: Once in the base case for lists 2 or more
   elements, and once in the special case for open patterns with just one
   element (e.g., [~x, ..]).  Rather than manually inlining
   [labeled_tuple_pat_element] twice, we simply define it twice: once with the
   [%prec] annotations needed for its occurrences in tail position, and once
   without them suitable for use in other locations.
*)
%inline labeled_tuple_pat_element(self):
  | self { None, $1 }
  | LABEL simple_pattern %prec COMMA
      { let label = mkrhs $1 $loc($1) in
        Some label, $2 }
  | TILDE label = LIDENT
      { let loc = $loc(label) in
        let lbl = ghrhs label loc in
        Some lbl, mkpatvar ~loc label }
  | TILDE LPAREN label = LIDENT COLON cty = core_type RPAREN %prec COMMA
      { let loc = $loc(label) in
        let pat = mkpatvar ~loc label in
        let lbl = ghrhs label loc in
        Some lbl, mkpat_with_modes ~loc ~modes:[] ~pat ~cty:(Some cty) }

%inline labeled_tuple_pat_element_noprec(self):
  | self { None, $1 }
  | LABEL simple_pattern
      { let label = mkrhs $1 $loc($1) in
        Some label, $2 }
  | TILDE label = LIDENT
      { let loc = $loc(label) in
        let lbl = ghrhs label loc in
        Some lbl, mkpatvar ~loc label }
  | TILDE LPAREN label = LIDENT COLON cty = core_type RPAREN
      { let loc = $loc(label) in
        let pat = mkpatvar ~loc label in
        let lbl = ghrhs label loc in
        Some lbl, mkpat_with_modes ~loc ~modes:[] ~pat ~cty:(Some cty) }

labeled_tuple_pat_element_list(self):
  | labeled_tuple_pat_element_list(self) COMMA labeled_tuple_pat_element(self)
      { $3 :: $1 }
  | labeled_tuple_pat_element_noprec(self) COMMA labeled_tuple_pat_element(self)
      { [ $3; $1 ] }
  | self COMMA error
      { expecting $loc($3) "pattern" }
;

reversed_labeled_tuple_pattern(self):
  | labeled_tuple_pat_element_list(self) %prec below_COMMA
      { Closed, $1 }
  | labeled_tuple_pat_element_list(self) COMMA DOTDOT
      { Open, $1 }
  | labeled_tuple_pat_element_noprec(self) COMMA DOTDOT
      { Open, [ $1 ] }

pattern_gen:
    simple_pattern
      { $1 }
  | mkpat(
      mkrhs(constr_longident) pattern %prec prec_constr_appl
        { Ppat_construct($1, Some ([], $2)) }
    | constr=mkrhs(constr_longident) LPAREN TYPE newtypes=lident_list RPAREN
        pat=simple_pattern
        { Ppat_construct(constr, Some (newtypes, pat)) }
    | name_tag pattern %prec prec_constr_appl
        { Ppat_variant($1, Some $2) }
    ) { $1 }
  | LAZY ext_attributes simple_pattern
      { mkpat_attrs ~loc:$sloc (Ppat_lazy $3) $2}
;
simple_pattern:
    mkpat(mkrhs(val_ident) %prec below_EQUAL
      { Ppat_var ($1) })
      { $1 }
  | simple_pattern_not_ident { $1 }
;

simple_pattern_not_ident:
  | LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | simple_delimited_pattern
      { $1 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack ($4, None)) $3 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) COLON package_type RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack ($4, Some $6)) $3 }
  | mkpat(simple_pattern_not_ident_)
      { $1 }
;
%inline simple_pattern_not_ident_:
  | UNDERSCORE
      { Ppat_any }
  | signed_constant
      { Ppat_constant $1 }
  | signed_constant DOTDOT signed_constant
      { Ppat_interval ($1, $3) }
  | mkrhs(constr_longident)
      { Ppat_construct($1, None) }
  | name_tag
      { Ppat_variant($1, None) }
  | hash mkrhs(type_longident)
      { Ppat_type ($2) }
  | mkrhs(mod_longident) DOT simple_delimited_pattern
      { Ppat_open($1, $3) }
  | mkrhs(mod_longident) DOT mkrhs(LBRACKET RBRACKET {Lident "[]"})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT mkrhs(LPAREN RPAREN {Lident "()"})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT LPAREN pattern RPAREN
      { Ppat_open ($1, $4) }
  | mod_longident DOT LPAREN pattern error
      { unclosed "(" $loc($3) ")" $loc($5)  }
  | mod_longident DOT LPAREN error
      { expecting $loc($4) "pattern" }
  | LPAREN pattern error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN pattern COLON error
      { expecting $loc($4) "type" }
  | LPAREN MODULE ext_attributes module_name COLON package_core_type
    error
      { unclosed "(" $loc($1) ")" $loc($7) }
  | extension
      { Ppat_extension $1 }
  | LPAREN sub_pat=pattern modes=at_mode_expr RPAREN
      { match modes with
        | [] ->
          (* This is possible when we are erasing jane syntax *)
          sub_pat.ppat_desc
        | modes -> Ppat_constraint(sub_pat, None, modes) }
  | LPAREN pattern COLON core_type modes=optional_atat_mode_expr RPAREN
      { Ppat_constraint($2, Some $4, modes) }
;

simple_delimited_pattern:
  mkpat(
      LBRACE record_pat_content RBRACE
      { let (fields, closed) = $2 in
        Ppat_record(fields, closed) }
    | HASHLBRACE record_pat_content RBRACE
      { let (fields, closed) = $2 in
        Ppat_record_unboxed_product(fields, closed) }
    | LBRACE record_pat_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
    | LBRACKET pattern_semi_list RBRACKET
      { Ppat_list $2 }
    | LBRACKET pattern_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
    | array_patterns(LBRACKETBAR, BARRBRACKET)
        { Generic_array.pattern
            "[|" "|]"
            (fun elts -> Ppat_array (Mutable Location.none, elts))
            $1 }
    | array_patterns(LBRACKETCOLON, COLONRBRACKET)
        { Generic_array.pattern
            "[:" ":]"
            (fun elts -> Ppat_array (Immutable, elts))
            $1 }
  | HASHLPAREN reversed_labeled_tuple_pattern(pattern) RPAREN
        { let (closed, fields) = $2 in
          Ppat_unboxed_tuple (List.rev fields, closed) }
  ) { $1 }

%inline pattern_semi_list:
  ps = separated_or_terminated_nonempty_list(SEMI, pattern)
    { ps }
;
(* A label-pattern list is a nonempty list of label-pattern pairs, optionally
   followed with an UNDERSCORE, separated-or-terminated with semicolons. *)
%inline record_pat_content:
  listx(SEMI, record_pat_field, UNDERSCORE)
    { let fields, closed = $1 in
      let closed =
        match closed with
        | None -> OClosed
        | Some { txt = (); loc } -> OOpen loc
      in
      fields, closed }
;
%inline record_pat_field:
  label = mkrhs(label_longident)
  octy = preceded(COLON, core_type)?
  opat = preceded(EQUAL, pattern)?
    { label, octy, opat }
;

/* Value descriptions */

value_description:
  VAL
  ext = ext
  attrs1 = attributes
  id = mkrhs(val_ident)
  COLON
  ty = possibly_poly(core_type)
  modalities = optional_atat_modalities_expr
  attrs2 = post_item_attributes
    { let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Val.mk id ty ~modalities ~attrs ~loc ~docs,
      ext }
;

/* Primitive declarations */

primitive_declaration:
  EXTERNAL
  ext = ext
  attrs1 = attributes
  id = mkrhs(val_ident)
  COLON
  ty = possibly_poly(core_type)
  modalities = optional_atat_modalities_expr
  EQUAL
  prim = mkrhs(raw_string)+
  attrs2 = post_item_attributes
    { let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Val.mk id ty ~prim ~attrs ~modalities ~loc ~docs,
      ext }
;

(* Type declarations and type substitutions. *)

(* Type declarations [type t = u] and type substitutions [type t := u] are very
   similar, so we view them as instances of [generic_type_declarations]. In the
   case of a type declaration, the use of [nonrec_flag] means that [NONREC] may
   be absent or present, whereas in the case of a type substitution, the use of
   [no_nonrec_flag] means that [NONREC] must be absent. The use of [type_kind]
   versus [type_subst_kind] means that in the first case, we expect an [EQUAL]
   sign, whereas in the second case, we expect [COLONEQUAL]. *)

%inline type_declarations:
  generic_type_declarations(nonrec_flag, type_kind)
    { $1 }
;

%inline type_subst_declarations:
  generic_type_declarations(no_nonrec_flag, type_subst_kind)
    { $1 }
;

(* A set of type declarations or substitutions begins with a
   [generic_type_declaration] and continues with a possibly empty list of
   [generic_and_type_declaration]s. *)

%inline generic_type_declarations(flag, kind):
  xlist(
    generic_type_declaration(flag, kind),
    generic_and_type_declaration(kind)
  )
  { $1 }
;

(* [generic_type_declaration] and [generic_and_type_declaration] look similar,
   but are in reality different enough that it is difficult to share anything
   between them. *)

generic_type_declaration(flag, kind):
  TYPE
  ext = ext
  attrs1 = attributes
  flag = flag
  params = type_parameters
  id = mkrhs(LIDENT)
  jkind_and_attr = jkind_attr_opt
  kind_priv_manifest = kind
  cstrs = constraints
  attrs2 = post_item_attributes
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let (jkind, attrs3) = jkind_and_attr in
      let docs = symbol_docs $sloc in
      let attrs = attrs1 @ attrs2 @ attrs3 in
      let loc = make_loc $sloc in
      (flag, ext),
      Type.mk id ~params ?jkind ~cstrs ~kind ~priv ?manifest ~attrs ~loc ~docs
    }
;
%inline generic_and_type_declaration(kind):
  AND
  attrs1 = attributes
  params = type_parameters
  id = mkrhs(LIDENT)
  jkind_and_attr = jkind_attr_opt
  kind_priv_manifest = kind
  cstrs = constraints
  attrs2 = post_item_attributes
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let (jkind, attrs3) = jkind_and_attr in
      let docs = symbol_docs $sloc in
      let attrs = attrs1 @ attrs2 @ attrs3 in
      let loc = make_loc $sloc in
      let text = symbol_text $symbolstartpos in
      Type.mk id ~params ?jkind ~cstrs ~kind ~priv ?manifest ~attrs ~loc ~docs ~text
    }
;
%inline constraints:
  llist(preceded(CONSTRAINT, constrain))
    { $1 }
;
(* Lots of %inline expansion are required for [nonempty_type_kind] to be
   LR(1). At the cost of some manual expansion, it would be possible to give a
   definition that leads to a smaller grammar (after expansion) and therefore
   a smaller automaton. *)
nonempty_type_kind:
  | priv = inline_private_flag
    ty = core_type
      { (Ptype_abstract, priv, Some ty) }
  | oty = type_synonym
    priv = inline_private_flag
    cs = constructor_declarations
      { (Ptype_variant cs, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    DOTDOT
      { (Ptype_open, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    LBRACE ls = label_declarations RBRACE
      { (Ptype_record ls, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    HASHLBRACE ls = label_declarations RBRACE
      { (Ptype_record_unboxed_product ls, priv, oty) }
;
%inline type_synonym:
  ioption(terminated(core_type, EQUAL))
    { $1 }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL nonempty_type_kind
      { $2 }
;
%inline type_subst_kind:
    COLONEQUAL nonempty_type_kind
      { $2 }
;
type_parameters:
    /* empty */
      { [] }
  | p = type_parameter
      { [p] }
  | LPAREN
    ps = separated_nonempty_llist(COMMA, parenthesized_type_parameter)
    RPAREN
      { ps }
;

jkind:
    mkrhs(jkind) MOD mode_expr
      { Mod ($1, $3) }
  | mkrhs(jkind) WITH core_type
      { With ($1, $3) }
  | mkrhs(ident)
      { let {txt; loc} = $1 in
        Abbreviation ({ txt; loc }) }
  | KIND_OF ty=core_type
      { Kind_of ty }
  | UNDERSCORE
      { Default }
  | reverse_product_jkind %prec below_AMPERSAND
      { Product (List.rev $1) }
  | LPAREN jkind RPAREN
      { $2 }
;

reverse_product_jkind :
  | jkind1 = mkrhs(jkind) AMPERSAND jkind2 = mkrhs(jkind) %prec prec_unboxed_product_kind
      { [jkind2; jkind1] }
  | jkinds = reverse_product_jkind
    AMPERSAND
    jkind = mkrhs(jkind) %prec prec_unboxed_product_kind
    { jkind :: jkinds }
;

jkind_annotation_gen: (* : jkind_annotation *)
  mkrhs(jkind) { $1 }
;

jkind_annotation: (* : jkind_annotation *)
  jkind_annotation_gen
    {
      if Erase_jane_syntax.should_erase () then None
      else Some $1
    }
;

jkind_attr_opt:
  /* empty */
    { None, [] }
  | COLON jkind_annotation_gen
    {
      if Erase_jane_syntax.should_erase ()
      then None, convert_jkind_to_legacy_attr $2
      else Some $2, []
    }
;

kind_abbreviation_decl:
  KIND_ABBREV abbrev=mkrhs(LIDENT) EQUAL jkind=jkind_annotation_gen {
    (abbrev, jkind)
  }
;

%inline type_param_with_jkind:
  name=mkrhs(tyvar_name_or_underscore)
  attrs=attributes
  COLON
  jkind=jkind_annotation
  { let descr = Ptyp_var (name, jkind) in
    mktyp ~loc:$sloc ~attrs descr }
;

parenthesized_type_parameter:
    type_parameter { $1 }
  | type_variance type_param_with_jkind
    { $2, $1 }
;

type_parameter:
    type_variance type_variable attributes
      { {$2 with ptyp_attributes = $3}, $1 }
;

%inline type_variable:
  mktyp(
    QUOTE tyvar_name = mkrhs(ident {Some $1})
      { Ptyp_var (tyvar_name, None) }
  | UNDERSCORE
      { Ptyp_any }
  ) { $1 }
;

%inline tyvar_name_or_underscore:
    QUOTE ident
      { Some $2 }
  | UNDERSCORE
      { None }
;

type_variance:
    /* empty */                             { [] }
  | PLUS                                    { [ mkvarinj "+" $sloc ] }
  | MINUS                                   { [ mkvarinj "-" $sloc ] }
  | BANG                                    { [ mkvarinj "!" $sloc ] }
  | PLUS BANG   { [ mkvarinj "+" $loc($1); mkvarinj "!" $loc($2) ] }
  | BANG PLUS   { [ mkvarinj "!" $loc($1); mkvarinj "+" $loc($2) ] }
  | MINUS BANG  { [ mkvarinj "-" $loc($1); mkvarinj "!" $loc($2) ] }
  | BANG MINUS  { [ mkvarinj "!" $loc($1); mkvarinj "-" $loc($2) ] }
  | INFIXOP2
      { if ($1 = "+!") || ($1 = "-!") then [ mkvarinj $1 $sloc ]
        else expecting $loc($1) "type_variance" }
  | PREFIXOP
      { if ($1 = "!+") || ($1 = "!-") then [ mkvarinj $1 $sloc ]
        else expecting $loc($1) "type_variance" }
;

(* A sequence of constructor declarations is either a single BAR, which
   means that the list is empty, or a nonempty BAR-separated list of
   declarations, with an optional leading BAR. *)
constructor_declarations:
  | BAR
      { [] }
  | cs = bar_llist(constructor_declaration)
      { cs }
;
(* A constructor declaration begins with an opening symbol, which can
   be either epsilon or BAR. Note that this opening symbol is included
   in the footprint $sloc. *)
(* Because [constructor_declaration] and [extension_constructor_declaration]
   are identical except for their semantic actions, we introduce the symbol
   [generic_constructor_declaration], whose semantic action is neutral -- it
   merely returns a tuple. *)
generic_constructor_declaration(opening):
  opening
  cid = mkrhs(constr_ident)
  vars_args_res = generalized_constructor_arguments
  attrs = attributes
    {
      let vars, args, res = vars_args_res in
      let info = symbol_info $endpos in
      let loc = make_loc $sloc in
      cid, vars, args, res, attrs, loc, info
    }
;
%inline constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, vars, args, res, attrs, loc, info = d in
      Type.constructor cid ~vars ~args ?res ~attrs ~loc ~info
    }
;
str_exception_declaration:
  sig_exception_declaration
    { $1 }
| EXCEPTION
  ext = ext
  attrs1 = attributes
  id = mkrhs(constr_ident)
  EQUAL
  lid = mkrhs(constr_longident)
  attrs2 = attributes
  attrs = post_item_attributes
  { let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Te.mk_exception ~attrs
      (Te.rebind id lid ~attrs:(attrs1 @ attrs2) ~loc ~docs)
    , ext }
;
sig_exception_declaration:
  EXCEPTION
  ext = ext
  attrs1 = attributes
  id = mkrhs(constr_ident)
  vars_args_res = generalized_constructor_arguments
  attrs2 = attributes
  attrs = post_item_attributes
    { let vars, args, res = vars_args_res in
      let loc = make_loc ($startpos, $endpos(attrs2)) in
      let docs = symbol_docs $sloc in
      Te.mk_exception ~attrs
        (Te.decl id ~vars ~args ?res ~attrs:(attrs1 @ attrs2) ~loc ~docs)
      , ext }
;
%inline let_exception_declaration:
    mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let vars, args, res = $2 in
        Te.decl $1 ~vars ~args ?res ~attrs:$3 ~loc:(make_loc $sloc) }
;
generalized_constructor_arguments:
    /*empty*/                     { ([],Pcstr_tuple [],None) }
  | OF constructor_arguments      { ([],$2,None) }
  | COLON constructor_arguments MINUSGREATER atomic_type %prec below_HASH
                                  { ([],$2,Some $4) }
  | COLON typevar_list DOT constructor_arguments MINUSGREATER atomic_type
     %prec below_HASH
                                  { ($2,$4,Some $6) }
  | COLON atomic_type %prec below_HASH
                                  { ([],Pcstr_tuple [],Some $2) }
  | COLON typevar_list DOT atomic_type %prec below_HASH
                                  { ($2,Pcstr_tuple [],Some $4) }
;

%inline constructor_argument:
  gbl=global_flag cty=atomic_type m1=optional_atat_modalities_expr {
    let cty = mkcty_global_maybe gbl cty in
    Type.constructor_arg cty ~modalities:m1 ~loc:(make_loc $sloc)
  }
;

constructor_arguments:
  | tys = inline_separated_nonempty_llist(STAR, constructor_argument)
      { Pcstr_tuple tys }
  | LBRACE label_declarations RBRACE
      { Pcstr_record (make_loc $sloc, $2) }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declaration_semi                      { [$1] }
  | label_declaration_semi label_declarations   { $1 :: $2 }
;
label_declaration:
    mutable_or_global_flag mkrhs(label) COLON poly_type_no_attr m1=optional_atat_modalities_expr attributes
      { let info = symbol_info $endpos in
        let mut, gbl = $1 in
        mkld_global_maybe gbl
          (Type.field $2 $4 ~mut ~modalities:m1 ~attrs:$6 ~loc:(make_loc $sloc) ~info) }
;
label_declaration_semi:
    mutable_or_global_flag mkrhs(label) COLON poly_type_no_attr m1=optional_atat_modalities_expr attributes
      SEMI attributes
      { let info =
          match rhs_info $endpos($6) with
          | Some _ as info_before_semi -> info_before_semi
          | None -> symbol_info $endpos
       in
       let mut, gbl = $1 in
       mkld_global_maybe gbl
         (Type.field $2 $4 ~mut ~modalities:m1 ~attrs:($6 @ $8) ~loc:(make_loc $sloc) ~info) }
;

/* Type Extensions */

%inline str_type_extension:
  type_extension(extension_constructor)
    { $1 }
;
%inline sig_type_extension:
  type_extension(extension_constructor_declaration)
    { $1 }
;
%inline type_extension(declaration):
  TYPE
  ext = ext
  attrs1 = attributes
  no_nonrec_flag
  params = type_parameters
  tid = mkrhs(type_longident)
  PLUSEQ
  priv = private_flag
  cs = bar_llist(declaration)
  attrs2 = post_item_attributes
    { let docs = symbol_docs $sloc in
      let attrs = attrs1 @ attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext }
;
%inline extension_constructor(opening):
    extension_constructor_declaration(opening)
      { $1 }
  | extension_constructor_rebind(opening)
      { $1 }
;
%inline extension_constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, vars, args, res, attrs, loc, info = d in
      Te.decl cid ~vars ~args ?res ~attrs ~loc ~info
    }
;
extension_constructor_rebind(opening):
  opening
  cid = mkrhs(constr_ident)
  EQUAL
  lid = mkrhs(constr_longident)
  attrs = attributes
      { let info = symbol_info $endpos in
        Te.rebind cid lid ~attrs ~loc:(make_loc $sloc) ~info }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
    TYPE type_parameters mkrhs(label_longident) with_type_binder
    core_type_no_attr constraints
      { let lident = loc_last $3 in
        Pwith_type
          ($3,
           (Type.mk lident
              ~params:$2
              ~cstrs:$6
              ~manifest:$5
              ~priv:$4
              ~loc:(make_loc $sloc))) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE type_parameters mkrhs(label_longident)
    COLONEQUAL core_type_no_attr
      { let lident = loc_last $3 in
        Pwith_typesubst
         ($3,
           (Type.mk lident
              ~params:$2
              ~manifest:$5
              ~loc:(make_loc $sloc))) }
  | MODULE mkrhs(mod_longident) EQUAL mkrhs(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE mkrhs(mod_longident) COLONEQUAL mkrhs(mod_ext_longident)
      { Pwith_modsubst ($2, $4) }
  | MODULE TYPE l=mkrhs(mty_longident) EQUAL rhs=module_type
      { Pwith_modtype (l, rhs) }
  | MODULE TYPE l=mkrhs(mty_longident) COLONEQUAL rhs=module_type
      { Pwith_modtypesubst (l, rhs) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private (make_loc $loc($2)) }
;

/* Polymorphic types */

%inline typevar: (* : string with_loc * jkind_annotation option *)
  QUOTE mkrhs(ident {Some $1})
    { $2, None }
  | LPAREN QUOTE mkrhs(ident {Some $1})  COLON jkind=jkind_annotation RPAREN
    { $3, jkind }
;
%inline typevar_list:
  (* : (string with_loc * jkind_annotation option) list *)
  nonempty_llist(typevar)
    { $1 }
;
%inline poly(X):
  typevar_list DOT X
    { Ptyp_poly($1, $3) }
;
possibly_poly(X):
  X
    { $1 }
| mktyp(poly(X))
    { $1 }
;
%inline poly_type:
  possibly_poly(core_type)
    { $1 }
;
%inline poly_type_no_attr:
  possibly_poly(core_type_no_attr)
    { $1 }
;

(* -------------------------------------------------------------------------- *)

(* Core language types. *)

(* A core type (core_type) is a core type without attributes (core_type_no_attr)
   followed with a list of attributes. *)
core_type:
    core_type_no_attr
      { $1 }
  | core_type attribute
      { Typ.attr $1 $2 }
;

(* A core type without attributes is currently defined as an alias type, but
   this could change in the future if new forms of types are introduced. From
   the outside, one should use core_type_no_attr. *)
%inline core_type_no_attr:
  alias_type
    { $1 }
;

(* Alias types include:
   - function types (see below);
   - proper alias types:                  'a -> int as 'a
 *)
alias_type:
    function_type
      { $1 }
  | mktyp(
      ty = alias_type AS QUOTE tyvar = mkrhs(ident {Some $1})
        { Ptyp_alias(ty, (tyvar, None)) })
    { $1 }
  | aliased_type = alias_type AS
      LPAREN
      name=mkrhs(tyvar_name_or_underscore)
      COLON
      jkind = jkind_annotation
      RPAREN
        { match jkind, name.txt with
        | None, None ->
          assert (Erase_jane_syntax.should_erase ());
          aliased_type
        | _ ->
          mktyp ~loc:$sloc (Ptyp_alias (aliased_type, (name, jkind))) }
;

(* Function types include:
   - tuple types (see below);
   - proper function types:               int -> int
                                          foo: int -> int
                                          ?foo: int -> int
 *)
function_type:
  | ty = tuple_type
    %prec MINUSGREATER
      { ty }
  | ty = strict_function_or_labeled_tuple_type
      { ty }
;
strict_function_or_labeled_tuple_type:
  | mktyp(
      label = arg_label
      local = mode_flags
      domain_with_modes = with_optional_mode_expr(extra_rhs(param_type))
      MINUSGREATER
      codomain = strict_function_or_labeled_tuple_type
        { let (domain, _), arg_modes = domain_with_modes in
          let type_ = mktyp_modes local domain in
          let label, type_ =
            if not (Erase_jane_syntax.should_erase ())
            then label, type_
            else (
              match label, type_.ptyp_desc with
              | Labelled l, Ptyp_extension ({ txt = "call_pos"; _ }, _) ->
                ( Optional l
                , Ast_helper.Typ.constr
                    { loc = make_loc $sloc
                    ; txt = Ldot (Ldot (Lident "Stdlib", "Lexing"), "position")
                    }
                    [] )
              | _ -> label, type_)
          in
          let arrow_type = {
            pap_label = label;
            pap_loc = make_loc $sloc;
            pap_type = type_;
            pap_modes = arg_modes
          }
          in
          let params, codomain, ret_modes =
            match codomain.ptyp_attributes, codomain.ptyp_desc with
            | [], Ptyp_arrow (params, codomain, ret_modes) -> params, codomain, ret_modes
            | _, _ -> [], codomain, []
          in
          Ptyp_arrow (arrow_type :: params, codomain, ret_modes)
        }
    )
    { $1 }
  | mktyp(
      label = arg_label
      arg_local = mode_flags
      domain_with_modes = with_optional_mode_expr(extra_rhs(param_type))
      MINUSGREATER
      ret_local = mode_flags
      codomain_with_modes = with_optional_mode_expr(tuple_type)
      %prec MINUSGREATER
         { let (domain, _), arg_modes = domain_with_modes in
           let (codomain, _), ret_modes = codomain_with_modes in
           let arrow_type = {
             pap_label = label;
             pap_loc = make_loc $sloc;
             pap_type = mktyp_modes arg_local domain;
             pap_modes = arg_modes
           }
           in
           let codomain =
             mktyp_modes ret_local (maybe_curry_typ codomain)
           in
           Ptyp_arrow([arrow_type], codomain, ret_modes)
         }
      )
      { $1 }
  (* These next three cases are for labled tuples - see comment on [tuple_type]
     below.

     The first two cases are present just to resolve a shift reduce conflict
     in a module type [S with t := x:t1 * t2 -> ...] which might be the
     beginning of
       [S with t := x:t1 * t2 -> S']    or    [S with t := x:t1 * t2 -> t3]
     They are the same as the previous two cases, but with [arg_label] replaced
     with the more specific [LIDENT COLON] and [param_type] replaced with the
     more specific [proper_tuple_type].  Apparently, this is sufficient for
     menhir to be able to delay a decision about which of the above module type
     cases we are in.  *)
  | mktyp(
      label = LIDENT COLON
      unique_local = mode_flags
      tuple_with_modes = with_optional_mode_expr(proper_tuple_type)
      MINUSGREATER
      codomain = strict_function_or_labeled_tuple_type
         {
           let (tuple, tuple_loc), arg_modes = tuple_with_modes in
           let ty, ltys = tuple in
           let label = mk_labelled label $loc(label) in
           let domain =
             mktyp ~loc:tuple_loc (Ptyp_tuple ((None, ty) :: ltys))
           in
           let domain = extra_rhs_core_type domain ~pos:(snd tuple_loc) in
           let arrow_type = {
             pap_label = label;
             pap_loc = make_loc $sloc;
             pap_type = mktyp_modes unique_local domain;
             pap_modes = arg_modes
           }
           in
           let params, codomain, ret_modes =
             match codomain.ptyp_attributes, codomain.ptyp_desc with
             | [], Ptyp_arrow (params, codomain, ret_modes) -> params, codomain, ret_modes
             | _, _ -> [], codomain, []
           in
           Ptyp_arrow(arrow_type :: params, codomain, ret_modes) }
    )
    { $1 }
  | mktyp(
      label = LIDENT COLON
      arg_unique_local = mode_flags
      tuple_with_modes = with_optional_mode_expr(proper_tuple_type)
      MINUSGREATER
      ret_unique_local = mode_flags
      codomain_with_modes = with_optional_mode_expr(tuple_type)
      %prec MINUSGREATER
         { let (tuple, tuple_loc), arg_modes = tuple_with_modes in
           let (codomain, _), ret_modes = codomain_with_modes in
           let ty, ltys = tuple in
           let label = mk_labelled label $loc(label) in
           let domain =
             mktyp ~loc:tuple_loc (Ptyp_tuple ((None, ty) :: ltys))
           in
           let domain = extra_rhs_core_type domain ~pos:(snd tuple_loc) in
           let arrow_type = {
             pap_label = label;
             pap_loc = make_loc $sloc;
             pap_type = mktyp_modes arg_unique_local domain;
             pap_modes = arg_modes
           }
           in
           Ptyp_arrow([arrow_type],
            mktyp_modes ret_unique_local (maybe_curry_typ codomain),
            ret_modes)
         }
    )
    { $1 }
  | label = LIDENT COLON proper_tuple_type %prec MINUSGREATER
    { let ty, ltys = $3 in
      let label = mkrhs label $loc(label) in
      mktyp ~loc:$sloc (Ptyp_tuple ((Some label, ty) :: ltys))
    }
;
%inline param_type:
  | mktyp(
    LPAREN vars = typevar_list DOT ty = core_type RPAREN
      { Ptyp_poly(vars, ty) }
    )
    { $1 }
  | ty = tuple_type
    { ty }
;
%inline arg_label:
  | label = optlabel
      { mk_optional label $sloc }
  | label = LIDENT COLON
      { mk_labelled label $sloc }
  | /* empty */
      { Nolabel }
;
%inline optional_local:
  | /* empty */
    { false }
  | LOCAL
    { true }
;
(* jane street: hackily copied and modified from our parser - to be replaced with the
   exact version from our parser when ocamlformat is updated for uniqueness. *)
%inline mode_flag:
   | LOCAL
       { $sloc }
;
%inline mode_flags:
   | flags = iloption(mode_flag+)
       { flags }
;

/* New mode annotation, introduced by AT or ATAT */
%inline mode:
  | LIDENT { mkloc (Mode $1) (make_loc $sloc) }
;

%inline mode_expr:
  | mode+ {
    if Erase_jane_syntax.should_erase ()
    then []
    else
      List.sort
        (fun
          { Location.txt = Mode m1; _ } { Location.txt = Mode m2; _ } ->
        String.compare m1 m2)
        $1
    }
;

at_mode_expr:
  | AT mode_expr {$2}
  | AT error { expecting $loc($2) "mode expression" }
;

%inline optional_at_mode_expr:
  | { [] }
  | at_mode_expr {$1}
;

%inline with_optional_mode_expr(ty):
  | ty=ty m=optional_at_mode_expr {
    (ty, $loc(ty)), m
  }
;

atat_mode_expr:
  | ATAT mode_expr {$2}
  | ATAT error { expecting $loc($2) "mode expression" }
;

%inline optional_atat_mode_expr:
  | { [] }
  | atat_mode_expr {$1}
;

/* Modalities */

%inline modality:
  | LIDENT { mkloc (Modality $1) (make_loc $sloc) }

%inline modalities:
  | modality+ {
    if Erase_jane_syntax.should_erase ()
    then []
    else
      List.sort
        (fun
          { Location.txt = Modality m1; _ } { Location.txt = Modality m2; _ } ->
        String.compare m1 m2)
        $1
    }

optional_atat_modalities_expr:
  | %prec below_HASH
    { [] }
  | ATAT modalities { $2 }
  | ATAT error { expecting $loc($2) "modality expression" }
;


(* Tuple types include:
   - atomic types (see below);
   - proper tuple types:                  int * int * int list
   A proper tuple type is a star-separated list of at least two atomic types.

   Tuple components can also be labeled, as an [int * int list * y:bool].

   However, the special case of labeled tuples where the first element has a
   label is not parsed as a proper_tuple_type, but rather as a case of
   strict_function_or_labled_tuple_type above.  This helps in dealing with
   ambiguities around [x:t1 * t2 -> t3] which must continue to parse as a
   function with one labeled argument even in the presense of labled tuples.
*)
tuple_type:
  | ty = atomic_type
    %prec below_HASH
      { ty }
  | proper_tuple_type %prec below_FUNCTOR
    { let ty, ltys = $1 in
      mktyp ~loc:$sloc (Ptyp_tuple ((None, ty) :: ltys))
    }
;

%inline proper_tuple_type:
  | ty = atomic_type
    STAR
    ltys = separated_nonempty_llist(STAR, labeled_tuple_typ_element)
      { ty, ltys }

(* In the case of an unboxed tuple, we don't need the nonsense above because
the [#( ... )] disambiguates.  However, we still must write out
the first element explicitly because [labeled_tuple_typ_element] is
restricted to tail position by its %prec annotation. *)
%inline unboxed_tuple_type_body:
  | ty1 = atomic_type
    STAR
    ltys = separated_nonempty_llist(STAR, labeled_tuple_typ_element)
    { (None, ty1) :: ltys }
  | label = LIDENT
    COLON
    ty1 = atomic_type
    STAR
    ltys = separated_nonempty_llist(STAR, labeled_tuple_typ_element)
    { let label = mkrhs label $loc(label) in
      (Some label, ty1) :: ltys }

%inline labeled_tuple_typ_element :
  | atomic_type %prec STAR
     { None, $1 }
  | label = LIDENT COLON ty = atomic_type %prec STAR
     { let label = mkrhs label $loc(label) in
       Some label, ty }

(* Atomic types are the most basic level in the syntax of types.
   Atomic types include:
   - types between parentheses:           (int -> int)
   - first-class module types:            (module S)
   - type variables:                      'a
   - applications of type constructors:   int, int list, int option list
   - variant types:                       [`A]
 *)
atomic_type:
  | LPAREN core_type RPAREN
      { $2 }
  | LPAREN MODULE ext_attributes package_core_type RPAREN
      { wrap_typ_attrs ~loc:$sloc (reloc_typ ~loc:$sloc $4) $3 }
  | mktyp( /* begin mktyp group */
      QUOTE mkrhs(ident {Some $1})
        { Ptyp_var ($2, None) }
    | UNDERSCORE
        { Ptyp_any }
    | tys = actual_type_parameters
      tid = mkrhs(type_longident)
      HASH_SUFFIX
        { Jane.ptyp_constr_unboxed tid tys }
    | tys = actual_type_parameters
      tid = mkrhs(type_longident)
        { Ptyp_constr(tid, tys) } %prec below_HASH
    | LESS meth_list GREATER
        { let (f, c) = $2 in Ptyp_object (f, c) }
    | LESS GREATER
        { Ptyp_object ([], OClosed) }
    | tys = actual_type_parameters
      HASH
      cid = mkrhs(clty_longident)
        { Ptyp_class(cid, tys) }
    | LBRACKET tag_field RBRACKET
        (* not row_field; see CONFLICTS *)
        { Ptyp_variant([$2], Closed, None) }
    | LBRACKET BAR row_field_list RBRACKET
        { Ptyp_variant($3, Closed, None) }
    | LBRACKET row_field BAR row_field_list RBRACKET
        { Ptyp_variant($2 :: $4, Closed, None) }
    | LBRACKETGREATER BAR? row_field_list RBRACKET
        { Ptyp_variant($3, Open, None) }
    | LBRACKETGREATER RBRACKET
        { Ptyp_variant([], Open, None) }
    | LBRACKETLESS BAR? row_field_list RBRACKET
        { Ptyp_variant($3, Closed, Some []) }
    | LBRACKETLESS BAR? row_field_list GREATER name_tag_list RBRACKET
        { Ptyp_variant($3, Closed, Some $5) }
    | HASHLPAREN unboxed_tuple_type_body RPAREN
        { Ptyp_unboxed_tuple $2 }
    | extension
        { Ptyp_extension $1 }
    | LPAREN QUOTE name=mkrhs(ident {Some $1}) COLON jkind=jkind_annotation RPAREN
      { Ptyp_var (name, jkind) }
    | LPAREN mkrhs(UNDERSCORE {None}) COLON jkind=jkind_annotation RPAREN
      { Ptyp_var ($2, jkind) }

  )
  { $1 } /* end mktyp group */
;

(* This is the syntax of the actual type parameters in an application of
   a type constructor, such as int, int list, or (int, bool) Hashtbl.t.
   We allow one of the following:
   - zero parameters;
   - one parameter:
     an atomic type;
     among other things, this can be an arbitrary type between parentheses;
   - two or more parameters:
     arbitrary types, between parentheses, separated with commas.
 *)
%inline actual_type_parameters:
  | /* empty */
      { [] }
  | ty = atomic_type
      { [ty] }
  | LPAREN
    tys = separated_nontrivial_llist(COMMA, one_type_parameter_of_several)
    RPAREN
      { tys }
;

(* Layout annotations on type expressions typically require parens, as in [('a :
   float64)].  But this is unnecessary when the type expression is used as the
   parameter of a tconstr with more than one argument, as in [(int, 'b :
   float64) t]. *)
%inline one_type_parameter_of_several:
  | core_type { $1 }
  | name=mkrhs(tyvar_name_or_underscore) COLON jkind=jkind_annotation
    { let descr = Ptyp_var (name, jkind) in
      mktyp ~loc:$sloc descr }

%inline package_core_type: module_type
      { let (lid, cstrs, attrs) = package_type_of_module_type $1 in
        let descr = Ptyp_package (lid, cstrs, []) in
        mktyp ~loc:$sloc ~attrs descr }
;
%inline package_type: module_type
      { package_type_of_module_type $1 }
;
%inline row_field_list:
  separated_nonempty_llist(BAR, row_field)
    { $1 }
;
row_field:
    tag_field
      { $1 }
  | core_type
      { Rf.inherit_ ~loc:(make_loc $sloc) $1 }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list attributes
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $5 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 $3 $4 }
  | name_tag attributes
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $2 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 true [] }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
%inline amper_type_list:
  separated_nonempty_llist(AMPERSAND, core_type_no_attr)
    { $1 }
;
%inline name_tag_list:
  nonempty_llist(name_tag)
    { $1 }
;
(* A method list (in an object type). *)
meth_list:
    head = field_semi         tail = meth_list
  | head = inherit_field SEMI tail = meth_list
      { let (f, c) = tail in (head :: f, c) }
  | head = field_semi
  | head = inherit_field SEMI
      { [head], OClosed }
  | head = field
  | head = inherit_field
      { [head], OClosed }
  | DOTDOT
      { [], OOpen (make_loc $sloc) }
;
%inline field:
  mkrhs(label) COLON poly_type_no_attr attributes
    { let info = symbol_info $endpos in
      let attrs = add_info_attrs info $4 in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

%inline field_semi:
  mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
    { let info =
        match rhs_info $endpos($4) with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info $endpos
      in
      let attrs = add_info_attrs info ($4 @ $6) in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

%inline inherit_field:
  ty = atomic_type
    { Of.inherit_ ~loc:(make_loc $sloc) ty }
;

%inline label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
  | INT          { let (n, m) = $1 in
                   mkconst ~loc:$sloc (Pconst_integer (n, m)) }
  | CHAR         { let (c, s) = $1 in
                   mkconst ~loc:$sloc (Pconst_char (c, s)) }
  | STRING       { let (s, strloc, d) = $1 in
                   mkconst ~loc:$sloc (Pconst_string (s,strloc,d)) }
  | FLOAT        { let (f, m) = $1 in
                   mkconst ~loc:$sloc (Pconst_float (f, m)) }

  (* Jane Street extension *)
  | HASH_INT     { let (n, m) = $1 in
                   mkconst ~loc:$sloc (Jane.pconst_unboxed_integer Positive n m) }
  | HASH_FLOAT   { let (f, m) = $1 in
                   mkconst ~loc:$sloc (Jane.pconst_unboxed_float Positive f m) }
  (* End Jane Street extension *)
;
signed_constant:
    constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in
                   mkconst ~loc:$sloc (Pconst_integer("-" ^ n, m)) }
  | MINUS FLOAT  { let (f, m) = $2 in
                   mkconst ~loc:$sloc (Pconst_float("-" ^ f, m)) }
  | PLUS INT     { let (n, m) = $2 in
                   mkconst ~loc:$sloc (Pconst_integer (n, m)) }
  | PLUS FLOAT   { let (f, m) = $2 in
                   mkconst ~loc:$sloc (Pconst_float(f, m)) }

  (* Jane Street extension *)
  | MINUS HASH_INT    { let (n, m) = $2 in
                        mkconst ~loc:$sloc
                          (Jane.pconst_unboxed_integer Negative n m) }
  | MINUS HASH_FLOAT  { let (f, m) = $2 in
                        mkconst ~loc:$sloc
                          (Jane.pconst_unboxed_float Negative f m) }
  | PLUS HASH_INT     { let (n, m) = $2 in
                        mkconst ~loc:$sloc
                          (Jane.pconst_unboxed_integer Positive n m) }
  | PLUS HASH_FLOAT   { let (f, m) = $2 in
                        mkconst ~loc:$sloc
                          (Jane.pconst_unboxed_float Positive f m) }
  (* End Jane Street extension *)
;

/* Identifiers and long identifiers */

ident:
    UIDENT                    { $1 }
  | LIDENT                    { $1 }
;
val_extra_ident:
  | LPAREN operator RPAREN    { $2 }
  | LPAREN operator error     { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN error              { expecting $loc($2) "operator" }
  | LPAREN MODULE error       { expecting $loc($3) "module-expr" }
;
val_ident:
    LIDENT                    { $1 }
  | val_extra_ident           { $1 }
;
operator:
    PREFIXOP                                    { $1 }
  | LETOP                                       { $1 }
  | ANDOP                                       { $1 }
  | DOTOP LPAREN index_mod RPAREN               { "."^ $1 ^"(" ^ $3 ^ ")" }
  | DOTOP LPAREN index_mod RPAREN LESSMINUS     { "."^ $1 ^ "(" ^ $3 ^ ")<-" }
  | DOTOP LBRACKET index_mod RBRACKET           { "."^ $1 ^"[" ^ $3 ^ "]" }
  | DOTOP LBRACKET index_mod RBRACKET LESSMINUS { "."^ $1 ^ "[" ^ $3 ^ "]<-" }
  | DOTOP LBRACE index_mod RBRACE               { "."^ $1 ^"{" ^ $3 ^ "}" }
  | DOTOP LBRACE index_mod RBRACE LESSMINUS     { "."^ $1 ^ "{" ^ $3 ^ "}<-" }
  | HASHOP                                      { $1 }
  | BANG                                        { "!" }
  | infix_operator                              { $1 }
;
%inline infixop3:
  | op = INFIXOP3 { op }
  | MOD           { "mod" }
;
%inline infix_operator:
  | op = INFIXOP0 { op }
  /* Still support the two symbols as infix operators */
  | AT             {"@"}
  | ATAT           {"@@"}
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | op = infixop3 { op }
  | op = INFIXOP4 { op }
  | PLUS           {"+"}
  | PLUSDOT       {"+."}
  | PLUSEQ        {"+="}
  | MINUS          {"-"}
  | MINUSDOT      {"-."}
  | SLASH          {"/"}
  | STAR           {"*"}
  | PERCENT        {"%"}
  | EQUAL          {"="}
  | LESS           {"<"}
  | GREATER        {">"}
  | OR            {"or"}
  | BARBAR        {"||"}
  | AMPERSAND      {"&"}
  | AMPERAMPER    {"&&"}
  | COLONEQUAL    {":="}
;
index_mod:
| { "" }
| SEMI DOTDOT { ";.." }
;

%inline constr_extra_ident:
  | LPAREN COLONCOLON RPAREN                    { "::" }
;
constr_extra_nonprefix_ident:
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
constr_ident:
    UIDENT                                      { $1 }
  | constr_extra_ident                          { $1 }
  | constr_extra_nonprefix_ident                { $1 }
;
constr_longident:
    mod_longident       %prec below_DOT  { $1 } /* A.B.x vs (A).B.x */
  | mod_longident DOT constr_extra_ident { Ldot($1,$3) }
  | constr_extra_ident                   { Lident $1 }
  | constr_extra_nonprefix_ident         { Lident $1 }
;
mk_longident(prefix,final):
   | final            { Lident $1 }
   | prefix DOT final { Ldot($1,$3) }
;
val_longident:
    mk_longident(mod_longident, val_ident) { $1 }
;
label_longident:
    mk_longident(mod_longident, LIDENT) { $1 }
;
type_longident:
    mk_longident(mod_ext_longident, LIDENT)  { $1 }
  (* Allow identifiers like [t/42]. *)
  | LIDENT SLASH TYPE_DISAMBIGUATOR          { Lident ($1 ^ "/" ^ $3) }
;
mod_longident:
    mk_longident(mod_longident, UIDENT)  { $1 }
;
mod_ext_longident_:
    UIDENT                          { Lident $1 }
  | UIDENT SLASH TYPE_DISAMBIGUATOR { Lident ($1 ^ "/" ^ $3) }
  | mod_ext_longident DOT UIDENT    { Ldot($1,$3) }
;
mod_ext_longident:
    mod_ext_longident_ { $1 }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN
      { lapply ~loc:$sloc $1 $3 }
  | mod_ext_longident LPAREN error
      { expecting $loc($3) "module path" }
;
mty_longident:
    mk_longident(mod_ext_longident,ident) { $1 }
;
clty_longident:
    mk_longident(mod_ext_longident,LIDENT) { $1 }
;
class_longident:
   mk_longident(mod_longident,LIDENT) { $1 }
;

/* BEGIN AVOID */
/* For compiler-libs: parse all valid longidents and a little more:
   final identifiers which are value specific are accepted even when
   the path prefix is only valid for types: (e.g. F(X).(::)) */
any_longident:
  | mk_longident (mod_ext_longident,
     ident | constr_extra_ident | val_extra_ident { $1 }
    ) { $1 }
  | constr_extra_nonprefix_ident { Lident $1 }
;
/* END AVOID */

/* Toplevel directives */

toplevel_directive:
  hash dir = mkrhs(ident)
  arg = ioption(mk_directive_arg(toplevel_directive_argument))
    { mk_directive ~loc:$sloc dir arg }
;

%inline toplevel_directive_argument:
  | STRING        { let (s, _, _) = $1 in Pdir_string s }
  | INT           { let (n, m) = $1 in Pdir_int (n ,m) }
  | val_longident { Pdir_ident $1 }
  | mod_longident { Pdir_ident $1 }
  | FALSE         { Pdir_bool false }
  | TRUE          { Pdir_bool true }
;

/* Miscellaneous */

(* The symbol epsilon can be used instead of an /* empty */ comment. *)
%inline epsilon:
  /* empty */
    { () }
;

%inline raw_string:
  s = STRING
    { let body, _, _ = s in body }
;

name_tag:
  BACKQUOTE mkrhs(ident)
    { mkloc $2 (make_loc $sloc) }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
%inline nonrec_flag:
    /* empty */                                 { Recursive }
  | NONREC                                      { Nonrecursive }
;
%inline no_nonrec_flag:
    /* empty */ { Recursive }
/* BEGIN AVOID */
  | NONREC      { not_expecting $loc "nonrec flag" }
/* END AVOID */
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
  inline_private_flag
    { $1 }
;
%inline inline_private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private (make_loc $sloc) }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable (make_loc $sloc) }
;
mutable_or_global_flag:
    /* empty */                                 { Immutable, Nothing }
  | MUTABLE                                     { Mutable (make_loc $sloc),
                                                  Nothing }
  | GLOBAL                                      { Immutable, Global }
;
%inline global_flag:
          { Nothing }
  | GLOBAL { Global }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual (make_loc $sloc) }
;
mutable_virtual_flags:
    /* empty */
      { mk_mv () }
  | MUTABLE
      { mk_mv ~mut:(make_loc $sloc) () }
  | VIRTUAL
      { mk_mv ~virt:(make_loc $sloc) () }
  | MUTABLE VIRTUAL
      { mk_mv ~mut:(make_loc $loc($1)) ~virt:(make_loc $loc($2)) () }
  | VIRTUAL MUTABLE
      { mk_mv ~virt:(make_loc $loc($1)) ~mut:(make_loc $loc($2)) () }
;
private_virtual_flags:
    /* empty */
      { mk_pv () }
  | PRIVATE
      { mk_pv ~priv:(make_loc $sloc) () }
  | VIRTUAL
      { mk_pv ~virt:(make_loc $sloc) () }
  | PRIVATE VIRTUAL
      { mk_pv ~priv:(make_loc $loc($1)) ~virt:(make_loc $loc($2)) () }
  | VIRTUAL PRIVATE
      { mk_pv ~virt:(make_loc $loc($1)) ~priv:(make_loc $loc($2)) () }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a MUTABLE keyword. *)
virtual_with_mutable_flag:
  | VIRTUAL
      { mk_mv ~virt:(make_loc $sloc) () }
  | MUTABLE VIRTUAL
      { mk_mv ~mut:(make_loc $loc($1)) ~virt:(make_loc $loc($2)) () }
  | VIRTUAL MUTABLE
      { mk_mv ~virt:(make_loc $loc($1)) ~mut:(make_loc $loc($2)) () }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a PRIVATE keyword. *)
virtual_with_private_flag:
  | VIRTUAL
      { mk_pv ~virt:(make_loc $sloc) () }
  | PRIVATE VIRTUAL
      { mk_pv ~priv:(make_loc $loc($1)) ~virt:(make_loc $loc($2)) () }
  | VIRTUAL PRIVATE
      { mk_pv ~virt:(make_loc $loc($1)) ~priv:(make_loc $loc($2)) () }
;
%inline no_override_flag:
    /* empty */                                 { Fresh }
;
%inline override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;
optlabel:
   | OPTLABEL                                   { $1 }
   | QUESTION LIDENT COLON                      { $2 }
;

/* Attributes and extensions */

single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | LOCAL { "local_" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | NONREC { "nonrec" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  mkloc(
      single_attr_id { $1 }
    | single_attr_id DOT attr_id { $1 ^ "." ^ $3.txt }
  ) { $1 }
;
attribute:
  LBRACKETAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
post_item_attribute:
  LBRACKETATAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
floating_attribute:
  LBRACKETATATAT attr_id payload RBRACKET
    { mark_symbol_docs $sloc;
      Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
%inline post_item_attributes:
  post_item_attribute*
    { $1 }
;
%inline attributes:
  attribute*
    { $1 }
;
ext:
  | /* empty */     { None }
  | PERCENT attr_id { Some $2 }
;
%inline no_ext:
  | /* empty */     { None }
/* BEGIN AVOID */
  | PERCENT attr_id { not_expecting $loc "extension" }
/* END AVOID */
;
%inline ext_attributes:
  ext attributes    { $1, $2 }
;
extension:
  | LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
  | QUOTED_STRING_EXPR
    { mk_quotedext ~loc:$sloc $1 }
;
item_extension:
  | LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
  | QUOTED_STRING_ITEM
    { mk_quotedext ~loc:$sloc $1 }
;
payload:
    structure { PStr $1 }
  | COLON signature { PSig $2 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, Some $4) }
;
%%
