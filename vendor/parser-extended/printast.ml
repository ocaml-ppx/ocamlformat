(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Format
open Lexing
open Location
open Parsetree

let fmt_position with_name f l =
  let fname = if with_name then l.pos_fname else "" in
  if l.pos_lnum = -1
  then fprintf f "%s[%d]" fname l.pos_cnum
  else fprintf f "%s[%d,%d+%d]" fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)

let curr_indent : int ref = ref 0

let line i f s (*...*) =
  curr_indent := i;
  fprintf f "%s" (String.make ((2*i) mod 72) ' ');
  fprintf f s (*...*)

type cmts =
  { before: Location.t -> string list option
  ; within: Location.t -> string list option
  ; after: Location.t -> string list option }

let cmts : cmts option ref = ref None

let fmt_cmts i f lbl = function
  | Some cmts ->
      let fmt_cmt f s =
        line i f "%s: (*%s*)" lbl s
      in
      fprintf f "\n";
      pp_print_list fmt_cmt f cmts
  | None -> ()

let fmt_location f loc =
  if not !Clflags.locations then ()
  else begin
    let p_2nd_name = loc.loc_start.pos_fname <> loc.loc_end.pos_fname in
    fprintf f "(%a..%a)" (fmt_position true) loc.loc_start
                         (fmt_position p_2nd_name) loc.loc_end;
    if loc.loc_ghost then fprintf f " ghost";
    match !cmts with
    | None -> ()
    | Some {before; within; after} -> (
        match before loc, within loc, after loc with
        | None, None, None -> ()
        | b, w, a ->
            let i = !curr_indent in
            fprintf f "\n";
            line i f "comments";
            let i = i+1 in
            fmt_cmts i f "before" b;
            fmt_cmts i f "within" w;
            fmt_cmts i f " after" a )
  end

let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident (s) -> fprintf f "%s" s
  | Longident.Ldot (y, s) -> fprintf f "%a.%s" fmt_longident_aux y s
  | Longident.Lapply (y, z) ->
      fprintf f "%a(%a)" fmt_longident_aux y fmt_longident_aux z

let fmt_longident f x = fprintf f "\"%a\"" fmt_longident_aux x

let fmt_longident_loc f (x : Longident.t loc) =
  fprintf f "\"%a\" %a" fmt_longident_aux x.txt fmt_location x.loc

let fmt_string_loc f (x : string loc) =
  fprintf f "\"%s\" %a" x.txt fmt_location x.loc

let fmt_str_opt_loc f (x : string option loc) =
  fprintf f "\"%s\" %a" (Option.value x.txt ~default:"_") fmt_location x.loc

let fmt_char_option f = function
  | None -> fprintf f "None"
  | Some c -> fprintf f "Some %c" c

let fmt_mutable_flag f x =
  match x with
  | Immutable -> fprintf f "Immutable"
  | Mutable loc -> fprintf f "Mutable %a" fmt_location loc

let fmt_virtual_flag f x =
  match x with
  | Virtual loc -> fprintf f "Virtual %a" fmt_location loc
  | Concrete -> fprintf f "Concrete"

let fmt_override_flag f x =
  match x with
  | Override -> fprintf f "Override"
  | Fresh -> fprintf f "Fresh"

let fmt_closed_flag f x =
  match x with
  | Closed -> fprintf f "Closed"
  | Open -> fprintf f "Open"

let fmt_obj_closed_flag f x =
  match x with
  | OClosed -> fprintf f "OClosed"
  | OOpen loc -> fprintf f "OOpen %a" fmt_location loc

let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> fprintf f "Nonrec"
  | Recursive -> fprintf f "Rec"

let fmt_direction_flag f x =
  match x with
  | Upto -> fprintf f "Up"
  | Downto -> fprintf f "Down"

let fmt_private_flag f x =
  match x with
  | Public -> fprintf f "Public"
  | Private loc -> fprintf f "Private %a" fmt_location loc

let fmt_opt f ppf = function
  | None -> fprintf ppf "None"
  | Some x -> fprintf ppf "Some(%a)" f x

let fmt_private_virtual_flag ppf { pv_priv; pv_virt } =
  fprintf ppf "(private=%a, virtual=%a)"
    (fmt_opt fmt_location) pv_priv
    (fmt_opt fmt_location) pv_virt

let fmt_mutable_virtual_flag ppf { mv_mut; mv_virt } =
  fprintf ppf "(mutable=%a, virtual=%a)"
    (fmt_opt fmt_location) mv_mut
    (fmt_opt fmt_location) mv_virt

let fmt_constant i f x =
  line i f "constant %a\n" fmt_location x.pconst_loc;
  let i = i+1 in
  match x.pconst_desc with
  | Pconst_integer (j,m) -> line i f "PConst_int (%s,%a)\n" j fmt_char_option m
  | Pconst_char (c, s) -> line i f "PConst_char (%02x,%s)\n" (Char.code c) s
  | Pconst_string (s, strloc, None) ->
      line i f "PConst_string(%S,%a,None)\n" s fmt_location strloc
  | Pconst_string (s, strloc, Some delim) ->
      line i f "PConst_string (%S,%a,Some %S)\n" s fmt_location strloc delim
  | Pconst_float (s,m) -> line i f "PConst_float (%s,%a)\n" s fmt_char_option m

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n"
  | _ :: _ ->
     line i ppf "[\n";
     List.iter (f (i+1) ppf) l;
     line i ppf "]\n"

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n"
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x

let longident_loc i ppf li = line i ppf "%a\n" fmt_longident_loc li
let string i ppf s = line i ppf "\"%s\"\n" s
let string_loc i ppf s = line i ppf "%a\n" fmt_string_loc s
let str_opt_loc i ppf s = line i ppf "%a\n" fmt_str_opt_loc s
let arg_label i ppf = function
  | Nolabel -> line i ppf "Nolabel\n"
  | Optional s -> line i ppf "Optional %a\n" fmt_string_loc s
  | Labelled s -> line i ppf "Labelled %a\n" fmt_string_loc s

let paren_kind i ppf = function
  | Paren -> line i ppf "Paren\n"
  | Brace -> line i ppf "Brace\n"
  | Bracket -> line i ppf "Bracket\n"

let typevars ppf vs =
  List.iter (fun x ->
      fprintf ppf " %a %a" Pprintast.tyvar x.txt fmt_location x.loc) vs

let variant_var i ppf (x : variant_var) =
  line i ppf "variant_var %a\n" fmt_location x.loc;
  string_loc (i+1) ppf x.txt

let rec core_type i ppf x =
  line i ppf "core_type %a\n" fmt_location x.ptyp_loc;
  attributes i ppf x.ptyp_attributes;
  let i = i+1 in
  match x.ptyp_desc with
  | Ptyp_any -> line i ppf "Ptyp_any\n";
  | Ptyp_var (s) -> line i ppf "Ptyp_var %s\n" s;
  | Ptyp_arrow (params, ct2) ->
      line i ppf "Ptyp_arrow\n";
      list i arrow_param ppf params;
      core_type i ppf ct2;
  | Ptyp_tuple l ->
      line i ppf "Ptyp_tuple\n";
      list i core_type ppf l;
  | Ptyp_constr (li, l) ->
      line i ppf "Ptyp_constr %a\n" fmt_longident_loc li;
      list i core_type ppf l;
  | Ptyp_variant (l, closed, low) ->
      line i ppf "Ptyp_variant closed=%a\n" fmt_closed_flag closed;
      list i row_field ppf l;
      option i (fun i -> list i variant_var) ppf low
  | Ptyp_object (l, c) ->
      line i ppf "Ptyp_object %a\n" fmt_obj_closed_flag c;
      list i object_field ppf l
  | Ptyp_class (li, l) ->
      line i ppf "Ptyp_class %a\n" fmt_longident_loc li;
      list i core_type ppf l
  | Ptyp_alias (ct, s) ->
      line i ppf "Ptyp_alias \"%a\"\n" fmt_string_loc s;
      core_type i ppf ct;
  | Ptyp_poly (sl, ct) ->
      line i ppf "Ptyp_poly%a\n" typevars sl;
      core_type i ppf ct;
  | Ptyp_package pt ->
      line i ppf "Ptyp_package\n";
      package_type i ppf pt
  | Ptyp_open (mod_ident, t) ->
      line i ppf "Ptyp_open \"%a\"\n" fmt_longident_loc mod_ident;
      core_type i ppf t
  | Ptyp_extension (s, arg) ->
      line i ppf "Ptyp_extension %a\n" fmt_string_loc s;
      payload i ppf arg

and arrow_param i ppf {pap_label; pap_loc; pap_type} =
  line i ppf "arrow_param %a\n" fmt_location pap_loc;
  arg_label i ppf pap_label;
  core_type i ppf pap_type

and object_field i ppf x =
  line i ppf "object_field %a\n" fmt_location x.pof_loc;
  attributes i ppf x.pof_attributes;
  let i = i+1 in
  match x.pof_desc with
  | Otag (l, t) ->
      line i ppf "Otag %a\n" fmt_string_loc l;
      core_type i ppf t
  | Oinherit ct ->
      line i ppf "Oinherit\n";
      core_type i ppf ct

and package_with i ppf (s, t) =
  line i ppf "with type %a\n" fmt_longident_loc s;
  core_type i ppf t

and package_type i ppf (s, l, attrs) =
  line i ppf "package_type %a\n" fmt_longident_loc s;
  attributes (i+1) ppf attrs;
  list i package_with ppf l

and pattern i ppf x =
  line i ppf "pattern %a\n" fmt_location x.ppat_loc;
  attributes i ppf x.ppat_attributes;
  let i = i+1 in
  match x.ppat_desc with
  | Ppat_any -> line i ppf "Ppat_any\n";
  | Ppat_var (s) -> line i ppf "Ppat_var %a\n" fmt_string_loc s;
  | Ppat_alias (p, s) ->
      line i ppf "Ppat_alias %a\n" fmt_string_loc s;
      pattern i ppf p;
  | Ppat_constant (c) ->
      line i ppf "Ppat_constant\n";
      fmt_constant i ppf c;
  | Ppat_interval (c1, c2) ->
      line i ppf "Ppat_interval\n";
      fmt_constant i ppf c1;
      fmt_constant i ppf c2;
  | Ppat_tuple (l) ->
      line i ppf "Ppat_tuple\n";
      list i pattern ppf l;
  | Ppat_construct (li, po) ->
      line i ppf "Ppat_construct %a\n" fmt_longident_loc li;
      option i
        (fun i ppf (vl, p) ->
          list i string_loc ppf vl;
          pattern i ppf p)
        ppf po
  | Ppat_variant (l, po) ->
      line i ppf "Ppat_variant\n";
      variant_var i ppf l;
      option i pattern ppf po;
  | Ppat_record (l, c) ->
      line i ppf "Ppat_record %a\n" fmt_obj_closed_flag c;
      list i longident_x_pattern ppf l;
  | Ppat_array (l) ->
      line i ppf "Ppat_array\n";
      list i pattern ppf l;
  | Ppat_list (l) ->
      line i ppf "Ppat_list\n";
      list i pattern ppf l;
  | Ppat_or l ->
      line i ppf "Ppat_or\n";
      list i pattern ppf l
  | Ppat_lazy p ->
      line i ppf "Ppat_lazy\n";
      pattern i ppf p;
  | Ppat_constraint (p, ct) ->
      line i ppf "Ppat_constraint\n";
      pattern i ppf p;
      core_type i ppf ct;
  | Ppat_type (li) ->
      line i ppf "Ppat_type\n";
      longident_loc i ppf li
  | Ppat_unpack (s, pt) ->
      line i ppf "Ppat_unpack %a\n" fmt_str_opt_loc s;
      option i package_type ppf pt
  | Ppat_exception p ->
      line i ppf "Ppat_exception\n";
      pattern i ppf p
  | Ppat_effect(p1, p2) ->
      line i ppf "Ppat_effect\n";
      pattern i ppf p1;
      pattern i ppf p2
  | Ppat_open (m,p) ->
      line i ppf "Ppat_open \"%a\"\n" fmt_longident_loc m;
      pattern i ppf p
  | Ppat_extension (s, arg) ->
      line i ppf "Ppat_extension %a\n" fmt_string_loc s;
      payload i ppf arg
  | Ppat_cons l ->
      line i ppf "Ppat_cons\n";
      list i pattern ppf l

and expression i ppf x =
  line i ppf "expression %a\n" fmt_location x.pexp_loc;
  attributes i ppf x.pexp_attributes;
  let i = i+1 in
  match x.pexp_desc with
  | Pexp_ident (li) -> line i ppf "Pexp_ident %a\n" fmt_longident_loc li;
  | Pexp_constant (c) ->
      line i ppf "Pexp_constant\n";
      fmt_constant i ppf c;
  | Pexp_let (l, e, loc_in) ->
      line i ppf "Pexp_let %a\n" fmt_rec_flag l.pvbs_rec;
      line (i + 1) ppf "loc_in: %a\n" fmt_location loc_in;
      value_bindings i ppf l;
      expression i ppf e;
  | Pexp_function (params, c, body) ->
      line i ppf "Pexp_function\n";
      list i expr_function_param ppf params;
      option i type_constraint ppf c;
      function_body i ppf body
  | Pexp_apply (e, l) ->
      line i ppf "Pexp_apply\n";
      expression i ppf e;
      list i label_x_expression ppf l;
  | Pexp_match (e, l) ->
      line i ppf "Pexp_match\n";
      expression i ppf e;
      list i case ppf l;
  | Pexp_try (e, l) ->
      line i ppf "Pexp_try\n";
      expression i ppf e;
      list i case ppf l;
  | Pexp_tuple (l) ->
      line i ppf "Pexp_tuple\n";
      list i expression ppf l;
  | Pexp_construct (li, eo) ->
      line i ppf "Pexp_construct %a\n" fmt_longident_loc li;
      option i expression ppf eo;
  | Pexp_variant (l, eo) ->
      line i ppf "Pexp_variant\n";
      variant_var i ppf l;
      option i expression ppf eo;
  | Pexp_record (l, eo) ->
      line i ppf "Pexp_record\n";
      list i longident_x_expression ppf l;
      option i expression ppf eo;
  | Pexp_field (e, li) ->
      line i ppf "Pexp_field\n";
      expression i ppf e;
      longident_loc i ppf li;
  | Pexp_setfield (e1, li, e2) ->
      line i ppf "Pexp_setfield\n";
      expression i ppf e1;
      longident_loc i ppf li;
      expression i ppf e2;
  | Pexp_array (l) ->
      line i ppf "Pexp_array\n";
      list i expression ppf l;
  | Pexp_list (l) ->
      line i ppf "Pexp_list\n";
      list i expression ppf l;
  | Pexp_ifthenelse (eN, eo) ->
      let pp_else i ppf (exp, loc_else) =
        line i ppf "else %a\n" fmt_location loc_else;
        expression (i+1) ppf exp
      in
      line i ppf "Pexp_ifthenelse\n";
      list i if_branch ppf eN;
      option i pp_else ppf eo;
  | Pexp_sequence (e1, e2) ->
      line i ppf "Pexp_sequence\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexp_while (e1, e2) ->
      line i ppf "Pexp_while\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexp_for (p, e1, e2, df, e3) ->
      line i ppf "Pexp_for %a\n" fmt_direction_flag df;
      pattern i ppf p;
      expression i ppf e1;
      expression i ppf e2;
      expression i ppf e3;
  | Pexp_constraint (e, ct) ->
      line i ppf "Pexp_constraint\n";
      expression i ppf e;
      core_type i ppf ct;
  | Pexp_coerce (e, cto1, cto2) ->
      line i ppf "Pexp_coerce\n";
      expression i ppf e;
      option i core_type ppf cto1;
      core_type i ppf cto2;
  | Pexp_send (e, s) ->
      line i ppf "Pexp_send %a\n" fmt_string_loc s;
      expression i ppf e;
  | Pexp_new (li) -> line i ppf "Pexp_new %a\n" fmt_longident_loc li;
  | Pexp_setinstvar (s, e) ->
      line i ppf "Pexp_setinstvar %a\n" fmt_string_loc s;
      expression i ppf e;
  | Pexp_override (l) ->
      line i ppf "Pexp_override\n";
      list i string_x_expression ppf l;
  | Pexp_letmodule (s, args, me, e) ->
      line i ppf "Pexp_letmodule %a\n" fmt_str_opt_loc s;
      list i functor_parameter ppf args;
      module_expr i ppf me;
      expression i ppf e;
  | Pexp_letexception (cd, e) ->
      line i ppf "Pexp_letexception\n";
      extension_constructor i ppf cd;
      expression i ppf e;
  | Pexp_assert (e) ->
      line i ppf "Pexp_assert\n";
      expression i ppf e;
  | Pexp_lazy (e) ->
      line i ppf "Pexp_lazy\n";
      expression i ppf e;
  | Pexp_object s ->
      line i ppf "Pexp_object\n";
      class_structure i ppf s
  | Pexp_pack (me, pt) ->
      line i ppf "Pexp_pack\n";
      module_expr i ppf me;
      option i package_type ppf pt
  | Pexp_open (lid, e) ->
      line i ppf "Pexp_open\n";
      longident_loc i ppf lid;
      expression i ppf e
  | Pexp_letopen (o, e) ->
      line i ppf "Pexp_letopen\n";
      open_declaration i ppf o;
      expression i ppf e
  | Pexp_letop {let_; ands; body} ->
      line i ppf "Pexp_letop\n";
      binding_op i ppf let_;
      list i binding_op ppf ands;
      expression i ppf body
  | Pexp_extension (s, arg) ->
      line i ppf "Pexp_extension %a\n" fmt_string_loc s;
      payload i ppf arg
  | Pexp_unreachable ->
      line i ppf "Pexp_unreachable\n"
  | Pexp_hole ->
      line i ppf "Pexp_hole\n"
  | Pexp_beginend e ->
      line i ppf "Pexp_beginend\n";
      expression i ppf e
  | Pexp_parens e ->
      line i ppf "Pexp_parens\n";
      expression i ppf e
  | Pexp_cons l ->
      line i ppf "Pexp_cons\n";
      list i expression ppf l
  | Pexp_indexop_access {pia_lhs; pia_kind; pia_paren; pia_rhs} ->
      line i ppf "Pexp_index_access\n";
      expression i ppf pia_lhs;
      begin
        match pia_kind with
        | Builtin idx ->
            expression i ppf idx
        | Dotop (path, op, idx) ->
            option i longident_loc ppf path;
            string i ppf op;
            list i expression ppf idx
      end;
      paren_kind i ppf pia_paren;
      option i expression ppf pia_rhs
  | Pexp_prefix (op, e) ->
      line i ppf "Pexp_prefix %a\n" fmt_string_loc op;
      expression i ppf e
  | Pexp_infix (op, e1, e2) ->
      line i ppf "Pexp_infix %a\n" fmt_string_loc op;
      expression i ppf e1;
      expression i ppf e2

and if_branch i ppf { if_cond; if_body; if_loc_then } =
  line i ppf "if_branch\n";
  expression i ppf if_cond;
  line i ppf "then %a\n" fmt_location if_loc_then;
  expression i ppf if_body

and pparam_val i ppf ~loc (l, eo, p) =
  line i ppf "Pparam_val %a\n" fmt_location loc;
  arg_label (i+1) ppf l;
  option (i+1) expression ppf eo;
  pattern (i+1) ppf p

and expr_function_param i ppf { pparam_desc = desc; pparam_loc = loc } =
  match desc with
  | Pparam_val p -> pparam_val i ppf ~loc p
  | Pparam_newtype tys ->
      List.iter (fun ty ->
        line i ppf "Pparam_newtype \"%s\" %a\n" ty.txt fmt_location loc)
        tys

and class_function_param i ppf { pparam_desc = desc; pparam_loc = loc } =
  pparam_val i ppf ~loc desc

and function_body i ppf body =
  match body with
  | Pfunction_body e ->
      line i ppf "Pfunction_body\n";
      expression (i+1) ppf e
  | Pfunction_cases (cases, loc, attrs) ->
      line i ppf "Pfunction_cases %a\n" fmt_location loc;
      attributes (i+1) ppf attrs;
      list (i+1) case ppf cases

and type_constraint i ppf constraint_ =
  match constraint_ with
  | Pconstraint ty ->
      line i ppf "Pconstraint\n";
      core_type (i+1) ppf ty
  | Pcoerce (ty1, ty2) ->
      line i ppf "Pcoerce\n";
      option (i+1) core_type ppf ty1;
      core_type (i+1) ppf ty2

and value_description i ppf x =
  line i ppf "value_description %a %a\n" fmt_string_loc
       x.pval_name fmt_location x.pval_loc;
  ext_attrs i ppf x.pval_attributes;
  core_type (i+1) ppf x.pval_type;
  list (i+1) string_loc ppf x.pval_prim

and type_parameter i ppf (x, _variance) = core_type i ppf x

and type_declaration i ppf x =
  line i ppf "type_declaration %a %a\n" fmt_string_loc x.ptype_name
       fmt_location x.ptype_loc;
  ext_attrs i ppf x.ptype_attributes;
  let i = i+1 in
  line i ppf "ptype_params =\n";
  list (i+1) type_parameter ppf x.ptype_params;
  line i ppf "ptype_cstrs =\n";
  list (i+1) core_type_x_core_type_x_location ppf x.ptype_cstrs;
  line i ppf "ptype_kind =\n";
  type_kind (i+1) ppf x.ptype_kind;
  line i ppf "ptype_private = %a\n" fmt_private_flag x.ptype_private;
  line i ppf "ptype_manifest =\n";
  option (i+1) core_type ppf x.ptype_manifest

and attribute i ppf k a =
  line i ppf "%s %a %a\n" k fmt_string_loc a.attr_name fmt_location a.attr_loc;
  payload i ppf a.attr_payload;

and attributes i ppf l =
  let i = i + 1 in
  List.iter (fun a ->
    line i ppf "attribute %a %a\n" fmt_string_loc a.attr_name
      fmt_location a.attr_loc;
    payload (i + 1) ppf a.attr_payload;
  ) l

and ext_attrs i ppf attrs =
  let i = i + 1 in
  option (i + 1)
    (fun i ppf ext ->  line i ppf "extension %a\n" fmt_string_loc ext)
    ppf attrs.attrs_extension;
  attributes i ppf attrs.attrs_before;
  attributes i ppf attrs.attrs_after

and payload i ppf = function
  | PStr x -> structure i ppf x
  | PSig x -> signature i ppf x
  | PTyp x -> core_type i ppf x
  | PPat (x, None) -> pattern i ppf x
  | PPat (x, Some g) ->
    pattern i ppf x;
    line i ppf "<when>\n";
    expression (i + 1) ppf g


and type_kind i ppf x =
  match x with
  | Ptype_abstract ->
      line i ppf "Ptype_abstract\n"
  | Ptype_variant l ->
      line i ppf "Ptype_variant\n";
      list (i+1) constructor_decl ppf l;
  | Ptype_record l ->
      line i ppf "Ptype_record\n";
      list (i+1) label_decl ppf l;
  | Ptype_open ->
      line i ppf "Ptype_open\n";

and type_extension i ppf x =
  line i ppf "type_extension %a\n" fmt_location x.ptyext_loc;
  ext_attrs i ppf x.ptyext_attributes;
  let i = i+1 in
  line i ppf "ptyext_path = %a\n" fmt_longident_loc x.ptyext_path;
  line i ppf "ptyext_params =\n";
  list (i+1) type_parameter ppf x.ptyext_params;
  line i ppf "ptyext_constructors =\n";
  list (i+1) extension_constructor ppf x.ptyext_constructors;
  line i ppf "ptyext_private = %a\n" fmt_private_flag x.ptyext_private;

and type_exception i ppf x =
  line i ppf "type_exception %a\n" fmt_location x.ptyexn_loc;
  ext_attrs i ppf x.ptyexn_attributes;
  let i = i+1 in
  line i ppf "ptyext_constructor =\n";
  let i = i+1 in
  extension_constructor i ppf x.ptyexn_constructor

and extension_constructor i ppf x =
  line i ppf "extension_constructor %a\n" fmt_location x.pext_loc;
  attributes i ppf x.pext_attributes;
  let i = i + 1 in
  line i ppf "pext_name = %a\n" fmt_string_loc x.pext_name;
  line i ppf "pext_kind =\n";
  extension_constructor_kind (i + 1) ppf x.pext_kind;

and extension_constructor_kind i ppf x =
  match x with
      Pext_decl(v, a, r) ->
        line i ppf "Pext_decl\n";
        if v <> [] then line (i+1) ppf "vars%a\n" typevars v;
        constructor_arguments (i+1) ppf a;
        option (i+1) core_type ppf r;
    | Pext_rebind li ->
        line i ppf "Pext_rebind\n";
        line (i+1) ppf "%a\n" fmt_longident_loc li;

and class_type i ppf x =
  line i ppf "class_type %a\n" fmt_location x.pcty_loc;
  attributes i ppf x.pcty_attributes;
  let i = i+1 in
  match x.pcty_desc with
  | Pcty_constr (li, l) ->
      line i ppf "Pcty_constr %a\n" fmt_longident_loc li;
      list i core_type ppf l;
  | Pcty_signature (cs) ->
      line i ppf "Pcty_signature\n";
      class_signature i ppf cs;
  | Pcty_arrow (params, cl) ->
      line i ppf "Pcty_arrow\n";
      list i arrow_param ppf params;
      class_type i ppf cl;
  | Pcty_extension (s, arg) ->
      line i ppf "Pcty_extension %a\n" fmt_string_loc s;
      payload i ppf arg
  | Pcty_open (o, e) ->
      line i ppf "Pcty_open\n";
      open_description i ppf o;
      class_type i ppf e

and class_signature i ppf cs =
  line i ppf "class_signature\n";
  option (i+1) core_type ppf cs.pcsig_self;
  list (i+1) class_type_field ppf cs.pcsig_fields;

and class_type_field i ppf x =
  line i ppf "class_type_field %a\n" fmt_location x.pctf_loc;
  let i = i+1 in
  attributes i ppf x.pctf_attributes;
  match x.pctf_desc with
  | Pctf_inherit (ct) ->
      line i ppf "Pctf_inherit\n";
      class_type i ppf ct;
  | Pctf_val (s, mv, ct) ->
      line i ppf "Pctf_val %a %a\n" fmt_string_loc s
        fmt_mutable_virtual_flag mv;
      core_type (i+1) ppf ct;
  | Pctf_method (s, pv, ct) ->
      line i ppf "Pctf_method %a %a\n" fmt_string_loc s
        fmt_private_virtual_flag pv;
      core_type (i+1) ppf ct;
  | Pctf_constraint (ct1, ct2) ->
      line i ppf "Pctf_constraint\n";
      core_type (i+1) ppf ct1;
      core_type (i+1) ppf ct2;
  | Pctf_attribute a ->
      attribute i ppf "Pctf_attribute" a
  | Pctf_extension (s, arg) ->
      line i ppf "Pctf_extension %a\n" fmt_string_loc s;
     payload i ppf arg

and class_infos : 'a. _ -> (_ -> _ -> 'a -> _) -> _ -> _ -> 'a class_infos -> _ =
 fun label f i ppf x ->
  line i ppf "%s %a\n" label fmt_location x.pci_loc;
  ext_attrs i ppf x.pci_attributes;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i ppf "pci_params =\n";
  list (i+1) type_parameter ppf x.pci_params;
  line i ppf "pci_name = %a\n" fmt_string_loc x.pci_name;
  line i ppf "pci_args =\n";
  list (i+1) class_function_param ppf x.pci_args;
  line i ppf "pci_constraint = %a\n" (fmt_opt (class_type i)) x.pci_constraint;
  line i ppf "pci_expr =\n";
  f (i+1) ppf x.pci_expr

and class_description i = class_infos "class_description" class_type i

and class_type_declaration i = class_infos "class_type_declaration" class_type i

and class_expr i ppf x =
  line i ppf "class_expr %a\n" fmt_location x.pcl_loc;
  attributes i ppf x.pcl_attributes;
  let i = i+1 in
  match x.pcl_desc with
  | Pcl_constr (li, l) ->
      line i ppf "Pcl_constr %a\n" fmt_longident_loc li;
      list i core_type ppf l;
  | Pcl_structure (cs) ->
      line i ppf "Pcl_structure\n";
      class_structure i ppf cs;
  | Pcl_fun (p, e) ->
      line i ppf "Pcl_fun\n";
      list i class_function_param ppf p;
      class_expr i ppf e;
  | Pcl_apply (ce, l) ->
      line i ppf "Pcl_apply\n";
      class_expr i ppf ce;
      list i label_x_expression ppf l;
  | Pcl_let (lbs, ce, loc_in) ->
      line i ppf "Pcl_let %a\n" fmt_rec_flag lbs.pvbs_rec;
      line (i + 1) ppf "loc_in: %a\n" fmt_location loc_in;
      value_bindings i ppf lbs;
      class_expr i ppf ce;
  | Pcl_constraint (ce, ct) ->
      line i ppf "Pcl_constraint\n";
      class_expr i ppf ce;
      class_type i ppf ct;
  | Pcl_extension (s, arg) ->
      line i ppf "Pcl_extension %a\n" fmt_string_loc s;
      payload i ppf arg
  | Pcl_open (o, e) ->
      line i ppf "Pcl_open\n";
      open_description i ppf o;
      class_expr i ppf e

and class_structure i ppf { pcstr_self = p; pcstr_fields = l } =
  line i ppf "class_structure\n";
  option (i+1) pattern ppf p;
  list (i+1) class_field ppf l;

and class_field i ppf x =
  line i ppf "class_field %a\n" fmt_location x.pcf_loc;
  let i = i + 1 in
  attributes i ppf x.pcf_attributes;
  match x.pcf_desc with
  | Pcf_inherit (ovf, ce, so) ->
      line i ppf "Pcf_inherit %a\n" fmt_override_flag ovf;
      class_expr (i+1) ppf ce;
      option (i+1) string_loc ppf so;
  | Pcf_val (s, mf, k) ->
      line i ppf "Pcf_val %a\n" fmt_mutable_virtual_flag mf;
      line (i+1) ppf "%a\n" fmt_string_loc s;
      class_field_value_kind (i+1) ppf k
  | Pcf_method (s, pf, k) ->
      line i ppf "Pcf_method %a\n" fmt_private_virtual_flag pf;
      line (i+1) ppf "%a\n" fmt_string_loc s;
      class_field_method_kind (i+1) ppf k
  | Pcf_constraint (ct1, ct2) ->
      line i ppf "Pcf_constraint\n";
      core_type (i+1) ppf ct1;
      core_type (i+1) ppf ct2;
  | Pcf_initializer (e) ->
      line i ppf "Pcf_initializer\n";
      expression (i+1) ppf e;
  | Pcf_attribute a ->
      attribute i ppf "Pcf_attribute" a
  | Pcf_extension (s, arg) ->
      line i ppf "Pcf_extension %a\n" fmt_string_loc s;
      payload i ppf arg

and class_field_value_kind i ppf = function
  | Cfk_concrete (o, tc, e) ->
      line i ppf "Concrete %a\n" fmt_override_flag o;
      option i type_constraint ppf tc;
      expression i ppf e
  | Cfk_virtual t ->
      line i ppf "Virtual\n";
      core_type i ppf t

and class_field_method_kind i ppf = function
  | Cfk_concrete (o, (args, t), e) ->
      line i ppf "Concrete %a\n" fmt_override_flag o;
      list i expr_function_param ppf args;
      option i value_constraint ppf t;
      expression i ppf e
  | Cfk_virtual t ->
      line i ppf "Virtual\n";
      core_type i ppf t

and class_declaration i = class_infos "class_declaration" class_expr i

and functor_parameter i ppf x =
  line i ppf "functor_parameter %a\n" fmt_location x.loc;
  let i = i+1 in
  match x.txt with
  | Unit ->
      line i ppf "Unit\n"
  | Named (s, mt) ->
      line i ppf "Named %a\n" fmt_str_opt_loc s;
      module_type i ppf mt

and module_type i ppf x =
  line i ppf "module_type %a\n" fmt_location x.pmty_loc;
  attributes i ppf x.pmty_attributes;
  let i = i+1 in
  match x.pmty_desc with
  | Pmty_ident li -> line i ppf "Pmty_ident %a\n" fmt_longident_loc li;
  | Pmty_alias li -> line i ppf "Pmty_alias %a\n" fmt_longident_loc li;
  | Pmty_signature (s) ->
      line i ppf "Pmty_signature\n";
      signature i ppf s;
  | Pmty_functor (params, mt, short) ->
      line i ppf "Pmty_functor short=%b\n" short;
      list i functor_parameter ppf params;
      module_type i ppf mt
  | Pmty_with (mt, l) ->
      line i ppf "Pmty_with\n";
      module_type i ppf mt;
      list i with_constraint ppf l;
  | Pmty_typeof m ->
      line i ppf "Pmty_typeof\n";
      module_expr i ppf m;
  | Pmty_extension (s, arg) ->
      line i ppf "Pmod_extension %a\n" fmt_string_loc s;
      payload i ppf arg

and signature i ppf x = list i signature_item ppf x

and signature_item i ppf x =
  line i ppf "signature_item %a\n" fmt_location x.psig_loc;
  let i = i+1 in
  match x.psig_desc with
  | Psig_value vd ->
      line i ppf "Psig_value\n";
      value_description i ppf vd;
  | Psig_type (rf, l) ->
      line i ppf "Psig_type %a\n" fmt_rec_flag rf;
      list i type_declaration ppf l;
  | Psig_typesubst l ->
      line i ppf "Psig_typesubst\n";
      list i type_declaration ppf l;
  | Psig_typext te ->
      line i ppf "Psig_typext\n";
      type_extension i ppf te
  | Psig_exception te ->
      line i ppf "Psig_exception\n";
      type_exception i ppf te
  | Psig_module pmd ->
      line i ppf "Psig_module\n";
      module_declaration i ppf pmd
  | Psig_modsubst pms ->
      line i ppf "Psig_modsubst %a = %a\n"
        fmt_string_loc pms.pms_name
        fmt_longident_loc pms.pms_manifest;
      fmt_location ppf pms.pms_loc;
      ext_attrs i ppf pms.pms_ext_attrs;
  | Psig_recmodule decls ->
      line i ppf "Psig_recmodule\n";
      list i module_declaration ppf decls;
  | Psig_modtype x ->
      line i ppf "Psig_modtype\n";
      module_type_declaration i ppf x
  | Psig_modtypesubst x ->
      line i ppf "Psig_modtypesubst\n";
      module_type_declaration i ppf x
  | Psig_open od ->
      line i ppf "Psig_open\n";
      open_description i ppf od
  | Psig_include incl ->
      line i ppf "Psig_include\n";
      include_description i ppf incl
  | Psig_class (l) ->
      line i ppf "Psig_class\n";
      list i class_description ppf l;
  | Psig_class_type (l) ->
      line i ppf "Psig_class_type\n";
      list i class_type_declaration ppf l;
  | Psig_extension ((s, arg), attrs) ->
      line i ppf "Psig_extension %a\n" fmt_string_loc s;
      attributes i ppf attrs;
      payload i ppf arg
  | Psig_attribute a ->
      attribute i ppf "Psig_attribute" a

and modtype_declaration i ppf = function
  | None -> line i ppf "#abstract"
  | Some mt -> module_type (i+1) ppf mt

and with_constraint i ppf x =
  match x with
  | Pwith_type (lid, td) ->
      line i ppf "Pwith_type %a\n" fmt_longident_loc lid;
      type_declaration (i+1) ppf td;
  | Pwith_typesubst (lid, td) ->
      line i ppf "Pwith_typesubst %a\n" fmt_longident_loc lid;
      type_declaration (i+1) ppf td;
  | Pwith_module (lid1, lid2) ->
      line i ppf "Pwith_module %a = %a\n"
        fmt_longident_loc lid1
        fmt_longident_loc lid2;
  | Pwith_modsubst (lid1, lid2) ->
      line i ppf "Pwith_modsubst %a = %a\n"
        fmt_longident_loc lid1
        fmt_longident_loc lid2;
  | Pwith_modtype (lid1, mty) ->
      line i ppf "Pwith_modtype %a\n"
        fmt_longident_loc lid1;
      module_type (i+1) ppf mty
  | Pwith_modtypesubst (lid1, mty) ->
     line i ppf "Pwith_modtypesubst %a\n"
        fmt_longident_loc lid1;
      module_type (i+1) ppf mty

and module_expr i ppf x =
  line i ppf "module_expr %a\n" fmt_location x.pmod_loc;
  attributes i ppf x.pmod_attributes;
  let i = i+1 in
  match x.pmod_desc with
  | Pmod_ident (li) -> line i ppf "Pmod_ident %a\n" fmt_longident_loc li;
  | Pmod_structure (s) ->
      line i ppf "Pmod_structure\n";
      structure i ppf s;
  | Pmod_functor (params, me) ->
      line i ppf "Pmod_functor\n";
      list i functor_parameter ppf params;
      module_expr i ppf me
  | Pmod_apply (me1, me2) ->
      line i ppf "Pmod_apply\n";
      module_expr i ppf me1;
      module_expr i ppf me2;
  | Pmod_constraint (me, mt) ->
      line i ppf "Pmod_constraint\n";
      module_expr i ppf me;
      module_type i ppf mt;
  | Pmod_unpack (e, ty1, ty2) ->
      line i ppf "Pmod_unpack\n";
      expression i ppf e;
      option i package_type ppf ty1;
      option i package_type ppf ty2
  | Pmod_apply_unit (x, loc) ->
      line i ppf "Pmod_apply_unit\n";
      module_expr i ppf x;
      line (i+1) ppf "() %a" fmt_location loc
  | Pmod_extension (s, arg) ->
      line i ppf "Pmod_extension %a\n" fmt_string_loc s;
      payload i ppf arg
  | Pmod_hole ->
      line i ppf "Pmod_hole\n"

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf x =
  line i ppf "structure_item %a\n" fmt_location x.pstr_loc;
  let i = i+1 in
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
      line i ppf "Pstr_eval\n";
      attributes i ppf attrs;
      expression i ppf e;
  | Pstr_value l ->
      line i ppf "Pstr_value %a\n" fmt_rec_flag l.pvbs_rec;
      value_bindings i ppf l
  | Pstr_primitive vd ->
      line i ppf "Pstr_primitive\n";
      value_description i ppf vd;
  | Pstr_type (rf, l) ->
      line i ppf "Pstr_type %a\n" fmt_rec_flag rf;
      list i type_declaration ppf l;
  | Pstr_typext te ->
      line i ppf "Pstr_typext\n";
      type_extension i ppf te
  | Pstr_exception te ->
      line i ppf "Pstr_exception\n";
      type_exception i ppf te
  | Pstr_module x ->
      line i ppf "Pstr_module\n";
      module_binding i ppf x
  | Pstr_recmodule bindings ->
      line i ppf "Pstr_recmodule\n";
      list i module_binding ppf bindings;
  | Pstr_modtype x ->
      line i ppf "Pstr_modtype\n";
      module_type_declaration i ppf x
  | Pstr_open od ->
      line i ppf "Pstr_open\n";
      open_declaration i ppf od
  | Pstr_class (l) ->
      line i ppf "Pstr_class\n";
      list i class_declaration ppf l;
  | Pstr_class_type (l) ->
      line i ppf "Pstr_class_type\n";
      list i class_type_declaration ppf l;
  | Pstr_include incl ->
      line i ppf "Pstr_include\n";
      include_declaration i ppf incl
  | Pstr_extension ((s, arg), attrs) ->
      line i ppf "Pstr_extension %a\n" fmt_string_loc s;
      attributes i ppf attrs;
      payload i ppf arg
  | Pstr_attribute a ->
      attribute i ppf "Pstr_attribute" a

and module_type_declaration i ppf x =
  line i ppf "module_type_declaration %a %a\n" fmt_string_loc x.pmtd_name
    fmt_location x.pmtd_loc;
  ext_attrs i ppf x.pmtd_ext_attrs;
  modtype_declaration (i+1) ppf x.pmtd_type

and module_declaration i ppf pmd =
  line i ppf "module_declaration %a %a\n" fmt_str_opt_loc pmd.pmd_name
    fmt_location pmd.pmd_loc;
  list i functor_parameter ppf pmd.pmd_args;
  ext_attrs i ppf pmd.pmd_ext_attrs;
  module_type (i+1) ppf pmd.pmd_type;

and module_binding i ppf x =
  line i ppf "module_binding %a %a\n" fmt_str_opt_loc x.pmb_name
    fmt_location x.pmb_loc;
  list i functor_parameter ppf x.pmb_args;
  ext_attrs i ppf x.pmb_ext_attrs;
  module_expr (i+1) ppf x.pmb_expr

and core_type_x_core_type_x_location i ppf (ct1, ct2, l) =
  line i ppf "<constraint> %a\n" fmt_location l;
  core_type (i+1) ppf ct1;
  core_type (i+1) ppf ct2;

and constructor_decl i ppf
     {pcd_name; pcd_vars; pcd_args; pcd_res; pcd_loc; pcd_attributes} =
  line i ppf "%a\n" fmt_location pcd_loc;
  line (i+1) ppf "%a\n" fmt_string_loc pcd_name;
  if pcd_vars <> [] then line (i+1) ppf "pcd_vars =%a\n" typevars pcd_vars;
  attributes i ppf pcd_attributes;
  constructor_arguments (i+1) ppf pcd_args;
  option (i+1) core_type ppf pcd_res

and constructor_arguments i ppf = function
  | Pcstr_tuple l -> list i core_type ppf l
  | Pcstr_record (_, l) -> list i label_decl ppf l

and label_decl i ppf {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes}=
  line i ppf "%a\n" fmt_location pld_loc;
  attributes i ppf pld_attributes;
  line (i+1) ppf "%a\n" fmt_mutable_flag pld_mutable;
  line (i+1) ppf "%a" fmt_string_loc pld_name;
  core_type (i+1) ppf pld_type

and longident_x_pattern i ppf (li, t, p) =
  line i ppf "%a\n" fmt_longident_loc li;
  option (i+1) core_type ppf t;
  option (i+1) pattern ppf p;

and case i ppf {pc_lhs; pc_guard; pc_rhs} =
  line i ppf "<case>\n";
  pattern (i+1) ppf pc_lhs;
  begin match pc_guard with
  | None -> ()
  | Some g -> line (i+1) ppf "<when>\n"; expression (i + 2) ppf g
  end;
  expression (i+1) ppf pc_rhs;

and value_binding i ppf x =
  line i ppf "<def> %a is_pun=%b\n" fmt_location x.pvb_loc x.pvb_is_pun;
  let i = i + 1 in
  ext_attrs i ppf x.pvb_attributes;
  pattern i ppf x.pvb_pat;
  line i ppf "args\n";
  list i expr_function_param ppf x.pvb_args;
  Option.iter (value_constraint i ppf) x.pvb_constraint;
  function_body i ppf x.pvb_body

and value_constraint i ppf x =
  let pp_sep ppf () = Format.fprintf ppf "@ "; in
  let pp_newtypes = Format.pp_print_list fmt_string_loc ~pp_sep in
  match x with
  | Pvc_constraint { locally_abstract_univars = []; typ } ->
      core_type i ppf typ
  | Pvc_constraint { locally_abstract_univars=newtypes; typ} ->
      line i ppf "<type> %a.\n" pp_newtypes newtypes;
      core_type i ppf  typ
  | Pvc_coercion { ground; coercion} ->
      line i ppf "<coercion>\n";
      option i core_type ppf ground;
      core_type i ppf coercion;

and open_infos : 'a. _ -> (_ -> _ -> 'a -> _) -> _ -> _ ->  'a open_infos -> _ =
 fun label f i ppf x ->
  line i ppf "%s %a %a\n" label fmt_override_flag x.popen_override
    fmt_location x.popen_loc;
  ext_attrs i ppf x.popen_attributes;
  f (i+1) ppf x.popen_expr

and open_description i = open_infos "open_description" longident_loc i

and open_declaration i = open_infos "open_declaration" module_expr i

and include_infos : 'a. _ -> (_ -> _ -> 'a -> _) -> _ -> _ -> 'a include_infos -> _ =
 fun label f i ppf x ->
  line i ppf "%s %a\n" label fmt_location x.pincl_loc;
  ext_attrs i ppf x.pincl_attributes;
  f (i+1) ppf x.pincl_mod

and include_description i = include_infos "include_description" module_type i

and include_declaration i = include_infos "include_declaration" module_expr i

and value_bindings i ppf x =
  list i value_binding ppf x.pvbs_bindings;

and binding_op i ppf x =
  line i ppf "<binding_op> %a %a"
    fmt_string_loc x.pbop_op fmt_location x.pbop_loc;
  pattern (i+1) ppf x.pbop_pat;
  expression (i+1) ppf x.pbop_exp;

and string_x_expression i ppf (s, e) =
  line i ppf "<override> %a\n" fmt_string_loc s;
  expression (i+1) ppf e;

and longident_x_expression i ppf (li, c, e) =
  line i ppf "%a\n" fmt_longident_loc li;
  option (i+1) type_constraint ppf c;
  option (i+1) expression ppf e;

and label_x_expression i ppf (l,e) =
  line i ppf "<arg>\n";
  arg_label i ppf l;
  expression (i+1) ppf e;

and row_field i ppf x =
  line i ppf "row_field %a\n" fmt_location x.prf_loc;
  attributes i ppf x.prf_attributes;
  let i = i+1 in
  match x.prf_desc with
  | Rtag (l, b, ctl) ->
      line i ppf "Rtag %s\n" (string_of_bool b);
      variant_var i ppf l;
      list i core_type ppf ctl
  | Rinherit (ct) ->
      line i ppf "Rinherit\n";
      core_type i ppf ct

let rec toplevel_phrase i ppf x =
  match x with
  | Ptop_def (s) ->
      line i ppf "Ptop_def\n";
      structure (i+1) ppf s;
  | Ptop_dir {pdir_name; pdir_arg; pdir_loc} ->
      line i ppf "Ptop_dir %a %a\n" fmt_string_loc pdir_name
        fmt_location pdir_loc;
      match pdir_arg with
      | None -> ()
      | Some da -> directive_argument i ppf da;

and directive_argument i ppf x =
  line i ppf "directive_argument %a\n" fmt_location x.pdira_loc;
  let i = i+1 in
  match x.pdira_desc with
  | Pdir_string (s) -> line i ppf "Pdir_string \"%s\"\n" s
  | Pdir_int (n, None) -> line i ppf "Pdir_int %s\n" n
  | Pdir_int (n, Some m) -> line i ppf "Pdir_int %s%c\n" n m
  | Pdir_ident (li) -> line i ppf "Pdir_ident %a\n" fmt_longident li
  | Pdir_bool (b) -> line i ppf "Pdir_bool %s\n" (string_of_bool b)

let repl_phrase i ppf x =
  line i ppf "repl_phrase\n";
  let i = i+1 in
  toplevel_phrase i ppf x.prepl_phrase;
  line i ppf "output %S\n" x.prepl_output

let interface ppf x = list 0 signature_item ppf x

let implementation ppf x = list 0 structure_item ppf x

let top_phrase ppf x = toplevel_phrase 0 ppf x

let repl_phrase ppf x = repl_phrase 0 ppf x

let expression ppf x = expression 0 ppf x

let payload ppf x = payload 0 ppf x

let core_type ppf x = core_type 0 ppf x

let module_type ppf x = module_type 0 ppf x

let pattern ppf x = pattern 0 ppf x

let type_declaration ppf x = type_declaration 0 ppf x

let value_binding ppf x = value_binding 0 ppf x

let module_binding ppf x = module_binding 0 ppf x

let module_declaration ppf x = module_declaration 0 ppf x

let class_expr ppf x = class_expr 0 ppf x

let class_type ppf x = class_type 0 ppf x

let class_field ppf x = class_field 0 ppf x

let class_type_field ppf x = class_type_field 0 ppf x

let module_expr ppf x = module_expr 0 ppf x

let structure_item ppf x = structure_item 0 ppf x

let signature_item ppf x = signature_item 0 ppf x

let expr_function_param ppf x = expr_function_param 0 ppf x

let class_function_param ppf x = class_function_param 0 ppf x

let value_constraint ppf x = value_constraint 0 ppf x

let binding_op ppf x = binding_op 0 ppf x

let class_declaration ppf x = class_declaration 0 ppf x

let class_type_declaration ppf x = class_type_declaration 0 ppf x
