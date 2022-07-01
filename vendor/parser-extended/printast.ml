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

let fmt_constant i f x =
  line i f "constant %a\n" fmt_location x.pconst_loc;
  let i = i+1 in
  match x.pconst_desc with
  | Pconst_integer (j,m) -> line i f "PConst_int (%s,%a)" j fmt_char_option m
  | Pconst_char (c) -> line i f "PConst_char %02x" (Char.code c)
  | Pconst_string (s, strloc, None) ->
      line i f "PConst_string(%S,%a,None)" s fmt_location strloc
  | Pconst_string (s, strloc, Some delim) ->
      line i f "PConst_string (%S,%a,Some %S)" s fmt_location strloc delim
  | Pconst_float (s,m) -> line i f "PConst_float (%s,%a)" s fmt_char_option m

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
let arg_label i ppf = function
  | Nolabel -> line i ppf "Nolabel\n"
  | Optional s -> line i ppf "Optional \"%s\"\n" s
  | Labelled s -> line i ppf "Labelled \"%s\"\n" s

let typevars ppf vs =
  List.iter (fun x ->
      fprintf ppf " %a %a" Pprintast.tyvar x.txt fmt_location x.loc) vs

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
      option i (fun i -> list i string) ppf low
  | Ptyp_object (l, c) ->
      line i ppf "Ptyp_object %a\n" fmt_obj_closed_flag c;
      list i object_field ppf l
  | Ptyp_class (li, l) ->
      line i ppf "Ptyp_class %a\n" fmt_longident_loc li;
      list i core_type ppf l
  | Ptyp_alias (ct, s) ->
      line i ppf "Ptyp_alias \"%s\"\n" s;
      core_type i ppf ct;
  | Ptyp_poly (sl, ct) ->
      line i ppf "Ptyp_poly%a\n" typevars sl;
      core_type i ppf ct;
  | Ptyp_package (s, l) ->
      line i ppf "Ptyp_package %a\n" fmt_longident_loc s;
      list i package_with ppf l;
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
      line i ppf "Ppat_variant \"%s\"\n" l;
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
  | Ppat_or (p1, p2) ->
      line i ppf "Ppat_or\n";
      pattern i ppf p1;
      pattern i ppf p2;
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
  | Ppat_unpack s ->
      line i ppf "Ppat_unpack %a\n" fmt_str_opt_loc s;
  | Ppat_exception p ->
      line i ppf "Ppat_exception\n";
      pattern i ppf p
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
  | Pexp_let (rf, l, e) ->
      line i ppf "Pexp_let %a\n" fmt_rec_flag rf;
      list i value_binding ppf l;
      expression i ppf e;
  | Pexp_function l ->
      line i ppf "Pexp_function\n";
      list i case ppf l;
  | Pexp_fun (l, eo, p, e) ->
      line i ppf "Pexp_fun\n";
      arg_label i ppf l;
      option i expression ppf eo;
      pattern i ppf p;
      expression i ppf e;
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
      line i ppf "Pexp_variant \"%s\"\n" l;
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
  | Pexp_ifthenelse (e1, e2, eo) ->
      line i ppf "Pexp_ifthenelse\n";
      expression i ppf e1;
      expression i ppf e2;
      option i expression ppf eo;
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
  | Pexp_letmodule (s, me, e) ->
      line i ppf "Pexp_letmodule %a\n" fmt_str_opt_loc s;
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
  | Pexp_poly (e, cto) ->
      line i ppf "Pexp_poly\n";
      expression i ppf e;
      option i core_type ppf cto;
  | Pexp_object s ->
      line i ppf "Pexp_object\n";
      class_structure i ppf s
  | Pexp_newtype (s, e) ->
      line i ppf "Pexp_newtype %a\n" fmt_string_loc s;
      expression i ppf e
  | Pexp_pack me ->
      line i ppf "Pexp_pack\n";
      module_expr i ppf me
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
      line i ppf "Pexp_unreachable"
  | Pexp_hole ->
      line i ppf "Pexp_hole"
  | Pexp_beginend e ->
      line i ppf "Pexp_beginend\n";
      expression i ppf e
  | Pexp_cons l ->
      line i ppf "Pexp_cons\n";
      list i expression ppf l

and value_description i ppf x =
  line i ppf "value_description %a %a\n" fmt_string_loc
       x.pval_name fmt_location x.pval_loc;
  attributes i ppf x.pval_attributes;
  core_type (i+1) ppf x.pval_type;
  list (i+1) string ppf x.pval_prim

and type_parameter i ppf (x, _variance) = core_type i ppf x

and type_declaration i ppf x =
  line i ppf "type_declaration %a %a\n" fmt_string_loc x.ptype_name
       fmt_location x.ptype_loc;
  attributes i ppf x.ptype_attributes;
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
  ) l;

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
  attributes i ppf x.ptyext_attributes;
  let i = i+1 in
  line i ppf "ptyext_path = %a\n" fmt_longident_loc x.ptyext_path;
  line i ppf "ptyext_params =\n";
  list (i+1) type_parameter ppf x.ptyext_params;
  line i ppf "ptyext_constructors =\n";
  list (i+1) extension_constructor ppf x.ptyext_constructors;
  line i ppf "ptyext_private = %a\n" fmt_private_flag x.ptyext_private;

and type_exception i ppf x =
  line i ppf "type_exception %a\n" fmt_location x.ptyexn_loc;
  attributes i ppf x.ptyexn_attributes;
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
  core_type (i+1) ppf cs.pcsig_self;
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

and class_description i ppf x =
  line i ppf "class_description %a\n" fmt_location x.pci_loc;
  attributes i ppf x.pci_attributes;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i ppf "pci_params =\n";
  list (i+1) type_parameter ppf x.pci_params;
  line i ppf "pci_name = %a\n" fmt_string_loc x.pci_name;
  line i ppf "pci_expr =\n";
  class_type (i+1) ppf x.pci_expr;

and class_type_declaration i ppf x =
  line i ppf "class_type_declaration %a\n" fmt_location x.pci_loc;
  attributes i ppf x.pci_attributes;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i ppf "pci_params =\n";
  list (i+1) type_parameter ppf x.pci_params;
  line i ppf "pci_name = %a\n" fmt_string_loc x.pci_name;
  line i ppf "pci_expr =\n";
  class_type (i+1) ppf x.pci_expr;

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
  | Pcl_fun (l, eo, p, e) ->
      line i ppf "Pcl_fun\n";
      arg_label i ppf l;
      option i expression ppf eo;
      pattern i ppf p;
      class_expr i ppf e;
  | Pcl_apply (ce, l) ->
      line i ppf "Pcl_apply\n";
      class_expr i ppf ce;
      list i label_x_expression ppf l;
  | Pcl_let (rf, l, ce) ->
      line i ppf "Pcl_let %a\n" fmt_rec_flag rf;
      list i value_binding ppf l;
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
  pattern (i+1) ppf p;
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
      class_field_kind (i+1) ppf k
  | Pcf_method (s, pf, k) ->
      line i ppf "Pcf_method %a\n" fmt_private_virtual_flag pf;
      line (i+1) ppf "%a\n" fmt_string_loc s;
      class_field_kind (i+1) ppf k
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

and class_field_kind i ppf = function
  | Cfk_concrete (o, e) ->
      line i ppf "Concrete %a\n" fmt_override_flag o;
      expression i ppf e
  | Cfk_virtual t ->
      line i ppf "Virtual\n";
      core_type i ppf t

and class_declaration i ppf x =
  line i ppf "class_declaration %a\n" fmt_location x.pci_loc;
  attributes i ppf x.pci_attributes;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i ppf "pci_params =\n";
  list (i+1) type_parameter ppf x.pci_params;
  line i ppf "pci_name = %a\n" fmt_string_loc x.pci_name;
  line i ppf "pci_expr =\n";
  class_expr (i+1) ppf x.pci_expr;

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
  | Pmty_functor (Unit, mt2) ->
      line i ppf "Pmty_functor ()\n";
      module_type i ppf mt2;
  | Pmty_functor (Named (s, mt1), mt2) ->
      line i ppf "Pmty_functor %a\n" fmt_str_opt_loc s;
      module_type i ppf mt1;
      module_type i ppf mt2;
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
      attributes i ppf pms.pms_attributes;
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
  | Pmod_functor (Unit, me) ->
      line i ppf "Pmod_functor ()\n";
      module_expr i ppf me;
  | Pmod_functor (Named (s, mt), me) ->
      line i ppf "Pmod_functor %a\n" fmt_str_opt_loc s;
      module_type i ppf mt;
      module_expr i ppf me;
  | Pmod_apply (me1, me2) ->
      line i ppf "Pmod_apply\n";
      module_expr i ppf me1;
      module_expr i ppf me2;
  | Pmod_constraint (me, mt) ->
      line i ppf "Pmod_constraint\n";
      module_expr i ppf me;
      module_type i ppf mt;
  | Pmod_unpack (e) ->
      line i ppf "Pmod_unpack\n";
      expression i ppf e;
  | Pmod_extension (s, arg) ->
      line i ppf "Pmod_extension %a\n" fmt_string_loc s;
      payload i ppf arg
  | Pmod_hole ->
      line i ppf "Pmod_hole"

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf x =
  line i ppf "structure_item %a\n" fmt_location x.pstr_loc;
  let i = i+1 in
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
      line i ppf "Pstr_eval\n";
      attributes i ppf attrs;
      expression i ppf e;
  | Pstr_value (rf, l) ->
      line i ppf "Pstr_value %a\n" fmt_rec_flag rf;
      list i value_binding ppf l;
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
  attributes i ppf x.pmtd_attributes;
  modtype_declaration (i+1) ppf x.pmtd_type

and module_declaration i ppf pmd =
  line i ppf "module_declaration %a %a\n" fmt_str_opt_loc pmd.pmd_name
    fmt_location pmd.pmd_loc;
  attributes i ppf pmd.pmd_attributes;
  module_type (i+1) ppf pmd.pmd_type;

and module_binding i ppf x =
  line i ppf "module_binding %a %a\n" fmt_str_opt_loc x.pmb_name
    fmt_location x.pmb_loc;
  attributes i ppf x.pmb_attributes;
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
  | Pcstr_record l -> list i label_decl ppf l

and label_decl i ppf {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes}=
  line i ppf "%a\n" fmt_location pld_loc;
  attributes i ppf pld_attributes;
  line (i+1) ppf "%a\n" fmt_mutable_flag pld_mutable;
  line (i+1) ppf "%a" fmt_string_loc pld_name;
  core_type (i+1) ppf pld_type

and longident_x_pattern i ppf (li, p) =
  line i ppf "%a\n" fmt_longident_loc li;
  pattern (i+1) ppf p;

and case i ppf {pc_lhs; pc_guard; pc_rhs} =
  line i ppf "<case>\n";
  pattern (i+1) ppf pc_lhs;
  begin match pc_guard with
  | None -> ()
  | Some g -> line (i+1) ppf "<when>\n"; expression (i + 2) ppf g
  end;
  expression (i+1) ppf pc_rhs;

and open_description i ppf x =
  line i ppf "open_description %a %a\n" fmt_override_flag x.popen_override
    fmt_location x.popen_loc;
  attributes i ppf x.popen_attributes;
  fmt_longident_loc ppf x.popen_expr

and open_declaration i ppf x =
  line i ppf "open_declaration %a %a\n" fmt_override_flag x.popen_override
    fmt_location x.popen_loc;
  attributes i ppf x.popen_attributes;
  let i = i+1 in
  module_expr i ppf x.popen_expr

and include_description i ppf x =
  line i ppf "include_description %a\n" fmt_location x.pincl_loc;
  attributes i ppf x.pincl_attributes;
  let i = i+1 in
  module_type i ppf x.pincl_mod

and include_declaration i ppf x =
  line i ppf "include_declaration %a\n" fmt_location x.pincl_loc;
  attributes i ppf x.pincl_attributes;
  let i = i+1 in
  module_expr i ppf x.pincl_mod

and value_binding i ppf x =
  line i ppf "<def> %a\n" fmt_location x.pvb_loc;
  attributes (i+1) ppf x.pvb_attributes;
  pattern (i+1) ppf x.pvb_pat;
  expression (i+1) ppf x.pvb_expr

and binding_op i ppf x =
  line i ppf "<binding_op> %a %a"
    fmt_string_loc x.pbop_op fmt_location x.pbop_loc;
  pattern (i+1) ppf x.pbop_pat;
  expression (i+1) ppf x.pbop_exp;

and string_x_expression i ppf (s, e) =
  line i ppf "<override> %a\n" fmt_string_loc s;
  expression (i+1) ppf e;

and longident_x_expression i ppf (li, e) =
  line i ppf "%a\n" fmt_longident_loc li;
  expression (i+1) ppf e;

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
      line i ppf "Rtag %a %s\n" fmt_string_loc l (string_of_bool b);
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

let class_expr ppf x = class_expr 0 ppf x

let class_type ppf x = class_type 0 ppf x

let class_field ppf x = class_field 0 ppf x

let class_type_field ppf x = class_type_field 0 ppf x

let module_expr ppf x = module_expr 0 ppf x

let structure_item ppf x = structure_item 0 ppf x

let signature_item ppf x = signature_item 0 ppf x
