(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Attribute
open IL
open CodeBits

(* This module generates the code and the signature of the unparsing API. *)

(* It is organized in three sections, each of which is a functor. The first
   section defines common naming conventions. The second section generates
   the code. The last section generates a signature. *)

(* The input signature of each functor is a minimal subset of the signature
   exposed by the [Grammar] module. *)

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* Naming conventions. *)

module NamingConventions (G : sig
  module Terminal : sig
    type t
    val print: t -> string
  end
  module Nonterminal : sig
    type t
    val print: bool -> t -> string
  end
  module Production : sig
    type index
    val attributes: index -> attributes
  end
end)
= struct
open G

(* ------------------------------------------------------------------------ *)

(* Public conventional names. *)

(* These names are visible in the unparsing API. *)

(* ------------------------------------------------------------------------ *)

(**The DCST submodule. *)
let mDCST =
  "DCST"

(**The DCST type associated with [nt]. *)
let dcst_name nt =
  Nonterminal.print true nt

(**The DCST choice constructor associated with [nt]. *)
let dcst_choice_name nt =
  sprintf "%s_choice" (dcst_name nt)

(**The DCST constructor associated with [prod]. *)
let dcst_constructor_name prod = (* can raise [Not_found] *)
  (* If a [@name] attribute is associated with this production, use
     its payload as the name of this production. Otherwise, fail:
     this constructor will not be exposed. *)
  find_attribute "name" (Production.attributes prod)
  |> String.lowercase_ascii

(* ------------------------------------------------------------------------ *)

(**The CST submodule. *)
let mCST =
  "CST"

(**The CST type associated with [nt]. *)
let cst_name nt =
  dcst_name nt

(**The CST visitor class. *)
let reduce =
  "reduce"

(**The method associated with the nonterminal symbol [nt]. *)
let nt_method_name nt =
  sprintf "visit_%s" (cst_name nt)

(**The method associated with production [prod]. *)
let prod_method_name branch = (* can raise [Not_found] *)
  sprintf "case_%s" (dcst_constructor_name branch)

(**The privacy flag of the method associated with production [prod]. *)
let prod_method_privacy prod =
  match dcst_constructor_name prod with
  | _name ->
      Public
  | exception Not_found ->
      (* If this production does not have a [@name] attribute, then
         this method is private. It exists but cannot be invoked or
         overridden by the user. *)
      Private

(**The method associated with the terminal symbol [tok]. *)
let tok_method_name tok =
  sprintf "visit_%s" (Terminal.print tok)

(**The [zero] method: the neutral element of result concatenation. *)
let zero_method_name =
  "zero"

(**The [cat] method: result concatenation. *)
let cat_method_name =
  "cat"

(**The [text] method: conversion of a string to a result. *)
let text_method_name =
  "text"

(* ------------------------------------------------------------------------ *)

(**The [Settle] submodule. *)
let mSettle =
  "Settle"

(**The (unqualified) public name of the [settle] function for symbol [nt]. *)
let settle_name nt =
  Nonterminal.print true nt

(* ------------------------------------------------------------------------ *)

(* Internal names. *)

(**The name used to refer to self in an object.
   This name is fixed in the [Printer] module,
   but could be made flexible if necessary. *)
let self =
  "self"

let eself =
  evar self

(**The type variable that represents the result type of every method. *)
let reduce_result =
  "r"

let tresult =
  tvar reduce_result

end

(* End of the naming conventions *)

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

module Code (G : sig
  module Terminal : sig
    type t
    val sharp: t
    val t2i: t -> int
    val print: t -> string
    val real: t -> bool
    val unquoted_alias: t -> string option
    val ocamltype: t -> Stretch.ocamltype option
    val map_real: (t -> 'a) -> 'a list
  end
  module Nonterminal : sig
    type t
    val print: bool -> t -> string
    val mapx: (t -> 'a) -> 'a list
  end
  module Symbol : sig
    type t =
      | N of Nonterminal.t
      | T of Terminal.t
  end
  module Production : sig
    type index
    val p2i: index -> int
    val print: index -> string
    val rhs: index -> Symbol.t array
    val attributes: index -> attributes
    val positions: index -> Positions.t list
    val error_free: index -> bool
    val mapnt:  Nonterminal.t -> (index -> 'a) -> 'a list
    val mapx: (index -> 'a) -> 'a list
  end
end)
(A : sig
  (* A list of the start symbols and corresponding start states. *)
  val entry: (G.Nonterminal.t * int) list
end)
(N : sig
  val tables: string
end) = struct
open G
open A
open N

(* ------------------------------------------------------------------------ *)

(* Check the names of all productions. If a production has no name, or if two
   productions have the same name, fail. *)

(* Due to inlining and to the expansion of parameterized nonterminal symbols,
   it may be somewhat difficult for the user to ensure that names are
   unique. *)

include struct
open StringSet

let c = Error.new_category()

let find_name_attribute prod : string option =
  try
    Some (find_attribute "name" (Production.attributes prod))
  with Not_found ->
    Error.signal c (Production.positions prod)
      "This production has no @name attribute.";
    None

let duplicates, _ =
  let names : string option list = Production.mapx find_name_attribute in
  List.fold_left (fun (duplicates, names) oname ->
    match oname with
    | None ->
        duplicates, names
    | Some name ->
        if mem name names then
          add name duplicates, names
        else
          duplicates, add name names
  ) (empty, empty) names

let () =
  if not (is_empty duplicates) then begin
    (* We provide no source code positions for this error message. We could
       try, but the [@name] attributes that are synthesized during inlining
       sometimes carry no meaningful position, because they are constructed
       by concatenation of several names. *)
    Error.signal c [] "the following production names are not unique:%s"
      (elements duplicates
       |> List.map (fun name -> sprintf "\n  %s" name)
       |> String.concat "")
  end

let () =
  Error.exit_if c

end

(* ------------------------------------------------------------------------ *)

(* Public naming conventions. *)

include NamingConventions(G)

(* Internal naming conventions. *)

let prod_method_name prod =
  try
    prod_method_name prod
  with Not_found ->
    (* If no [@name] attribute exists, we generate a unique internal name
       for this method, which will be private. *)
    sprintf "prod_%d" (Production.p2i prod)

(* ------------------------------------------------------------------------ *)

(* Internal names. *)

(* These names are shared with the library MenhirCST, whose source code is
   found in the directory cst/. *)

(**The name of the library. *)
let menhirCST =
  "MenhirCST"

(**The main functor provided by the library. *)
let make_settle =
  menhirCST ^ ".Settle.Make"

(**The internal name that we use to designate the result of applying
   the functor [MenhirCST.Settle.Make]. *)
let settle =
  "MenhirSettle"

let idcst =
  settle ^ ".DCST.dcst"

let idcst_terminal =
  settle ^ ".DCST.terminal"

let idcst_nonterminal =
  settle ^ ".DCST.nonterminal"

let ichoice =
  settle ^ ".DCST.choice"

let icst =
  settle ^ ".CST.cst"

let icst_terminal =
  settle ^ ".CST.Terminal"

let icst_nonterminal =
  settle ^ ".CST.NonTerminal"

let isettle =
  settle ^ ".settle"

(* ------------------------------------------------------------------------ *)

(* Auxiliary functions for code generation. *)

(* [tokpat tok pat] is a pattern that matches the token [tok] and binds
   its semantic value (if it has one) to the pattern [pat]. *)

let tokpat tok pat =
  let data = TokenType.tokendata (Terminal.print tok) in
  PData (
    data,
    if Terminal.ocamltype tok = None then [] else [ pat ]
  )

(* [tokexpr tok e] is an expression that constructs the token [tok],
   taking its semantic value (if it needs one) from the expression [e]. *)

let tokexpr tok e =
  let data = TokenType.tokendata (Terminal.print tok) in
  EData (
    data,
    if Terminal.ocamltype tok = None then [] else [ e ]
  )

(* ------------------------------------------------------------------------ *)

(* Generated code for the unparsing API. *)

let rec unparsing_API () : structure =

  (* Apply the functor [MenhirCST.Settle.Make] to the parser's tables. This
     yields a submodule [Settle] which contains the (internal) type of DCSTs
     [DCST.dcst], the internal type of CSTs [CST.cst], and the resolution
     algorithm [settle], which translates internal DCSTs to internal CSTs. *)
  SIModuleDef (settle, MApp (MVar make_settle, MVar tables)) ::

  (* Generate the external types of DCSTs. Whereas internally there is a
     single type of all DCSTs, in the public API we want to give the user
     a family of DCST types, one such type per nonterminal symbol. This
     prevents the user from constructing ill-formed DCSTs. *)
  SIModuleDef (mDCST, MStruct (all_dcst_defs())) ::

  (* Generate the external types of CSTs. Whereas internally there is a
     single type of all CSTs, in the public API we want to give the user
     a family of CST types, one such type per nonterminal symbol. This
     allows us to publish case analysis principles without extraneous
     branches. *)
  SIModuleDef (mCST, MStruct (all_cst_defs())) ::

  (* Expose the resolution algorithm. *)
  SIModuleDef (mSettle, MStruct (all_settle_defs())) ::

  []

(* The definitions inside the DCST sub-module. *)

and all_dcst_defs () : structure =
  (* For each (non-start) nonterminal symbol [nt], we define a type named
     [dcst_name nt] as a synonym for the internal type of DCSTs. Then, we
     provide smart constructors for this type: there is one constructor per
     production plus one [choice] constructor. *)
  List.flatten (Nonterminal.mapx dcst_defs)

and dcst_defs nt : structure =
  (* The public type of DCSTs for the nonterminal symbol [nt]. *)
  SITypeDefs [dcst_def nt] ::
  (* The public [choice] constructor for this type. *)
  valdef (defpublic (dcst_choice_name nt) (evar ichoice)) ::
  (* One constructor per production for this type,
     excluding productions that involve the [error] token. *)
  List.flatten (Production.mapnt nt dcst_constructor_def)

and dcst_def nt : typedef = {
  typename = dcst_name nt;
  typeparams = [];
  typerhs = TAbbrev (TypApp (idcst, []));
  typeconstraint = None;
}

and dcst_constructor_def prod : structure =
  try
    if not (Production.error_free prod) then raise Not_found;
    SIComment (Production.print prod) ::
    valdef (
      (* If [dcst_constructor_name prod] raises [Not_found] then the
         definition of this constructor is omitted altogether. *)
      defpublic (dcst_constructor_name prod) (
        (* The constructor takes one formal parameter for each element in the
           right-hand side of the production [prod]. However, the parameters
           that correspond to terminal symbols without a semantic value are
           unnecessary and are removed. *)
        EFun (dcst_constructor_params prod,
          (* The function [nonterminal] is applied to a production index and
             to an array of subtrees that matches the right-hand side of the
             production [prod]. *)
          EApp (evar idcst_nonterminal, [
            EIntConst (Production.p2i prod);
            EArray (dcst_constructor_subtrees prod)
          ])
        )
      )
    ) ::
    []
  with Not_found ->
    []

and dcst_constructor_params prod : pattern list =
  Production.rhs prod
  |> Array.to_list
  |> List.mapi dcst_constructor_param
  (* Drop the unnecessary parameters, which correspond to terminal
     symbols that do not carry a semantic value. *)
  |> Misc.filter_map (fun opat -> opat)
  (* If we end up with a list of zero parameters, insert a unit parameter. *)
  |> (fun pats -> if pats = [] then [ PUnit ] else pats)

and dcst_constructor_param i symbol : pattern option =
  let p = PVar (dcst_constructor_param_name i) in
  match symbol with
  | Symbol.N _ ->
      Some p (* The parameter is a DCST for this symbol. *)
  | Symbol.T tok ->
      assert (Terminal.real tok);
      match Terminal.ocamltype tok with
      | None   -> None
      | Some _ -> Some p (* The parameter is a semantic value for this token. *)

and dcst_constructor_param_name i : string =
  sprintf "x%d" i

and dcst_constructor_subtrees prod : expr list =
  Production.rhs prod
  |> Array.to_list
  |> List.mapi dcst_constructor_subtree

and dcst_constructor_subtree i symbol : expr =
  let x = evar (dcst_constructor_param_name i) in
  match symbol with
  | Symbol.N _ ->
      (* Plug in the nonterminal subtree that we have received. *)
      x
  | Symbol.T tok ->
      assert (Terminal.real tok);
      (* Plug in a terminal subtree that we construct. *)
      EApp (evar idcst_terminal, [tokexpr tok x])

(* The definitions inside the CST sub-module. *)

and all_cst_defs () : structure =
  (* For each (non-start) nonterminal symbol [nt], we define a type named
     [cst_name nt] as a synonym for the internal type of CSTs. *)
  List.flatten (Nonterminal.mapx cst_defs) @
  (* Then, we construct a [reduce] visitor class for this family of types.
     We could parameterize this class over its self type, or parameterize
     it over one type for each nonterminal symbol, but these options seem
     complex, and this complexity is not necessarily useful in practice.
     We choose to parameterize this class over a single type variable,
     which represents the common result type of all methods. *)
  SIClass (Virtual, [reduce_result], reduce, None, reduce_class_fields()) ::
  []

and cst_defs nt : structure =
  (* The public type of CSTs for the nonterminal symbol [nt]. *)
  SITypeDefs [cst_def nt] ::
  []

and cst_def nt : typedef = {
  typename = cst_name nt;
  typeparams = [];
  typerhs = TAbbrev (TypApp (icst, []));
  typeconstraint = None;
}

and reduce_class_fields () : class_fields =
  (* We have a fixed set of virtual methods, [zero], [cat], [text]. *)
  fixed_virtual_methods() @
  (* For each (real) terminal symbol [tok], there is a method. *)
  Terminal.map_real tok_method_def @
  (* For each (non-start) nonterminal symbol [nt], a method maps the
     type [cst_name nt] to the result type [reduce_result]. *)
  Nonterminal.mapx nt_method_def @
  (* For each error-free production [prod], there is also a method. *)
  List.flatten (Production.mapx prod_method_def)

and vmeth m ty =
  CFMethodVirtual (m, type2scheme ty)

and fixed_virtual_methods () : class_fields =
  (* The method [zero] has type ['r]. *)
  vmeth zero_method_name tresult ::
  (* The method [cat] has type ['r -> 'r -> 'r]. *)
  vmeth cat_method_name (marrow [tresult; tresult] tresult) ::
  (* The method [text] has type [string -> 'r]. *)
  vmeth text_method_name (marrow [tstring] tresult) ::
  []

and tok_method_def tok : class_field =
  (* If this token has a semantic value, then we cannot provide a
     meaningful definition for this method, so it must be virtual.
     If this token does not have a semantic value, and if there is
     a token alias for it, then we can use this alias to provide
     a plausible definition. *)
  match Terminal.ocamltype tok with
  | Some stretch ->
      (* Virtual method, one argument. *)
      let ty = TypTextual stretch in
      vmeth (tok_method_name tok) (marrow [ty] tresult)
  | None ->
      match Terminal.unquoted_alias tok with
      | None ->
          (* Virtual method, no argument. *)
          vmeth (tok_method_name tok) tresult
      | Some alias ->
          (* Non-virtual method, no argument. *)
          CFMethod (Public, tok_method_name tok,
            EApp (
              EMethodCall (eself, text_method_name),
              [EStringConst alias]
            )
          )

and nt_method_def nt : class_field =
  let t = "t" in
  CFMethod (Public, nt_method_name nt,
    EFun ([pvar t],
      EAnnot (
        EMatch (evar t,
          nt_method_branches nt
        ),
        type2scheme (tvar reduce_result)
      )
    )
  )

and nt_method_branches nt =
  (* For each production [prod] associated with [nt], there is one branch. *)
  Production.mapnt nt nt_method_branch @
  (* A default branch, which cannot be taken, is also needed. *)
  branch PWildcard (eassert efalse) ::
  []

and nt_method_branch prod =
  let csts = "csts" in
  branch (
    (* The pattern matches production [prod], based on its integer index.
       The array of subtrees is bound to the local variable [csts]. *)
    PData (icst_nonterminal, [pint (Production.p2i prod); pvar csts])
  ) (
    (* The body is a call to the method associated with production [prod]. *)
    EApp (
      EMethodCall (eself, prod_method_name prod),
      prod_method_args csts prod
    )
  )

and prod_method_args csts prod : expr list =
  Production.rhs prod
  |> Array.to_list
  |> List.mapi (prod_method_arg csts)
  (* Drop the unnecessary arguments, which correspond to terminal
     symbols that do not carry a semantic value. *)
  |> Misc.filter_map (fun oexpr -> oexpr)
  (* If we end up with a list of zero arguments, pass a unit argument. *)
  |> (fun exprs -> if exprs = [] then [ EUnit ] else exprs)

and prod_method_arg csts i symbol : expr option =
  let subtree = EArrayAccess (evar csts, EIntConst i) in
  match symbol with
  | Symbol.N _ ->
      (* The argument is a subtree for this symbol. *)
      Some subtree
  | Symbol.T tok ->
      assert (Terminal.real tok);
      match Terminal.ocamltype tok with
      | None   -> None
      | Some _ ->
          (* The argument is a semantic value for this token. *)
          (* The subtree must be a terminal node. Out of this
             terminal node, we extract a token. Then, out this
             token, we extract a semantic value. *)
          let v = "v" in
          let pat = PData (icst_terminal, [tokpat tok (pvar v)]) in
          Some (
            EMatch (subtree,
              branch pat (evar v) ::
              branch PWildcard (eassert efalse) ::
              []
            )
          )

and prod_method_def prod : class_fields =
  if Production.error_free prod then
    CFMethod (
      prod_method_privacy prod,
      prod_method_name prod,
      EFun (
        prod_method_params prod,
        EAnnot (
          prod_method_body prod,
          type2scheme (tvar reduce_result)
        )
      )
    ) ::
    []
  else
    []

and prod_method_param_name i : string =
  dcst_constructor_param_name i

and prod_method_params prod : pattern list =
  dcst_constructor_params prod

and prod_method_body prod : expr =
  reduction (prod_method_subtrees prod)

and prod_method_subtrees prod : expr list =
  Production.rhs prod
  |> Array.to_list
  |> List.mapi prod_method_subtree

and prod_method_subtree i symbol : expr =
  let x = evar (prod_method_param_name i) in
  match symbol with
  | Symbol.N nt ->
      (* Use the nonterminal subtree that we have received.
         Pass it as an actual argument in a recursive call
         to the method associated with [nt]. *)
      EApp (
        EMethodCall (eself, nt_method_name nt),
        [x]
      )
  | Symbol.T tok ->
      assert (Terminal.real tok);
      (* Use the semantic value that we have received.
         Pass it as an actual argument in a call
         to the method associated with [tok]. *)
      EApp (
        EMethodCall (eself, tok_method_name tok),
        match Terminal.ocamltype tok with
        | None   -> []  (* no argument *)
        | Some _ -> [x] (* one argument: the semantic value *)
      )

and reduction (es : expr list) : expr =
  (* Generate code to reduce the list of things [es] into a single thing,
     using [self#cat] as the binary concatenation operator and [self#zero]
     as the unit. *)
  (* We must choose between a left-leaning or a right-leaning reduction.
     We note that OCaml's concatenation operators [@] and [^] are right
     associative, so we follow the same convention. *)
  match es with
  | [] ->
      EMethodCall (eself, zero_method_name)
  | [e] ->
      e
  | e :: es ->
      EApp (EMethodCall (eself, cat_method_name), [e; reduction es])

(* The definitions inside the Settle sub-module. *)

and all_settle_defs () : structure =
  (* For each start symbol [nt], we expose a specialized version of the
     resolution algorithm. We use the entry state associated with [nt]
     as starting state. We use [#] as the lookahead symbol. *)
  List.map (fun (nt, state) ->
    valdef (defpublic (settle_name nt) (settle_def state))
  ) entry

and settle_def (state : int) : expr =
  let t = "t" in
  EFun ([pvar t],
    EApp (
      evar isettle, [
        (* A triple of a DCST, a state, and a terminal symbol. *)
        etuple [
          evar t;
          EIntConst state;
          EIntConst Terminal.(t2i sharp)
        ]
      ]
    )
  )

(* End of the generated code for the unparsing API. *)

end

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

module Interface (G : sig
  module Terminal : sig
    type t
    val print: t -> string
    val real: t -> bool
    val unquoted_alias: t -> string option
    val ocamltype: t -> Stretch.ocamltype option
    val map_real: (t -> 'a) -> 'a list
  end
  module Nonterminal : sig
    type t
    val print: bool -> t -> string
    val mapx: (t -> 'a) -> 'a list
  end
  module Symbol : sig
    type t =
      | N of Nonterminal.t
      | T of Terminal.t
  end
  module Production : sig
    type index
    val print: index -> string
    val rhs: index -> Symbol.t array
    val attributes: index -> attributes
    val error_free: index -> bool
    val mapnt:  Nonterminal.t -> (index -> 'a) -> 'a list
    val mapx: (index -> 'a) -> 'a list
  end
end)
(A : sig
  val entry: G.Nonterminal.t list
end)
= struct
open G
open A

include NamingConventions(G)

let rec unparsing_API () : interface =
  IIComment "The unparsing API." ::
  (* TODO comment each submodule *)
  IIModule (mDCST, MTSigEnd (all_dcst_decls())) ::
  IIModule (mCST, MTSigEnd (all_cst_decls())) ::
  IIModule (mSettle, MTSigEnd (all_settle_decls())) ::
  []

(* The declarations inside the DCST sub-module. *)

and all_dcst_decls () : interface =
  (* For each (non-start) nonterminal symbol [nt], we declare an abstract
     type named [dcst_name nt]. All of the type declarations must come
     first, because the constructor declarations that follow need all of
     them. *)
  List.flatten (Nonterminal.mapx dcst_type_decl) @
  (* Then, we declare the smart constructors. For each nonterminal symbol
     [nt], there is one constructor per (error-free) production plus one
     [choice] constructor. *)
  List.flatten (Nonterminal.mapx dcst_constructor_decls)

and dcst_type_decl nt : interface =
  (* The public type of DCSTs for the nonterminal symbol [nt]. *)
  IIComment (sprintf "The type of DCSTs for the nonterminal symbol [%s]."
               (Nonterminal.print false nt)) ::
  IITypeDecls [dcst_decl nt] ::
  []

and dcst_constructor_decls nt : interface =
  (* The public [choice] constructor for this type. *)
  IIComment (sprintf "The binary choice constructor at type [%s]." (dcst_name nt)) ::
  IIValDecls [(dcst_choice_name nt, type2scheme (dcst_choice_type nt))] ::
  (* One constructor per error-free production for this type. *)
  List.flatten (Production.mapnt nt (dcst_constructor_decl nt))

and dcst_decl nt : typedef = {
  typename = dcst_name nt;
  typeparams = [];
  typerhs = TAbstract;
  typeconstraint = None;
}

and dcst_choice_type nt : typ =
  let dcst = dcst_type nt in
  marrow [dcst; dcst] dcst

and dcst_type nt : typ =
  TypApp (dcst_name nt, [])

and qualified_dcst_type nt : typ =
  TypApp (qualified_dcst_name nt, [])

and qualified_dcst_name nt =
  mDCST ^ "." ^ dcst_name nt

and dcst_constructor_decl nt prod : interface =
  try
    if not (Production.error_free prod) then raise Not_found;
    IIComment (Production.print prod) ::
    IIValDecls [
      (* If [dcst_constructor_name branch] raises [Not_found] then the
         declaration of this constructor is omitted altogether. *)
      dcst_constructor_name prod,
      type2scheme (marrow
        (* The constructor takes one formal parameter for each element in
           the right-hand side of the production [prod]. However, the
           parameters that correspond to terminal symbols without a
           semantic value are unnecessary and are removed. *)
        (dcst_constructor_params prod)
        (dcst_type nt)
      )
    ] ::
    []
  with Not_found ->
    []

and dcst_constructor_params prod : typ list =
  Production.rhs prod
  |> Array.to_list
  |> List.map dcst_constructor_param
  (* Drop the unnecessary parameters, which correspond to terminal
     symbols that do not carry a semantic value. *)
  |> Misc.filter_map (fun otyp -> otyp)
  (* If we end up with a list of zero parameters, insert a unit parameter. *)
  |> (fun typs -> if typs = [] then [ tunit ] else typs)

and dcst_constructor_param symbol : typ option =
  match symbol with
  | Symbol.N nt ->
      Some (dcst_type nt)
      (* The parameter is a DCST for this symbol. *)
  | Symbol.T tok ->
      assert (Terminal.real tok);
      match Terminal.ocamltype tok with
      | None    -> None
      | Some ty -> Some (TypTextual ty)
                   (* The parameter is a semantic value for this token. *)

(* The declarations inside the CST sub-module. *)

and all_cst_decls () : interface =
  (* For each (non-start) nonterminal symbol [nt], we declare an abstract
     type named [cst_name nt]. *)
  List.flatten (Nonterminal.mapx cst_type_decl) @
  (* Then, we publish a [reduce] visitor class for this family of types. *)
  IIComment ("This visitor helps transform concrete syntax trees into something else.") ::
  IIClass (Virtual, [reduce_result], reduce, None, reduce_class_field_specs()) ::
  []

and cst_type nt : typ =
  TypApp (cst_name nt, [])

and qualified_cst_type nt : typ =
  TypApp (qualified_cst_name nt, [])

and qualified_cst_name nt =
  mCST ^ "." ^ cst_name nt

and cst_type_decl nt : interface =
  (* The public type of CSTs for the nonterminal symbol [nt]. *)
  IIComment (sprintf "The type of CSTs for the nonterminal symbol [%s]."
               (Nonterminal.print false nt)) ::
  IITypeDecls [cst_decl nt] ::
  []

and cst_decl nt : typedef = {
  typename = cst_name nt;
  typeparams = [];
  typerhs = TAbstract;
  typeconstraint = None;
}

and reduce_class_field_specs () : class_field_specs =
  (* We have a fixed set of virtual methods, [zero], [cat], [text]. *)
  fixed_virtual_methods() @
  (* For each (real) terminal symbol [tok], there is a method. *)
  CFSComment "One method per terminal symbol." ::
  Terminal.map_real tok_method_decl @
  (* For each (non-start) nonterminal symbol [nt], a method maps the
     type [cst_name nt] to the result type [reduce_result]. *)
  CFSComment "One method per nonterminal symbol." ::
  Nonterminal.mapx nt_method_decl @
  (* For each error-free production [branch], there is also a method.
     However, when a production does not have a [@name] attribute, the
     corresponding method is private. *)
  CFSComment "One method per production." ::
  List.flatten (Production.mapx prod_method_decl)

and vmeth m ty =
  CFSMethod (Virtual, m, type2scheme ty)

and cmeth m ty =
  CFSMethod (NonVirtual, m, type2scheme ty)

and fixed_virtual_methods () : class_field_specs =
  (* The method [zero] has type ['r]. *)
  CFSComment "[zero] is an empty piece of output." ::
  vmeth zero_method_name tresult ::
  (* The method [cat] has type ['r -> 'r -> 'r]. *)
  CFSComment "[cat] concatenates two pieces of output." ::
  vmeth cat_method_name (marrow [tresult; tresult] tresult) ::
  (* The method [text] has type [string -> 'r]. *)
  CFSComment "[text] transforms a string into a piece of output." ::
  vmeth text_method_name (arrow tstring tresult) ::
  []

and tok_method_decl tok : class_field_spec =
  (* If this token has a semantic value, then we cannot provide a
     meaningful definition for this method, so it must be virtual.
     If this token does not have a semantic value, and if there is
     a token alias for it, then we can use this alias to provide
     a plausible definition. *)
  match Terminal.ocamltype tok with
  | Some stretch ->
      (* Virtual method, one argument. *)
      let ty = TypTextual stretch in
      vmeth (tok_method_name tok) (arrow ty tresult)
  | None ->
      match Terminal.unquoted_alias tok with
      | None ->
          (* Virtual method, no argument. *)
          vmeth (tok_method_name tok) tresult
      | Some _ ->
          (* Non-virtual method, no argument. *)
          cmeth (tok_method_name tok) tresult

and nt_method_decl nt : class_field_spec =
  cmeth (nt_method_name nt) (arrow (cst_type nt) tresult)

and prod_method_decl prod : class_field_specs =
  if Production.error_free prod then
    match prod_method_name prod with
    | name ->
        [ cmeth name (marrow (prod_method_params prod) tresult) ]
    | exception Not_found ->
        (* If this production does not have a [@name] attribute, then
           this method is private. It exists but cannot be invoked or
           overridden by the user. *)
        []
  else
    []

and prod_method_params prod : typ list =
  dcst_constructor_params prod

(* The declarations inside the Settle sub-module. *)

and all_settle_decls () : interface =
  (* For each start symbol [nt], we expose a resolution algorithm. *)
  List.map settle_decl entry

and settle_decl nt : interface_item =
  IIValDecls [(settle_name nt, type2scheme (settle_type nt))]

and settle_type nt : typ =
  arrow (qualified_dcst_type nt) (toption (qualified_cst_type nt))

(* End of the signature of the unparsing API. *)

end

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* The following glue code re-packages the functor [Interface] as a function
   that takes a grammar as an argument. This is required in order to avoid a
   dependency of [UnparsingAPI] on [Grammar]. It will hopefully be removed at
   some point in the future (see TODO.md). *)

open Syntax
open BasicSyntax

let unparsing_API (grammar : grammar) : interface =
  let module G = struct
    module Terminal = struct
      type t = terminal
      let print t = t
      let real t = (t <> "error")
      let unquoted_alias t = unquoted_alias grammar t
      let ocamltype t = ocamltype_of_token grammar t
      let map_real f = List.map f (tokens grammar)
    end
    module Nonterminal = struct
      type t = nonterminal
      let print normalize nt =
        if normalize then Misc.normalize nt else nt
      let mapx f =
        List.rev_map f (nonterminals grammar)
    end
    module Symbol = struct
      type t =
        | N of Nonterminal.t
        | T of Terminal.t
    end
    let producer2symbol producer =
      let symbol = producer.producer_symbol in
      if is_nonterminal grammar symbol then
        Symbol.N symbol
      else
        Symbol.T symbol
    module Production = struct
      type index = nonterminal * branch
      let print (nt, branch) = print_production nt branch
      let rhs (_nt, branch) =
        branch.producers
        |> List.map producer2symbol
        |> Array.of_list
      let attributes (_nt, branch) = branch.br_attributes
      let error_free (_nt, branch) = error_free branch
      let mapnt nt f =
        assert (StringMap.mem nt grammar.rules);
        let rule = StringMap.find nt grammar.rules in
        List.map (fun branch -> f (nt, branch)) rule.branches
      let productions grammar : index list =
        StringMap.fold (fun nt rule accu ->
          List.map (fun branch -> (nt, branch)) rule.branches @
          accu
        ) grammar.rules []
      let mapx f =
        List.map f (productions grammar)
    end
  end in
  let module A = struct
    let entry = StringSet.elements grammar.start_symbols
  end in
  let module I = Interface(G)(A) in
  I.unparsing_API()
