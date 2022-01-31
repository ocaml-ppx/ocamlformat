open Asttypes
open Parsetree
open Jane_syntax_parsing

(******************************************************************************)
(** Individual language extension modules *)

(* Note [Check for immutable extension in comprehensions code]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   When we spot a comprehension for an immutable array, we need to make sure
   that both [comprehensions] and [immutable_arrays] are enabled.  But our
   general mechanism for checking for enabled extensions (in [of_ast]) won't
   work well here: it triggers when converting from
   e.g. [[%jane.non_erasable.comprehensions.array] ...] to the
   comprehensions-specific AST. But if we spot a
   [[%jane.non_erasable.comprehensions.immutable]], there is no expression to
   translate. So we just check for the immutable arrays extension when
   processing a comprehension expression for an immutable array.

   Note [Wrapping with make_entire_jane_syntax]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The topmost node in the encoded AST must always look like e.g.
   [%jane.non_erasable.comprehensions]. (More generally,
   [%jane.ERASABILITY.FEATURE] or [@jane.ERASABILITY.FEATURE].) This allows the
   decoding machinery to know what extension is being used and what function to
   call to do the decoding. Accordingly, during encoding, after doing the hard
   work of converting the extension syntax tree into e.g. Parsetree.expression,
   we need to make a final step of wrapping the result in a [%jane.*.xyz] node.
   Ideally, this step would be done by part of our general structure, like we
   separate [of_ast] and [of_ast_internal] in the decode structure; this design
   would make it structurally impossible/hard to forget taking this final step.

   However, the final step is only one line of code (a call to
   [make_entire_jane_syntax]), but yet the name of the feature varies, as does
   the type of the payload. It would thus take several lines of code to execute
   this command otherwise, along with dozens of lines to create the structure in
   the first place. And so instead we just manually call
   [make_entire_jane_syntax] and refer to this Note as a reminder to authors of
   future syntax features to remember to do this wrapping.
*)

module Builtin = struct
  let make_curry_attr, extract_curry_attr, has_curry_attr =
    Embedded_name.marker_attribute_handler ["curry"]

  let is_curried typ = has_curry_attr typ.ptyp_attributes

  let mark_curried ~loc typ = match typ.ptyp_desc with
    | Ptyp_arrow _ when not (is_curried typ) ->
        Core_type.add_attributes [make_curry_attr ~loc] typ
    | _ -> typ

  let non_syntax_attributes attrs =
    Option.value ~default:attrs (extract_curry_attr attrs)
end

(** Locality modes *)
module Local = struct
  let feature : Feature.t = Language_extension Local

  type constructor_argument = Lcarg_global of core_type

  type nonrec core_type = Ltyp_local of core_type

  type nonrec expression =
    | Lexp_local of expression
    | Lexp_exclave of expression
    | Lexp_constrain_local of expression
      (* Invariant: [Lexp_constrain_local] is the direct child of a
         [Pexp_constraint] or [Pexp_coerce] node.  For more, see the [.mli]
         file. *)

  type nonrec pattern = Lpat_local of pattern
  (* Invariant: [Lpat_local] is always the outermost part of a pattern. *)

  let type_of ~loc ~attrs = function
    | Ltyp_local typ ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature (fun () ->
        (* Although there's only one constructor here, the use of
           [constructor_argument] means we need to be able to tell the two uses
           apart *)
        Core_type.make_jane_syntax feature ["type"; "local"] @@
        Core_type.add_attributes attrs typ)

  let of_type = Core_type.match_jane_syntax_piece feature @@ fun typ -> function
    | ["type"; "local"] -> Some (Ltyp_local typ)
    | _ -> None

  let constr_arg_of ~loc lcarg =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Constructor_argument.make_entire_jane_syntax ~loc feature (fun () ->
      match lcarg with
      | Lcarg_global carg ->
        (* Although there's only one constructor here, the use of [core_type]
           means we need to be able to tell the two uses apart *)
        Constructor_argument.make_jane_syntax
          feature ["constructor_argument"; "global"]
          carg)

  let of_constr_arg =
    Constructor_argument.match_jane_syntax_piece feature @@ fun carg -> function
      | ["constructor_argument"; "global"] -> Some (Lcarg_global carg)
      | _ -> None

  let expr_of ~loc ~attrs = function
    | Lexp_local expr ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Expression.make_jane_syntax feature ["local"] @@
        Expression.add_attributes attrs expr)
    | Lexp_exclave expr ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Expression.make_jane_syntax feature ["exclave"] @@
        Expression.add_attributes attrs expr)
    | Lexp_constrain_local expr ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Expression.make_jane_syntax feature ["constrain_local"] @@
        Expression.add_attributes attrs expr)

  let of_expr =
    Expression.match_jane_syntax_piece feature @@ fun expr -> function
      | ["local"] -> Some (Lexp_local expr)
      | ["exclave"] -> Some (Lexp_exclave expr)
      | ["constrain_local"] -> Some (Lexp_constrain_local expr)
      | _ -> None

  let pat_of ~loc ~attrs = function
    | Lpat_local pat ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () ->
        Pattern.add_attributes attrs pat)

  let of_pat pat = Lpat_local pat
end

(** List and array comprehensions *)
module Comprehensions = struct
  let feature : Feature.t = Language_extension Comprehensions

  type iterator =
    | Range of { start     : expression
               ; stop      : expression
               ; direction : direction_flag }
    | In of expression

  type clause_binding =
    { pattern    : pattern
    ; iterator   : iterator
    ; attributes : attribute list }

  type clause =
    | For of clause_binding list
    | When of expression

  type comprehension =
    { body    : expression
    ; clauses : clause list
    }

  type expression =
    | Cexp_list_comprehension  of comprehension
    | Cexp_array_comprehension of mutable_flag * comprehension

  (* The desugared-to-OCaml version of comprehensions is described by the
     following BNF, where [{% '...' | expr %}] refers to the result of
     [Expression.make_jane_syntax] (via [comprehension_expr]) as described at
     the top of [jane_syntax_parsing.mli].

     {v
         comprehension ::=
           | {% 'comprehension.list' | '[' clauses ']' %}
           | {% 'comprehension.array' | '[|' clauses '|]' %}

         clauses ::=
           | {% 'comprehension.for' | 'let' iterator+ 'in' clauses %}
           | {% 'comprehension.when' | expr ';' clauses %}
           | {% 'comprehension.body' | expr %}

         iterator ::=
           | pattern '=' {% 'comprehension.for.range.upto' | expr ',' expr %}
           | pattern '=' {% 'comprehension.for.range.downto' | expr ',' expr %}
           | pattern '=' {% 'comprehension.for.in' | expr %}
     v}
  *)

  let comprehension_expr = Expression.make_jane_syntax feature

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in [expr_of]. *)

  let expr_of_iterator = function
    | Range { start; stop; direction } ->
        comprehension_expr
          [ "for"
          ; "range"
          ; match direction with
            | Upto   -> "upto"
            | Downto -> "downto" ]
          (Ast_helper.Exp.tuple [start; stop])
    | In seq ->
        comprehension_expr ["for"; "in"] (Ast_helper.Exp.lazy_ seq)
        (* See Note [Wrapping with Pexp_lazy] *)

  let expr_of_clause_binding { pattern; iterator; attributes } =
    Ast_helper.Vb.mk ~attrs:attributes pattern (expr_of_iterator iterator)

  let expr_of_clause clause rest = match clause with
    | For iterators ->
        comprehension_expr
          ["for"]
          (Ast_helper.Exp.let_
             Nonrecursive (List.map expr_of_clause_binding iterators)
             rest)
    | When cond ->
        comprehension_expr ["when"] (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~type_ ~attrs { body; clauses } =
    (* See Note [Wrapping with Pexp_lazy] *)
    comprehension_expr
      type_
      (Expression.add_attributes
         attrs
         (Ast_helper.Exp.lazy_
            (List.fold_right
               expr_of_clause
               clauses
               (comprehension_expr ["body"] (Ast_helper.Exp.lazy_ body)))))

  let expr_of ~loc ~attrs cexpr =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature (fun () ->
      match cexpr with
      | Cexp_list_comprehension comp ->
          expr_of_comprehension ~type_:["list"] ~attrs comp
      | Cexp_array_comprehension (amut, comp) ->
          expr_of_comprehension
            ~type_:[ "array"
                   ; match amut with
                     | Mutable   -> "mutable"
                     | Immutable -> "immutable"
                   ]
            ~attrs
            comp)

  (** Then, we define how to go from the OCaml AST to the nice AST; this is
      the [..._of_expr] family of expressions, culminating in [of_expr]. *)

  module Desugaring_error = struct
    type error =
      | No_clauses
      | Unexpected_attributes of attributes
      (* Note [Wrapping with Pexp_lazy]
         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

         We require that every internal comprehensions node contain at least one
         constructor, using [Pexp_lazy] by convention when there isn't another
         obvious choice.  This means that every internal AST node synthesized
         for comprehensions can contain no other attributes, which we can then
         check for and raise [Unexpected_attributes] if we get this wrong.  This
         helps guard against attribute erros. *)

    let report_error ~loc = function
      | No_clauses ->
          Location.errorf ~loc
            "Tried to desugar a comprehension with no clauses"
      | Unexpected_attributes attrs ->
          Location.errorf ~loc
            "An internal synthesized comprehension node had extra attributes.@.\
             The attributes had the following names:@ %a"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
               (fun ppf attr -> Format.fprintf ppf "\"%s\"" attr.attr_name.txt))
            attrs

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise expr err = raise (Error(expr.pexp_loc, err))
  end

  let match_comprehension_piece matcher =
    Expression.match_jane_syntax_piece feature @@ fun expr subparts ->
      match expr.pexp_attributes with
      | [] -> matcher expr subparts
      | _ :: _ as attrs ->
        Desugaring_error.raise expr (Unexpected_attributes attrs)

  let iterator_of_expr = match_comprehension_piece @@ fun expr subparts ->
    match subparts, expr.pexp_desc with
    |["for"; "range"; "upto"], Pexp_tuple [start; stop] ->
        Some (Range { start; stop; direction = Upto })
    | ["for"; "range"; "downto"], Pexp_tuple [start; stop] ->
        Some (Range { start; stop; direction = Downto })
    | ["for"; "in"], Pexp_lazy seq ->
        Some (In seq)
    | _ -> None

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let comprehension_of_expr =
    let rec raw_comprehension_of_expr expr =
      expr |> match_comprehension_piece @@ fun expr subparts ->
        match subparts, expr.pexp_desc with
        | ["for"], Pexp_let(Nonrecursive, iterators, rest) ->
            Option.some @@ add_clause
              (For (List.map clause_binding_of_vb iterators))
              (raw_comprehension_of_expr rest)
        | ["when"], Pexp_sequence(cond, rest) ->
            Option.some @@ add_clause
              (When cond)
              (raw_comprehension_of_expr rest)
        | ["body"], Pexp_lazy body ->
            Some { body; clauses = [] }
        | _ ->
            None
    in
    fun expr ->
      match raw_comprehension_of_expr expr with
      | { body = _; clauses = [] } ->
          Desugaring_error.raise expr No_clauses
      | comp -> comp

  let of_expr = match_comprehension_piece @@ fun expr subparts ->
    (* See Note [Wrapping with Pexp_lazy] *)
    match subparts, expr.pexp_desc with
    | ["list"], Pexp_lazy comp ->
      Some (Cexp_list_comprehension (comprehension_of_expr comp))
    | ["array"; "mutable"], Pexp_lazy comp ->
      Some (Cexp_array_comprehension (Mutable,
                                      comprehension_of_expr comp))
    | ["array"; "immutable"], Pexp_lazy comp ->
      (* assert_extension_enabled:
         See Note [Check for immutable extension in comprehensions code]
      *)
      assert_extension_enabled ~loc:expr.pexp_loc Immutable_arrays ();
      Some (Cexp_array_comprehension (Immutable,
                                      comprehension_of_expr comp))
    | _ -> None
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression =
    | Iaexp_immutable_array of expression list

  type nonrec pattern =
    | Iapat_immutable_array of pattern list

  let feature : Feature.t = Language_extension Immutable_arrays

  let expr_of ~loc ~attrs = function
    | Iaexp_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Exp.array ~attrs elts)

  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc ~attrs = function
    | Iapat_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Pat.array ~attrs elts)

  let of_pat pat = match pat.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts
    | _ -> failwith "Malformed immutable array pattern"
end

(** [include functor] *)
module Include_functor = struct
  type signature_item =
    | Ifsig_include_functor of include_description

  type structure_item =
    | Ifstr_include_functor of include_declaration

  let feature : Feature.t = Language_extension Include_functor

  let sig_item_of ~loc = function
    | Ifsig_include_functor incl ->
        (* See Note [Wrapping with make_entire_jane_syntax] *)
        Signature_item.make_entire_jane_syntax ~loc feature (fun () ->
          Ast_helper.Sig.include_ incl)

  let of_sig_item sigi = match sigi.psig_desc with
    | Psig_include incl -> Ifsig_include_functor incl
    | _ -> failwith "Malformed [include functor] in signature"

  let str_item_of ~loc = function
    | Ifstr_include_functor incl ->
        (* See Note [Wrapping with make_entire_jane_syntax] *)
        Structure_item.make_entire_jane_syntax ~loc feature (fun () ->
          Ast_helper.Str.include_ incl)

  let of_str_item stri = match stri.pstr_desc with
    | Pstr_include incl -> Ifstr_include_functor incl
    | _ -> failwith "Malformed [include functor] in structure"
end

(** Module strengthening *)
module Strengthen = struct
  type nonrec module_type =
    { mty : Parsetree.module_type; mod_id : Longident.t Location.loc }

  let feature : Feature.t = Language_extension Module_strengthening

  (* Encoding: [S with M] becomes [functor (_ : S) -> (module M)], where
     the [(module M)] is a [Pmty_alias].  This isn't syntax we can write, but
     [(module M)] can be the inferred type for [M], so this should be fine. *)

  let mty_of ~loc ~attrs { mty; mod_id } =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Module_type.make_entire_jane_syntax ~loc feature (fun () ->
      Ast_helper.Mty.functor_ ~attrs (Named (Location.mknoloc None, mty))
        (Ast_helper.Mty.alias mod_id))

  (* Returns remaining unconsumed attributes *)
  let of_mty mty = match mty.pmty_desc with
    | Pmty_functor(Named(_, mty), {pmty_desc = Pmty_alias mod_id}) ->
       { mty; mod_id }
    | _ -> failwith "Malformed strengthened module type"
end

module Unboxed_constants = struct
  type t =
    | Float of string * char option
    | Integer of string * char

  type expression = t
  type pattern = t

  let feature : Feature.t = Language_extension Layouts

  let fail_malformed ~loc =
    Location.raise_errorf ~loc "Malformed unboxed numeric literal"

  let of_constant ~loc = function
    | Pconst_float (x, suffix) -> Float (x, suffix)
    | Pconst_integer (x, Some suffix) -> Integer (x, suffix)
    | Pconst_integer (_, None) ->
        Location.raise_errorf ~loc
          "Malformed unboxed int literal: suffix required"
    | _ -> fail_malformed ~loc


  (* Returns remaining unconsumed attributes *)
  let of_expr expr =
    let loc = expr.pexp_loc in
    match expr.pexp_desc with
    | Pexp_constant const -> of_constant ~loc const
    | _ -> fail_malformed ~loc

  (* Returns remaining unconsumed attributes *)
  let of_pat pat =
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_constant const -> of_constant ~loc const
    | _ -> fail_malformed ~loc

  let constant_of = function
    | Float (x, suffix) -> Pconst_float (x, suffix)
    | Integer (x, suffix) -> Pconst_integer (x, Some suffix)

  let expr_of ~loc ~attrs t =
    let constant = constant_of t in
    Expression.make_entire_jane_syntax ~loc feature (fun () ->
      Ast_helper.Exp.constant ~attrs constant)

  let pat_of ~loc ~attrs t =
    let constant = constant_of t in
    Pattern.make_entire_jane_syntax ~loc feature (fun () ->
      Ast_helper.Pat.constant ~attrs constant)
end

(******************************************************************************)
(** The interface to our novel syntax, which we export *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
  val ast_of : loc:Location.t -> t -> ast
end

module Core_type = struct
  type t =
    | Jtyp_local of Local.core_type

  let of_ast_internal (feat : Feature.t) typ = match feat with
    | Language_extension Local -> Some (Jtyp_local (Local.of_type typ))
    | _ -> None

  let of_ast = Core_type.make_of_ast ~of_ast_internal

  let ast_of ~loc (jtyp, attrs) = match jtyp with
    | Jtyp_local x -> Local.type_of ~loc ~attrs x
end

module Constructor_argument = struct
  type t =
    | Jcarg_local of Local.constructor_argument

  let of_ast_internal (feat : Feature.t) carg = match feat with
    | Language_extension Local -> Some (Jcarg_local (Local.of_constr_arg carg))
    | _ -> None

  let of_ast = Constructor_argument.make_of_ast ~of_ast_internal

  let ast_of ~loc jcarg = match jcarg with
    | Jcarg_local x -> Local.constr_arg_of ~loc x
end

module Expression = struct
  type t =
    | Jexp_local           of Local.expression
    | Jexp_comprehension   of Comprehensions.expression
    | Jexp_immutable_array of Immutable_arrays.expression
    | Jexp_unboxed_constant of Unboxed_constants.expression

  let of_ast_internal (feat : Feature.t) expr = match feat with
    | Language_extension Local ->
      Some (Jexp_local (Local.of_expr expr))
    | Language_extension Comprehensions ->
      Some (Jexp_comprehension (Comprehensions.of_expr expr))
    | Language_extension Immutable_arrays ->
      Some (Jexp_immutable_array (Immutable_arrays.of_expr expr))
    | Language_extension Layouts ->
      Some (Jexp_unboxed_constant (Unboxed_constants.of_expr expr))
    | _ -> None

  let of_ast = Expression.make_of_ast ~of_ast_internal

  let ast_of ~loc (jexp, attrs) = match jexp with
    | Jexp_local            x -> Local.expr_of             ~loc ~attrs x
    | Jexp_comprehension    x -> Comprehensions.expr_of    ~loc ~attrs x
    | Jexp_immutable_array  x -> Immutable_arrays.expr_of  ~loc ~attrs x
    | Jexp_unboxed_constant x -> Unboxed_constants.expr_of ~loc ~attrs x
end

module Pattern = struct
  type t =
    | Jpat_local           of Local.pattern
    | Jpat_immutable_array of Immutable_arrays.pattern
    | Jpat_unboxed_constant of Unboxed_constants.pattern

  let of_ast_internal (feat : Feature.t) pat = match feat with
    | Language_extension Local ->
      Some (Jpat_local (Local.of_pat pat))
    | Language_extension Immutable_arrays ->
      Some (Jpat_immutable_array (Immutable_arrays.of_pat pat))
    | Language_extension Layouts ->
      Some (Jpat_unboxed_constant (Unboxed_constants.of_pat pat))
    | _ -> None

  let of_ast = Pattern.make_of_ast ~of_ast_internal

  let ast_of ~loc (jpat, attrs) = match jpat with
    | Jpat_local x -> Local.pat_of ~loc ~attrs x
    | Jpat_immutable_array x -> Immutable_arrays.pat_of ~loc ~attrs x
    | Jpat_unboxed_constant x -> Unboxed_constants.pat_of ~loc ~attrs x
end

module Module_type = struct
  type t =
    | Jmty_strengthen of Strengthen.module_type

  let of_ast_internal (feat : Feature.t) mty = match feat with
    | Language_extension Module_strengthening ->
      Some (Jmty_strengthen (Strengthen.of_mty mty))
    | _ -> None

  let of_ast = Module_type.make_of_ast ~of_ast_internal

  let ast_of ~loc (jmty, attrs) = match jmty with
    | Jmty_strengthen x -> Strengthen.mty_of ~loc ~attrs x
end

module Signature_item = struct
  type t =
    | Jsig_include_functor of Include_functor.signature_item

  let of_ast_internal (feat : Feature.t) sigi =
    match feat with
    | Language_extension Include_functor ->
      Some (Jsig_include_functor (Include_functor.of_sig_item sigi))
    | _ -> None

  let of_ast = Signature_item.make_of_ast ~of_ast_internal

  let ast_of ~loc jsig = match jsig with
    | Jsig_include_functor x -> Include_functor.sig_item_of ~loc x
end

module Structure_item = struct
  type t =
    | Jstr_include_functor of Include_functor.structure_item

  let of_ast_internal (feat : Feature.t) stri =
    match feat with
    | Language_extension Include_functor ->
      Some (Jstr_include_functor (Include_functor.of_str_item stri))
    | _ -> None

  let of_ast = Structure_item.make_of_ast ~of_ast_internal

  let ast_of ~loc jstr = match jstr with
    | Jstr_include_functor x -> Include_functor.str_item_of ~loc x
end

module Extension_constructor = struct
  type t = |

  let of_ast_internal (feat : Feature.t) _ext = match feat with
    | _ -> None

  let of_ast = Extension_constructor.make_of_ast ~of_ast_internal

  let ast_of ~loc:_ (jext, _attrs) = match jext with
    | (_ : t) -> .
end
