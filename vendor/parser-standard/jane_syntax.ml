open Asttypes
open Parsetree
open Jane_syntax_parsing

(****************************************)
(* Helpers used just within this module *)

module type Extension = sig
  val feature : Feature.t
end


module Ast_of (AST : AST with type 'a with_attributes := 'a * attributes)
              (Ext : Extension) : sig
  (* Wrap a bit of AST with a jane-syntax annotation *)
  val wrap_jane_syntax :
    string list ->   (* these strings describe the bit of new syntax *)
    ?payload:payload ->
    AST.ast ->
    AST.ast
end = struct
  let wrap_jane_syntax suffixes ?payload to_be_wrapped =
    AST.make_jane_syntax Ext.feature suffixes ?payload to_be_wrapped
end

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

module type Payload_protocol = sig
  type t

  module Encode : sig
    val as_payload : t loc -> payload
    val list_as_payload : t loc list -> payload
    val option_list_as_payload : t loc option list -> payload
  end

  module Decode : sig
    val from_payload : loc:Location.t -> payload -> t loc
    val list_from_payload : loc:Location.t -> payload -> t loc list
    val option_list_from_payload :
      loc:Location.t -> payload -> t loc option list
  end
end

module type Stringable = sig
  type t
  val of_string : string -> t option
  val to_string : t -> string

  (** For error messages: a name that can be used to identify the
      [t] being converted to and from string, and its indefinite
      article (either "a" or "an").
  *)
  val indefinite_article_and_name : string * string
end

module Make_payload_protocol_of_stringable (Stringable : Stringable)
  : Payload_protocol with type t := Stringable.t = struct
  module Encode = struct
    let as_expr t_loc =
      let string = Stringable.to_string t_loc.txt in
      Ast_helper.Exp.ident
        (Location.mkloc (Longident.Lident string) t_loc.loc)

    let structure_item_of_expr expr =
      { pstr_desc = Pstr_eval (expr, []); pstr_loc = Location.none }

    let structure_item_of_none =
      { pstr_desc = Pstr_attribute { attr_name = Location.mknoloc "none"
                                   ; attr_payload = PStr []
                                   ; attr_loc = Location.none }
      ; pstr_loc = Location.none }

    let as_payload t_loc =
      let expr = as_expr t_loc in
      PStr [ structure_item_of_expr expr ]

    let list_as_payload t_locs =
      let items =
        List.map (fun t_loc -> structure_item_of_expr (as_expr t_loc)) t_locs
      in
      PStr items

    let option_list_as_payload t_locs =
      let items =
        List.map (function
            | None -> structure_item_of_none
            | Some t_loc -> structure_item_of_expr (as_expr t_loc))
          t_locs
      in
      PStr items
  end

  module Desugaring_error = struct
    type error =
      | Unknown_payload of payload

    let report_error ~loc = function
      | Unknown_payload payload ->
        let indefinite_article, name = Stringable.indefinite_article_and_name in
        Location.errorf ~loc
          "Attribute payload does not name %s %s:@;%a"
          indefinite_article name
          (Printast.payload 0) payload

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) ->
              Some (report_error ~loc err)
          | _ -> None)

    let raise ~loc err =
      raise (Error(loc, err))
  end

  module Decode = struct
    (* Avoid exporting a definition that raises [Unexpected]. *)
    open struct
      exception Unexpected

      let from_expr = function
        | { pexp_desc = Pexp_ident payload_lid; _ } ->
            let t =
              match Stringable.of_string (Longident.last payload_lid.txt) with
              | None -> raise Unexpected
              | Some t -> t
            in
            Location.mkloc t payload_lid.loc
        | _ -> raise Unexpected

      let expr_of_structure_item = function
        | { pstr_desc = Pstr_eval (expr, _) } -> expr
        | _ -> raise Unexpected

      let is_none_structure_item = function
        | { pstr_desc = Pstr_attribute { attr_name = { txt = "none" } } } ->
            true
        | _ -> false

      let from_payload payload =
        match payload with
        | PStr [ item ] -> from_expr (expr_of_structure_item item)
        | _ -> raise Unexpected

      let list_from_payload payload =
        match payload with
        | PStr items ->
            List.map (fun item -> from_expr (expr_of_structure_item item)) items
        | _ -> raise Unexpected

      let option_list_from_payload payload =
        match payload with
        | PStr items ->
            List.map (fun item ->
                if is_none_structure_item item
                then None
                else Some (from_expr (expr_of_structure_item item)))
              items
        | _ -> raise Unexpected
    end

    let from_payload ~loc payload : _ loc =
      try from_payload payload
      with Unexpected -> Desugaring_error.raise ~loc (Unknown_payload payload)

    let list_from_payload ~loc payload : _ list =
      try list_from_payload payload
      with Unexpected -> Desugaring_error.raise ~loc (Unknown_payload payload)

    let option_list_from_payload ~loc payload : _ list =
      try option_list_from_payload payload
      with Unexpected -> Desugaring_error.raise ~loc (Unknown_payload payload)
  end
end

module Builtin = struct
  let is_curry_attr = function
    | { attr_name = { txt = name; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _ } ->
      String.equal Jane_syntax_parsing.Marker_attributes.curry name
    | _ -> false

  let is_curried typ = List.exists is_curry_attr typ.ptyp_attributes

  let mark_curried ~loc typ = match typ.ptyp_desc with
  | Ptyp_arrow _ when not (is_curried typ) ->
    let loc = Location.ghostify loc in
    let curry_attr =
      Ast_helper.Attr.mk
        ~loc
        (Location.mkloc Jane_syntax_parsing.Marker_attributes.curry loc)
        (PStr [])
    in
    Core_type.add_attributes [curry_attr] typ
  | _ -> typ

let non_syntax_attributes attrs =
  List.filter (fun attr -> not (is_curry_attr attr)) attrs
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

(** Labeled tuples *)
module Labeled_tuples = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Labeled_tuples
  end

  include Ext

  type nonrec core_type = Lttyp_tuple of (string option * core_type) list

  type nonrec expression = Ltexp_tuple of (string option * expression) list

  type nonrec pattern =
    | Ltpat_tuple of (string option * pattern) list * closed_flag

  let string_of_label = function None -> "" | Some lbl -> lbl

  let label_of_string = function "" -> None | s -> Some s

  let string_of_closed_flag = function Closed -> "closed" | Open -> "open"

  let closed_flag_of_string = function
    | "closed" -> Closed
    | "open" -> Open
    | _ -> failwith "bad closed flag"

  module Desugaring_error = struct
    type error =
      | Malformed
      | Has_payload of payload

    let report_error ~loc = function
      | Malformed ->
        Location.errorf ~loc "Malformed embedded labeled tuple term"
      | Has_payload payload ->
        Location.errorf ~loc
          "Labeled tuples attribute has an unexpected payload:@;%a"
          (Printast.payload 0) payload

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn (function
        | Error (loc, err) -> Some (report_error ~loc err)
        | _ -> None)

    let raise loc err = raise (Error (loc, err))
  end

  let typ_of ~loc ~attrs = function
    | Lttyp_tuple tl ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature (fun () ->
          let names = List.map (fun (label, _) -> string_of_label label) tl in
          Core_type.make_jane_syntax feature names
          @@ Core_type.add_attributes attrs
               (Ast_helper.Typ.tuple (List.map snd tl)))

  (* Returns remaining unconsumed attributes *)
  let of_typ typ =
    let loc = typ.ptyp_loc in
    let typ, labels, payload =
      Core_type.match_payload_jane_syntax feature typ
    in
    match typ.ptyp_desc, payload with
    | Ptyp_tuple components, PStr [] ->
      if List.length labels <> List.length components
      then Desugaring_error.raise typ.ptyp_loc Malformed;
      let labeled_components =
        List.map2 (fun s t -> label_of_string s, t) labels components
      in
      Lttyp_tuple labeled_components
    | _, PStr [] -> Desugaring_error.raise loc Malformed
    | _, _ -> Desugaring_error.raise loc (Has_payload payload)

  let expr_of ~loc ~attrs = function
    | Ltexp_tuple el ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
          let names = List.map (fun (label, _) -> string_of_label label) el in
          Expression.make_jane_syntax feature names
          @@ Expression.add_attributes attrs
               (Ast_helper.Exp.tuple (List.map snd el)))

  (* Returns remaining unconsumed attributes *)
  let of_expr expr =
    let loc = expr.pexp_loc in
    let expr, labels, payload =
      Expression.match_payload_jane_syntax feature expr
    in
    match expr.pexp_desc, payload with
    | Pexp_tuple components, PStr [] ->
      if List.length labels <> List.length components
      then Desugaring_error.raise expr.pexp_loc Malformed;
      let labeled_components =
        List.map2 (fun s e -> label_of_string s, e) labels components
      in
      Ltexp_tuple labeled_components
    | _, PStr [] -> Desugaring_error.raise expr.pexp_loc Malformed
    | _, _ -> Desugaring_error.raise loc (Has_payload payload)

  let pat_of ~loc ~attrs = function
    | Ltpat_tuple (pl, closed) ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () ->
          let names = List.map (fun (label, _) -> string_of_label label) pl in
          Pattern.make_jane_syntax feature
            (string_of_closed_flag closed :: names)
          @@ Pattern.add_attributes attrs
               (Ast_helper.Pat.tuple (List.map snd pl)))

  (* Returns remaining unconsumed attributes *)
  let of_pat pat =
    let loc = pat.ppat_loc in
    let pat, labels, payload =
      Pattern.match_payload_jane_syntax feature pat
    in
    match labels, pat.ppat_desc, payload with
    | closed :: labels, Ppat_tuple components, PStr [] ->
      if List.length labels <> List.length components
      then Desugaring_error.raise pat.ppat_loc Malformed;
      let closed = closed_flag_of_string closed in
      let labeled_components =
        List.map2 (fun s e -> label_of_string s, e) labels components
      in
      Ltpat_tuple (labeled_components, closed)
    | _, _, PStr [] -> Desugaring_error.raise pat.ppat_loc Malformed
    | _, _, _ -> Desugaring_error.raise loc (Has_payload payload)
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

(** Layout annotations' encoding as attribute payload, used in both n-ary
    functions and layouts. *)
module Layout_annotation : sig
  include Payload_protocol with type t := const_layout

  module Decode : sig
    include module type of Decode

    val bound_vars_from_vars_and_payload :
      loc:Location.t -> string Location.loc list -> payload ->
      (string Location.loc * layout_annotation option) list
  end
end = struct
  module Protocol = Make_payload_protocol_of_stringable (struct
      type t = const_layout

      let indefinite_article_and_name = "a", "layout"

      let to_string = function
        | Any -> "any"
        | Value -> "value"
        | Void -> "void"
        | Immediate64 -> "immediate64"
        | Immediate -> "immediate"
        | Float64 -> "float64"

      (* CR layouts v1.5: revise when moving layout recognition away from parser*)
      let of_string = function
        | "any" -> Some Any
        | "value" -> Some Value
        | "void" -> Some Void
        | "immediate" -> Some Immediate
        | "immediate64" -> Some Immediate64
        | "float64" -> Some Float64
        | _ -> None
    end)
  (*******************************************************)
  (* Conversions with a payload *)

  module Encode = Protocol.Encode

  module Decode = struct
    include Protocol.Decode

    module Desugaring_error = struct
      type error =
        | Wrong_number_of_layouts of int * layout_annotation option list

      let report_error ~loc = function
        | Wrong_number_of_layouts (n, layouts) ->
            Location.errorf ~loc
              "Wrong number of layouts in an layout attribute;@;\
              expecting %i but got this list:@;%a"
              n
              (Format.pp_print_list
                (Format.pp_print_option
                    ~none:(fun ppf () -> Format.fprintf ppf "None")
                    (Printast.layout_annotation 0)))
              layouts

      exception Error of Location.t * error

      let () =
        Location.register_error_of_exn
          (function
            | Error(loc, err) ->
                Some (report_error ~loc err)
            | _ -> None)

      let raise ~loc err =
        raise (Error(loc, err))
    end

    let bound_vars_from_vars_and_payload ~loc var_names payload =
      let layouts = option_list_from_payload ~loc payload in
      try
        List.combine var_names layouts
      with
      (* seems silly to check the length in advance when [combine] does *)
        Invalid_argument _ ->
        Desugaring_error.raise ~loc
          (Wrong_number_of_layouts(List.length var_names, layouts))
  end
end

(** Layouts *)
module Layouts = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Layouts
  end

  include Ext

  type constant =
    | Float of string * char option
    | Integer of string * char

  type nonrec expression =
    | Lexp_constant of constant
    | Lexp_newtype of string loc * layout_annotation * expression

  type nonrec pattern =
    | Lpat_constant of constant

  type nonrec core_type =
    | Ltyp_var of { name : string option
                  ; layout : Asttypes.layout_annotation }
    | Ltyp_poly of { bound_vars : (string loc * layout_annotation option) list
                   ; inner_type : core_type }
    | Ltyp_alias of { aliased_type : core_type
                    ; name : string option
                    ; layout : Asttypes.layout_annotation }

  type nonrec extension_constructor =
    | Lext_decl of (string Location.loc *
                    Asttypes.layout_annotation option) list *
                   constructor_arguments *
                   Parsetree.core_type option

  (*******************************************************)
  (* Errors *)

  module Desugaring_error = struct
    type error =
      | No_integer_suffix
      | Unexpected_constant of Parsetree.constant

    let report_error ~loc = function
      | No_integer_suffix ->
        Location.errorf ~loc
          "All unboxed integers require a suffix to determine their size."
      | Unexpected_constant c ->
        Location.errorf ~loc
          "Unexpected unboxed constant:@ %a"
          (Printast.constant) c

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise ~loc err = raise (Error(loc, err))
  end

  module Encode = Layout_annotation.Encode
  module Decode = Layout_annotation.Decode

  (*******************************************************)
  (* Constants *)

  let constant_of = function
    | Float (x, suffix) -> Pconst_float (x, suffix)
    | Integer (x, suffix) -> Pconst_integer (x, Some suffix)

  let of_constant ~loc = function
    | Pconst_float (x, suffix) -> Float (x, suffix)
    | Pconst_integer (x, Some suffix) -> Integer (x, suffix)
    | Pconst_integer (_, None) ->
      Desugaring_error.raise ~loc No_integer_suffix
    | const -> Desugaring_error.raise ~loc (Unexpected_constant const)

  (*******************************************************)
  (* Encoding expressions *)

  let expr_of ~loc ~attrs expr =
    let module Ast_of = Ast_of (Expression) (Ext) in
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature begin fun () ->
      match expr with
      | Lexp_constant c ->
        let constant = constant_of c in
        Ast_of.wrap_jane_syntax ["unboxed"] @@
        Expression.add_attributes attrs @@
        Ast_helper.Exp.constant constant
      | Lexp_newtype (name, layout, inner_expr) ->
        let payload = Encode.as_payload layout in
        Ast_of.wrap_jane_syntax ["newtype"] ~payload @@
        Expression.add_attributes attrs @@
        Ast_helper.Exp.newtype name inner_expr
    end

  (*******************************************************)
  (* Desugaring expressions *)

  let of_expr expr =
    let loc = expr.pexp_loc in
    let expr, subparts, payload =
      Expression.match_payload_jane_syntax feature expr
    in
    match subparts, expr.pexp_desc, payload with
    | [ "unboxed" ], Pexp_constant const, PStr [] ->
        Lexp_constant (of_constant ~loc const)
    | [ "newtype" ], Pexp_newtype (name, inner_expr), payload ->
        let layout = Decode.from_payload ~loc payload in
        Lexp_newtype (name, layout, inner_expr)
    | _ ->
	      Expression.raise_partial_payload_match feature expr subparts payload

  (*******************************************************)
  (* Encoding patterns *)

  let pat_of ~loc ~attrs t =
    Pattern.make_entire_jane_syntax ~loc feature begin fun () ->
      match t with
      | Lpat_constant c ->
        let constant = constant_of c in
        Pattern.add_attributes attrs @@
        Ast_helper.Pat.constant constant
    end

  (*******************************************************)
  (* Desugaring patterns *)

  let of_pat pat =
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_constant const -> Lpat_constant (of_constant ~loc const)
    | _ -> Pattern.raise_partial_match feature pat []

  (*******************************************************)
  (* Encoding types *)

  module Type_of = Ast_of (Core_type) (Ext)

  let type_of ~loc ~attrs typ =
    let exception No_wrap_necessary of Parsetree.core_type in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature begin fun () ->
        match typ with
        | Ltyp_var { name; layout } ->
          let payload = Encode.as_payload layout in
          Type_of.wrap_jane_syntax ["var"] ~payload @@
          Core_type.add_attributes attrs @@
          begin match name with
          | None -> Ast_helper.Typ.any ~loc ()
          | Some name -> Ast_helper.Typ.var ~loc name
          end
        | Ltyp_poly { bound_vars; inner_type } ->
          let var_names, layouts = List.split bound_vars in
          (* Pass the loc because we don't want a ghost location here *)
          let tpoly =
            Core_type.add_attributes attrs (Ast_helper.Typ.poly ~loc var_names inner_type)
          in
          if List.for_all Option.is_none layouts
          then raise (No_wrap_necessary tpoly)
          else
            let payload = Encode.option_list_as_payload layouts in
            Type_of.wrap_jane_syntax ["poly"] ~payload tpoly

        | Ltyp_alias { aliased_type; name; layout } ->
          let payload = Encode.as_payload layout in
          let has_name, inner_typ = match name with
            | None -> "anon", aliased_type
            | Some name -> "named", Ast_helper.Typ.alias aliased_type name
          in
          Type_of.wrap_jane_syntax ["alias"; has_name] ~payload @@
          Core_type.add_attributes attrs @@
          inner_typ
      end
    with
      No_wrap_necessary result_type -> result_type

  (*******************************************************)
  (* Desugaring types *)

  let of_type typ =
    let loc = typ.ptyp_loc in
    let typ, subparts, payload =
      Core_type.match_payload_jane_syntax feature typ
    in
    match subparts, typ.ptyp_desc, payload with
    | [ "var" ], _, _ ->
        let layout = Decode.from_payload ~loc payload in
        let name = match typ.ptyp_desc with
          | Ptyp_any -> None
          | Ptyp_var name -> Some name
          | _ ->
              Core_type.raise_partial_payload_match feature typ subparts payload
        in
        Ltyp_var { name; layout }
    | [ "poly" ], Ptyp_poly (var_names, inner_type), PStr [] ->
        let bound_vars =
          Decode.bound_vars_from_vars_and_payload ~loc var_names payload
        in
        Ltyp_poly { bound_vars; inner_type }
    | [ "alias"; "anon" ], _, _ ->
        let layout = Decode.from_payload ~loc payload in
        Ltyp_alias { aliased_type = typ
                   ; name = None
                   ; layout }
    | [ "alias"; "named" ], Ptyp_alias (inner_type, name), _ ->
        let layout = Decode.from_payload ~loc payload in
        Ltyp_alias { aliased_type = inner_type
                   ; name = Some name
                   ; layout }
    | _ ->
	      Core_type.raise_partial_payload_match feature typ subparts payload

  (*******************************************************)
  (* Encoding extension constructor *)

  module Ext_ctor_of = Ast_of (Extension_constructor) (Ext)

  let extension_constructor_of ~loc ~name ~attrs ?info ?docs ext =
    (* using optional parameters to hook into existing defaulting
       in [Ast_helper.Te.decl], which seems unwise to duplicate *)
    let exception No_wrap_necessary of Parsetree.extension_constructor in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
        Extension_constructor.make_entire_jane_syntax ~loc feature
          begin fun () ->
            match ext with
            | Lext_decl (bound_vars, args, res) ->
              let vars, layouts = List.split bound_vars in
              let ext_ctor =
                (* Pass ~loc here, because the constructor declaration is
                   not a ghost *)
                Ast_helper.Te.decl ~loc ~vars ~args ?info ?docs ?res name
              in
              if List.for_all Option.is_none layouts
              then raise (No_wrap_necessary ext_ctor)
              else
                let payload = Encode.option_list_as_payload layouts in
                Ext_ctor_of.wrap_jane_syntax ["ext"] ~payload @@
                Extension_constructor.add_attributes attrs @@
                ext_ctor
          end
    with
      No_wrap_necessary ext_ctor -> ext_ctor

  (*******************************************************)
  (* Desugaring extension constructor *)

  let of_extension_constructor ext =
    let loc = ext.pext_loc in
    let ext, subparts, payload =
      Extension_constructor.match_payload_jane_syntax feature ext
    in
    match subparts, ext.pext_kind with
    | [ "ext" ], Pext_decl (var_names, args, res) ->
        let bound_vars =
          Decode.bound_vars_from_vars_and_payload ~loc var_names payload
        in
        Lext_decl (bound_vars, args, res)
    | _ ->
        Extension_constructor.raise_partial_payload_match
          feature ext subparts payload

  (*********************************************************)
  (* Constructing a [constructor_declaration] with layouts *)

  module Ctor_decl_of = Ast_of (Constructor_declaration) (Ext)

  let constructor_declaration_of ~loc ~attrs ~info ~vars_layouts ~args
        ~res name =
    let vars, layouts = List.split vars_layouts in
    let ctor_decl =
      Ast_helper.Type.constructor ~loc ~info ~vars ~args ?res name
    in
    let ctor_decl =
      if List.for_all Option.is_none layouts
      then ctor_decl
      else
        let payload = Encode.option_list_as_payload layouts in
        Constructor_declaration.make_entire_jane_syntax ~loc feature
          begin fun () ->
            Ctor_decl_of.wrap_jane_syntax ["vars"] ~payload ctor_decl
          end
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> ctor_decl
    | _ :: _ as attrs ->
        (* See Note [Outer attributes at end] *)
        { ctor_decl with pcd_attributes = ctor_decl.pcd_attributes @ attrs }

  let of_constructor_declaration_internal (feat : Feature.t) ctor_decl =
    match feat with
    | Language_extension Layouts ->
      let loc = ctor_decl.pcd_loc in
      let ctor_decl, subparts, payload =
        Constructor_declaration.match_payload_jane_syntax feature ctor_decl
      in
      begin match subparts with
      | [ "vars" ] ->
        Some
          (Decode.bound_vars_from_vars_and_payload
             ~loc ctor_decl.pcd_vars payload)
      | _ ->
        Constructor_declaration.raise_partial_payload_match
          feature ctor_decl subparts payload
      end
    | _ ->
      None

  let of_constructor_declaration =
    Constructor_declaration.make_of_ast
       ~of_ast_internal:of_constructor_declaration_internal
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
    | Jtyp_layout of Layouts.core_type
    | Jtyp_tuple of Labeled_tuples.core_type

  let of_ast_internal (feat : Feature.t) typ = match feat with
    | Language_extension Local -> Some (Jtyp_local (Local.of_type typ))
    | Language_extension Layouts -> Some (Jtyp_layout (Layouts.of_type typ))
    | Language_extension Labeled_tuples ->
      Some (Jtyp_tuple (Labeled_tuples.of_typ typ))
    | _ -> None

  let of_ast = Core_type.make_of_ast ~of_ast_internal

  let ast_of ~loc (jtyp, attrs) =
    match jtyp with
    | Jtyp_local x -> Local.type_of ~loc ~attrs x
    | Jtyp_layout x -> Layouts.type_of ~loc ~attrs x
    | Jtyp_tuple x -> Labeled_tuples.typ_of ~loc ~attrs x
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
    | Jexp_layout of Layouts.expression
    | Jexp_tuple of Labeled_tuples.expression

  let of_ast_internal (feat : Feature.t) expr = match feat with
    | Language_extension Local ->
      Some (Jexp_local (Local.of_expr expr))
    | Language_extension Comprehensions ->
      Some (Jexp_comprehension (Comprehensions.of_expr expr))
    | Language_extension Immutable_arrays ->
      Some (Jexp_immutable_array (Immutable_arrays.of_expr expr))
    | Language_extension Layouts -> Some (Jexp_layout (Layouts.of_expr expr))
    | Language_extension Labeled_tuples ->
      Some (Jexp_tuple (Labeled_tuples.of_expr expr))
    | _ -> None

  let of_ast = Expression.make_of_ast ~of_ast_internal

  let ast_of ~loc (jexp, attrs) = match jexp with
    | Jexp_local            x -> Local.expr_of             ~loc ~attrs x
    | Jexp_comprehension    x -> Comprehensions.expr_of    ~loc ~attrs x
    | Jexp_immutable_array  x -> Immutable_arrays.expr_of  ~loc ~attrs x
    | Jexp_layout x -> Layouts.expr_of ~loc ~attrs x
    | Jexp_tuple x -> Labeled_tuples.expr_of ~loc ~attrs x
end

module Pattern = struct
  type t =
    | Jpat_local           of Local.pattern
    | Jpat_immutable_array of Immutable_arrays.pattern
    | Jpat_layout of Layouts.pattern
    | Jpat_tuple of Labeled_tuples.pattern

  let of_ast_internal (feat : Feature.t) pat = match feat with
    | Language_extension Local ->
      Some (Jpat_local (Local.of_pat pat))
    | Language_extension Immutable_arrays ->
      Some (Jpat_immutable_array (Immutable_arrays.of_pat pat))
    | Language_extension Layouts ->
      Some (Jpat_layout (Layouts.of_pat pat))
    | Language_extension Labeled_tuples ->
      Some (Jpat_tuple (Labeled_tuples.of_pat pat))
    | _ -> None

  let of_ast = Pattern.make_of_ast ~of_ast_internal

  let ast_of ~loc (jpat, attrs) = match jpat with
    | Jpat_local x -> Local.pat_of ~loc ~attrs x
    | Jpat_immutable_array x -> Immutable_arrays.pat_of ~loc ~attrs x
    | Jpat_layout x -> Layouts.pat_of ~loc ~attrs x
    | Jpat_tuple x -> Labeled_tuples.pat_of ~loc ~attrs x
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
  type t =
    | Jext_layout of Layouts.extension_constructor

  let of_ast_internal (feat : Feature.t) ext = match feat with
    | Language_extension Layouts ->
      Some (Jext_layout (Layouts.of_extension_constructor ext))
    | _ -> None

  let of_ast = Extension_constructor.make_of_ast ~of_ast_internal

  let ast_of ~loc:_ = assert false

  let extension_constructor_of ~loc ~name ~attrs ?info ?docs t =
    let ext_ctor =
      match t with
      | Jext_layout lext ->
          Layouts.extension_constructor_of ~loc ~name ~attrs ?info ?docs lext
    in
    ext_ctor
end
