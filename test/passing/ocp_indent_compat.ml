[@@@ocamlformat "ocp-indent-compat=true"]

(* Bad: unboxing the function type *)
external i : (int -> float[@unboxed]) = "i" "i_nat"

module type M = sig
  val action : action
  (** Formatting action: input type and source, and output destination. *)

  val doc_atrs
    :  (string Location.loc * payload) list
      -> (string Location.loc * bool) list option
         * (string Location.loc * payload) list

  val transl_modtype_longident
    (* from Typemod *)
    : (Location.t -> Env.t -> Longident.t -> Path.t) ref

  val transl_modtype_longident
    (* foooooooooo fooooooooooooo foooooooooooo foooooooooooooo
       foooooooooooooo foooooooooooo *)
    : (Location.t -> Env.t -> Longident.t -> Path.t) ref

  val imported_sets_of_closures_table
    : Simple_value_approx.function_declarations option
      Set_of_closures_id.Tbl.t
end

[@@@ocamlformat "ocp-indent-compat=false"]

module type M = sig
  val transl_modtype_longident
    (* from Typemod *) :
    (Location.t -> Env.t -> Longident.t -> Path.t) ref

  val transl_modtype_longident
    (* foooooooooo fooooooooooooo foooooooooooo foooooooooooooo
       foooooooooooooo foooooooooooo *) :
    (Location.t -> Env.t -> Longident.t -> Path.t) ref

  val imported_sets_of_closures_table :
    Simple_value_approx.function_declarations option
    Set_of_closures_id.Tbl.t
end

let array_fold_transf (f : numbering -> 'a -> numbering * 'b) n
    (a : 'a array) : numbering * 'b array =
  match Array.length a with 0 -> (n, [||]) | 1 -> x

let to_clambda_function (id, (function_decl : Flambda.function_declaration))
    : Clambda.ufunction =
  (* All that we need in the environment, for translating one closure from a
     closed set of closures, is the substitutions for variables bound to the
     various closures in the set. Such closures will always be ... *)
  x
