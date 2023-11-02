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

  type 'a option_decl =
    names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  val select
    :  (* The fsevents context *)
       env
    -> (* Additional file descriptor to select for reading *)
       ?read_fdl:fd_select list
    -> (* Additional file descriptor to select for writing *)
       ?write_fdl:fd_select list
    -> (* Timeout...like Unix.select *)
       timeout:float
    -> (* The callback for file system events *)
       (event list -> unit)
    -> unit

  val f
    :  x:t
         (** an extremely long comment about [x] that does not fit on the
             same line with [x] *)
    -> unit

  val f
    :  fooooooooooooooooo:
         (fooooooooooooooo
          -> fooooooooooooooooooo
          -> foooooooooooooo
          -> foooooooooooooo * fooooooooooooooooo
          -> foooooooooooooooo )
         (** an extremely long comment about [x] that does not fit on the
             same line with [x] *)
    -> unit
end

let ssmap
    : (module MapT
         with type key = string
          and type data = string
          and type map = SSMap.map )
  =
  ()

let ssmap
    :  (module MapT
          with type key = string
           and type data = string
           and type map = SSMap.map )
    -> unit
  =
  ()

let long_function_name
    : type a. a long_long_type -> a -> a -> a -> wrap_wrap_wrap -> unit
  =
 fun () -> ()

let add_edge target dep =
  if target <> dep then (
    Hashtbl.replace edges dep
      (target :: (try Hashtbl.find edges dep with Not_found -> [])) ;
    Hashtbl.replace edge_count target
      (1 + try Hashtbl.find edge_count target with Not_found -> 0) ;
    if not (Hashtbl.mem edge_count dep) then Hashtbl.add edge_count dep 0 )

let iarray_fold_transf (f : numbering -> 'a -> numbering * 'b) n
    (a : 'a iarray)
    : numbering * 'b iarray
  =
  match Iarray.length a with 0 -> (n, [::]) | 1 -> x
