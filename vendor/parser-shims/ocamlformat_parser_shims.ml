(* Stdlib shims are separated because they are also used by the compiler libs
   shims. *)
include Ocamlformat_stdlib_shims

module Misc = Misc_

module Clflags : sig
  val include_dirs : string list ref
  val hidden_include_dirs : string list ref
  val debug : bool ref
  val unsafe : bool ref
  val open_modules : string list ref
  val absname : bool ref
  val use_threads : bool ref
  val principal : bool ref
  val recursive_types : bool ref
  val applicative_functors : bool ref
  val for_package : string option ref
  val transparent_modules : bool ref
  val locations : bool ref
  val color : Misc.Color.setting option ref
  val error_style : Misc.Error_style.setting option ref
  val unboxed_types : bool ref
  val no_std_include : bool ref
end = struct
  let include_dirs = ref ([] : string list)(* -I *)
  let hidden_include_dirs = ref ([] : string list)
  let debug = ref false                   (* -g *)
  let unsafe = ref false                  (* -unsafe *)
  let absname = ref false                 (* -absname *)
  let use_threads = ref false             (* -thread *)
  let open_modules = ref []               (* -open *)
  let principal = ref false               (* -principal *)
  let recursive_types = ref false         (* -rectypes *)
  let applicative_functors = ref true     (* -no-app-funct *)
  let for_package = ref (None: string option) (* -for-pack *)
  let transparent_modules = ref false     (* -trans-mod *)
  let locations = ref true                (* -d(no-)locations *)
  let color = ref None                    (* -color *)
  let error_style = ref None              (* -error-style *)
  let unboxed_types = ref false
  let no_std_include = ref false
end

module Load_path = struct
  type dir

  type auto_include_callback =
    (dir -> string -> string option) -> string -> string

  type paths = {visible: string list; hidden: string list}

  let get_paths () = {visible= []; hidden= []}

  let init ~auto_include:_ ~visible:_ ~hidden:_ = ()

  let auto_include_otherlibs _ _ s = s
end

module Builtin_attributes = struct
  type current_phase = Parser | Invariant_check

  let register_attr _ _ = ()

  let mark_payload_attrs_used _ = ()
end

module Format_doc = Format_doc
