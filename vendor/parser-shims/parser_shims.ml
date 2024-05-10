module List = struct
  include List

  let rec find_map f = function
    | [] -> None
    | x :: l ->
        begin match f x with
        | Some _ as result -> result
        | None -> find_map f l
        end
end

module Int = struct
  include Int

  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
end

module Misc = struct
  include Misc

  module Color = struct
    include Color

    let default_setting = Auto
  end

  module Error_style = struct
    include Error_style

    let default_setting = Contextual
  end

  type (_, _) eq = Refl : ('a, 'a) eq
end

module Clflags = struct
  let include_dirs = ref ([] : string list)(* -I *)
  let hidden_include_dirs = ref ([] : string list) (* -H *)
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
  let no_auto_include_otherlibs = ref false      (* -no-auto-include-otherlibs *)
end

module Load_path = struct
  type dir
  type auto_include_callback =
    (dir -> string -> string option) -> string -> string
  type paths =
    { visible : string list;
      hidden : string list }
  let init ~auto_include:_ ~visible:_ ~hidden:_ = ()
  let get_paths () = { visible = []; hidden = [] }
  let auto_include_otherlibs _ _ s = s
end
