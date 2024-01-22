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

    external isatty : out_channel -> bool = "caml_sys_isatty"

    (* reasonable heuristic on whether colors should be enabled *)
    let should_enable_color () =
      let term = try Sys.getenv "TERM" with Not_found -> "" in
      term <> "dumb"
      && term <> ""
      && isatty stderr

    let default_setting = Auto
    let enabled = ref true
  end

  module Error_style = struct
    include Error_style

    let default_setting = Contextual
  end

  module Style = struct
    (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
    type color =
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White

    type style =
      | FG of color (* foreground *)
      | BG of color (* background *)
      | Bold
      | Reset

    let ansi_of_color = function
      | Black -> "0"
      | Red -> "1"
      | Green -> "2"
      | Yellow -> "3"
      | Blue -> "4"
      | Magenta -> "5"
      | Cyan -> "6"
      | White -> "7"

    let code_of_style = function
      | FG c -> "3" ^ ansi_of_color c
      | BG c -> "4" ^ ansi_of_color c
      | Bold -> "1"
      | Reset -> "0"

    let ansi_of_style_l l =
      let s = match l with
        | [] -> code_of_style Reset
        | [s] -> code_of_style s
        | _ -> String.concat ";" (List.map code_of_style l)
      in
      "\x1b[" ^ s ^ "m"

    type Format.stag += Style of style list

    type tag_style ={
      ansi: style list;
      text_open:string;
      text_close:string
    }

    type styles = {
      error: tag_style;
      warning: tag_style;
      loc: tag_style;
      hint: tag_style;
      inline_code: tag_style;
    }

    let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

    let default_styles = {
        warning = no_markup [Bold; FG Magenta];
        error = no_markup [Bold; FG Red];
        loc = no_markup [Bold];
        hint = no_markup [Bold; FG Blue];
        inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
      }

    let cur_styles = ref default_styles

    (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
    let style_of_tag s = match s with
      | Format.String_tag "error" ->  (!cur_styles).error
      | Format.String_tag "warning" ->(!cur_styles).warning
      | Format.String_tag "loc" -> (!cur_styles).loc
      | Format.String_tag "hint" -> (!cur_styles).hint
      | Format.String_tag "inline_code" -> (!cur_styles).inline_code
      | Style s -> no_markup s
      | _ -> raise Not_found

    let as_inline_code printer ppf x =
      Format.pp_open_stag ppf (Format.String_tag "inline_code");
      printer ppf x;
      Format.pp_close_stag ppf ()

    let inline_code ppf s = as_inline_code Format.pp_print_string ppf s

    (* either prints the tag of [s] or delegates to [or_else] *)
    let mark_open_tag ~or_else s =
      try
        let style = style_of_tag s in
        if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
      with Not_found -> or_else s

    let mark_close_tag ~or_else s =
      try
        let style = style_of_tag s in
        if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
      with Not_found -> or_else s

    (* add tag handling to formatter [ppf] *)
    let set_tag_handling ppf =
      let open Format in
      let functions = pp_get_formatter_stag_functions ppf () in
      let functions' = {functions with
        mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
        mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
      } in
      pp_set_mark_tags ppf true; (* enable tags *)
      pp_set_formatter_stag_functions ppf functions';
      ()

    let setup =
      let first = ref true in (* initialize only once *)
      let formatter_l =
        [Format.std_formatter; Format.err_formatter; Format.str_formatter]
      in
      let enable_color = function
        | Color.Auto -> Color.should_enable_color ()
        | Color.Always -> true
        | Color.Never -> false
      in
      fun o ->
        if !first then (
          first := false;
          Format.set_mark_tags true;
          List.iter set_tag_handling formatter_l;
          Color.enabled := (match o with
            | Some s -> enable_color s
            | None -> enable_color Color.default_setting)
        );
        ()
  end
end

module Clflags = struct
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
