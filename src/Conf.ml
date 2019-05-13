(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** Configuration options *)

type t =
  { assignment_operator: [`Begin_line | `End_line]
  ; break_cases: [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; break_infix_before_func: bool
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_separators: [`Before | `After | `After_and_docked]
  ; break_sequences: bool
  ; break_string_literals: [`Newlines | `Never | `Wrap]
  ; break_struct: bool
  ; cases_exp_indent: int
  ; comment_check: bool
  ; disable: bool
  ; doc_comments: [`Before | `After]
  ; doc_comments_padding: int
  ; doc_comments_tag_only: [`Fit | `Default]
  ; escape_chars: [`Decimal | `Hexadecimal | `Preserve]
  ; escape_strings: [`Decimal | `Hexadecimal | `Preserve]
  ; extension_sugar: [`Preserve | `Always]
  ; field_space: [`Tight | `Loose]
  ; if_then_else: [`Compact | `Fit_or_vertical | `Keyword_first | `K_R]
  ; indent_after_in: int
  ; indicate_multiline_delimiters: bool
  ; indicate_nested_or_patterns: [`Space | `Unsafe_no]
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_indent: int
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_module: [`Compact | `Sparse]
  ; let_open: [`Preserve | `Auto | `Short | `Long]
  ; margin: int
  ; max_iters: int
  ; module_item_spacing: [`Compact | `Preserve | `Sparse]
  ; ocp_indent_compat: bool
  ; parens_ite: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; parens_tuple_patterns: [`Always | `Multi_line_only]
  ; parse_docstrings: bool
  ; quiet: bool
  ; sequence_blank_line: [`Compact | `Preserve_one]
  ; sequence_style: [`Separator | `Terminator]
  ; single_case: [`Compact | `Sparse]
  ; space_around_collection_expressions: bool
  ; type_decl: [`Compact | `Sparse]
  ; type_decl_indent: int
  ; wrap_comments: bool
  ; wrap_fun_args: bool }

module Fpath = struct
  include Fpath

  let cwd () = Unix.getcwd () |> v

  let exists p = to_string p |> Caml.Sys.file_exists

  let to_absolute file = if is_rel file then append (cwd ()) file else file

  let to_string ?(pretty = false) p =
    if pretty then
      Option.value_map
        (relativize ~root:(cwd ()) p)
        ~default:(to_string p) ~f:to_string
    else to_string p

  let pp ?(pretty = false) fmt p =
    Format.fprintf fmt "%s" (to_string ~pretty p)
end

(** Extension of Cmdliner supporting lighter-weight option definition *)
module Cmdliner : sig
  include module type of Cmdliner

  val mk : default:'a -> 'a Term.t -> 'a ref
  (** [mk ~default term] is a ref which, after [parse] is called, contains
      the value of the command line option specified by [term]. *)

  val parse : Term.info -> (unit -> unit Term.ret) -> unit
  (** [parse info validate] parses the command line according to the options
      declared by calls to [mk], using manual and version [info], and
      calling [validate] to check usage constraints not expressible in the
      [Term] language. *)
end = struct
  include Cmdliner

  (** existential package of a Term and a setter for a ref to receive the
      parsed value *)
  type arg = Arg : 'a Term.t * ('a -> unit) -> arg

  (** convert a list of arg packages to a term for the tuple of all the arg
      terms, and apply it to a function that sets all the receiver refs *)
  let tuple args =
    let pair (Arg (trm_x, set_x)) (Arg (trm_y, set_y)) =
      let trm_xy = Term.(const (fun a b -> (a, b)) $ trm_x $ trm_y) in
      let set_xy (a, b) = set_x a ; set_y b in
      Arg (trm_xy, set_xy)
    in
    let init = Arg (Term.const (), fun () -> ()) in
    let (Arg (trm, set)) = List.fold_right ~f:pair args ~init in
    Term.app (Term.const set) trm

  let args : arg list ref = ref []

  let mk ~default arg =
    let var = ref default in
    let set x = var := x in
    args := Arg (arg, set) :: !args ;
    var

  let parse info validate =
    match Term.eval (Term.(ret (const validate $ tuple !args)), info) with
    | `Ok () -> ()
    | `Error _ -> Caml.exit 1
    | `Help | `Version -> Caml.exit 0
end

let profile_option_names = ["p"; "profile"]

open Cmdliner

module C : sig
  type config = t

  type 'a t

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  val section_name : [`Formatting | `Operational] -> string

  val choice :
    ?has_default:bool -> all:(string * 'a * string) list -> 'a option_decl

  val flag : default:bool -> bool option_decl

  val int : default:int -> docv:string -> int option_decl

  val default : 'a t -> 'a

  val update_using_cmdline : config -> config

  val update :
       config:config
    -> from:updated_from
    -> name:string
    -> value:string
    -> inline:bool
    -> ( config
       , [ `Unknown of string * string
         | `Bad_value of string * string
         | `Malformed of string
         | `Misplaced of string * string ] )
       Result.t

  val print_config : config -> unit
end = struct
  type config = t

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type from =
    [`Default | `Profile of string * updated_from | `Updated of updated_from]

  type 'a t =
    { names: string list
    ; parse: string -> ('a, string) Result.t
    ; update: config -> 'a -> config
    ; allow_inline: bool
    ; cmdline_get: unit -> 'a option
    ; to_string: 'a -> string
    ; default: 'a
    ; get_value: config -> 'a
    ; from: from }

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  type pack = Pack : 'a t -> pack

  let store = ref []

  let in_attributes ~section cond =
    if cond || Poly.(section = `Operational) then ""
    else " Cannot be set in attributes."

  let generated_choice_doc ~allow_inline ~all ~doc ~section ~has_default =
    let open Format in
    let default =
      if has_default then
        asprintf "The default value is $(b,%a)."
          (fun fs (v, _, _) -> fprintf fs "%s" v)
          (List.hd_exn all)
      else ""
    in
    asprintf "%s %a %s%s" doc
      (pp_print_list
         ~pp_sep:(fun fs () -> fprintf fs "@,")
         (fun fs (_, _, d) -> fprintf fs "%s" d))
      all default
      (in_attributes ~section allow_inline)

  let generated_choice_docv ~all =
    let open Format in
    asprintf "@[<1>{%a}@]"
      (pp_print_list
         ~pp_sep:(fun fs () -> fprintf fs "@,|")
         (fun fs (v, _, _) -> fprintf fs "%s" v))
      all

  let generated_flag_doc ~allow_inline ~doc ~section =
    Format.sprintf "%s%s" doc (in_attributes ~section allow_inline)

  let generated_int_doc ~allow_inline ~doc ~section ~default =
    let default = Format.sprintf "The default value is $(b,%i)." default in
    Format.sprintf "%s %s%s" doc default
      (in_attributes ~section allow_inline)

  let section_name = function
    | `Formatting -> Cmdliner.Manpage.s_options ^ " (CODE FORMATTING STYLE)"
    | `Operational -> Cmdliner.Manpage.s_options

  let from = `Default

  let choice ?(has_default = true) ~all ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) update get_value =
    let _, default, _ = List.hd_exn all in
    let doc =
      generated_choice_doc ~allow_inline ~all ~doc ~section ~has_default
    in
    let docv = generated_choice_docv ~all in
    let opt_names = List.map all ~f:(fun (x, y, _) -> (x, y)) in
    let to_string v' =
      List.find_map_exn all ~f:(fun (str, v, _) ->
          if Poly.equal v v' then Some str else None)
    in
    let docs = section_name section in
    let term =
      Arg.(
        value
        & opt (some (enum opt_names)) None
        & info names ~doc ~docv ~docs)
    in
    let parse s =
      match
        List.find_map all ~f:(fun (n, v, _) ->
            Option.some_if (String.equal n s) v)
      with
      | Some v -> Ok v
      | None ->
          Error
            (Printf.sprintf "Invalid value '%s', expecting %s" s
               ( List.map all ~f:(fun (s, _, _) -> Format.sprintf "'%s'" s)
               |> String.concat ~sep:" or " ))
    in
    let r = mk ~default:None term in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from }
    in
    store := Pack opt :: !store ;
    opt

  let flag ~default ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) update get_value =
    let open Cmdliner in
    let invert_flag = default in
    let names_for_cmdline =
      if invert_flag then
        List.filter_map names ~f:(fun n ->
            if String.length n = 1 then None else Some ("no-" ^ n))
      else names
    in
    let doc = generated_flag_doc ~allow_inline ~doc ~section in
    let docs = section_name section in
    let term = Arg.(value & flag & info names_for_cmdline ~doc ~docs) in
    let parse s =
      try Ok (Bool.of_string s)
      with _ ->
        Error
          (Format.sprintf "invalid value '%s', expecting 'true' or 'false'"
             s)
    in
    let r = mk ~default term in
    let to_string = Bool.to_string in
    let cmdline_get () = if !r then Some (not invert_flag) else None in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from }
    in
    store := Pack opt :: !store ;
    opt

  let int ~default ~docv ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) update get_value =
    let open Cmdliner in
    let doc = generated_int_doc ~allow_inline ~doc ~section ~default in
    let docs = section_name section in
    let term =
      Arg.(value & opt (some int) None & info names ~doc ~docs ~docv)
    in
    let parse s =
      try Ok (Int.of_string s)
      with _ ->
        Error (Format.sprintf "invalid value '%s', expecting an integer" s)
    in
    let r = mk ~default:None term in
    let to_string = Int.to_string in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from }
    in
    store := Pack opt :: !store ;
    opt

  let update_from config name from =
    let is_profile_option_name x =
      List.exists profile_option_names ~f:(String.equal x)
    in
    let on_pack (Pack {names; get_value; to_string}) =
      if is_profile_option_name (List.hd_exn names) then
        Some (to_string (get_value config))
      else None
    in
    let on_pack (Pack ({names} as p)) =
      if is_profile_option_name name then
        if is_profile_option_name (List.hd_exn names) then
          (* updating --profile option *)
          Pack {p with from= `Updated from}
        else
          let profile_name = List.find_map_exn !store ~f:on_pack in
          (* updating other options when --profile is set *)
          Pack {p with from= `Profile (profile_name, from)}
      else if List.exists names ~f:(String.equal name) then
        (* updating a single option (without setting a profile) *)
        Pack {p with from= `Updated from}
      else Pack p
    in
    store := List.map !store ~f:on_pack

  let update ~config ~from ~name ~value ~inline =
    List.find_map !store
      ~f:(fun (Pack {names; parse; update; allow_inline}) ->
        if List.exists names ~f:(String.equal name) then
          if inline && not allow_inline then
            Some (Error (`Misplaced (name, value)))
          else
            match parse value with
            | Ok packed_value ->
                let config = update config packed_value in
                update_from config name from ;
                Some (Ok config)
            | Error error -> Some (Error (`Bad_value (name, error)))
        else None)
    |> Option.value ~default:(Error (`Unknown (name, value)))

  let default {default} = default

  let update_using_cmdline config =
    let on_pack config (Pack {cmdline_get; update; names}) =
      match cmdline_get () with
      | None -> config
      | Some x ->
          let config = update config x in
          update_from config (List.hd_exn names) `Commandline ;
          config
    in
    List.fold !store ~init:config ~f:on_pack

  let print_config c =
    let longest =
      let compare x y = compare (String.length x) (String.length y) in
      List.max_elt ~compare
    in
    let on_pack (Pack {names; to_string; get_value; from}) =
      let name = Option.value_exn (longest names) in
      let value = to_string (get_value c) in
      let aux_from = function
        | `Parsed (`File (p, i)) ->
            Format.sprintf " (file %s:%i)"
              (Fpath.to_string ~pretty:true p)
              i
        | `Parsed `Attribute -> " (attribute)"
        | `Env -> " (environment variable)"
        | `Commandline -> " (command line)"
      in
      let aux_from = function
        | `Default -> ""
        | `Profile (s, p) -> " (profile " ^ s ^ aux_from p ^ ")"
        | `Updated x -> aux_from x
      in
      Format.eprintf "%s=%s%s\n%!" name value (aux_from from)
    in
    List.iter !store ~f:on_pack
end

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "$(tname) automatically formats OCaml code."
    ; `S (C.section_name `Formatting)
    ; `P
        "Unless otherwise noted, any option \
         $(b,--)$(i,option)$(b,=)$(i,VAL) detailed in this section can be \
         set in many ways, its value is determined in the following order \
         (of increasing priority): the default value is used if no other \
         value is specified, the value can be modified in an \
         $(b,.ocamlformat) configuration file with an '$(b,option = \
         )$(i,VAL)' line (*), or using the OCAMLFORMAT environment \
         variable: \
         $(b,OCAMLFORMAT=)$(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL), \
         or as an optional parameter on the command line, or with a global \
         $(b,[@@@ocamlformat \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) \
         attribute in the processed file, or with an $(b,[@@ocamlformat \
         \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute on expression in \
         the processed file."
    ; `P
        "(*) $(b,.ocamlformat) files in current and all ancestor \
         directories for each input file are used, as well as the global \
         $(b,ocamlformat) file defined in $(b,\\$XDG_CONFIG_HOME) or in \
         $(b,\\$HOME/.config) if $(b,\\$XDG_CONFIG_HOME) is undefined. The \
         global $(b,ocamlformat) file has the lowest priority, then the \
         closer the directory is to the processed file, the higher the \
         priority."
    ; `P
        "An $(b,.ocamlformat-ignore) file specifies files that OCamlFormat \
         should ignore. Each line in an $(b,.ocamlformat-ignore) file \
         specifies a filename relative to the directory containing the \
         $(b,.ocamlformat-ignore) file. Lines starting with $(b,#) are \
         ignored and can be used as comments." ]
  in
  Term.info "ocamlformat" ~version:Version.version ~doc ~man

(** Options affecting formatting *)
module Formatting = struct
  let section = `Formatting

  let assignment_operator =
    let doc = "Position of the assignment operator." in
    let names = ["assignment-operator"] in
    let all =
      [ ( "end-line"
        , `End_line
        , "$(b,end-line) positions assignment operators (`:=` and `<-`) at \
           the end of the line and breaks after it if the whole assignment \
           expression does not fit on a single line." )
      ; ( "begin-line"
        , `Begin_line
        , "$(b,begin-line) positions assignment operators (`:=` and `<-`) \
           at the beginning of the line and breaks before it if the whole \
           assignment expression does not fit on a single line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with assignment_operator= x})
      (fun conf -> conf.assignment_operator)

  let break_cases =
    let doc = "Break pattern match cases." in
    let names = ["break-cases"] in
    let all =
      [ ( "nested"
        , `Nested
        , "$(b,nested) forces a break after nested or-patterns to \
           highlight the case body. Note that with $(b,nested), the \
           $(b,indicate-nested-or-patterns) option is not needed, and so \
           ignored." )
      ; ( "fit"
        , `Fit
        , "Specifying $(b,fit) lets pattern matches break at the margin \
           naturally." )
      ; ( "toplevel"
        , `Toplevel
        , "$(b,toplevel) forces top-level cases (i.e. not nested \
           or-patterns) to break across lines, otherwise break naturally \
           at the margin." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) tries to fit all or-patterns on the same \
           line, otherwise breaks." )
      ; ( "all"
        , `All
        , "$(b,all) forces all pattern matches to break across lines." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_cases= x})
      (fun conf -> conf.break_cases)

  let break_collection_expressions =
    let doc =
      "Break collection expressions (lists and arrays) elements by elements."
    in
    let names = ["break-collection-expressions"] in
    let all =
      [ ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line." )
      ; ( "wrap"
        , `Wrap
        , "$(b,wrap) will group simple expressions and try to format them \
           in a single line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_collection_expressions= x})
      (fun conf -> conf.break_collection_expressions)

  let break_fun_decl =
    let doc = "Style for function declarations and types." in
    let names = ["break-fun-decl"] in
    let all =
      [ ("wrap", `Wrap, "$(b,wrap) breaks only if necessary.")
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line." )
      ; ( "smart"
        , `Smart
        , "$(b,smart) is like $(b,fit-or-vertical) but try to fit \
           arguments on their line if they fit." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_fun_decl= x})
      (fun conf -> conf.break_fun_decl)

  let break_fun_sig =
    let doc = "Style for function signatures." in
    let names = ["break-fun-sig"] in
    let all =
      [ ("wrap", `Wrap, "$(b,wrap) breaks only if necessary.")
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line." )
      ; ( "smart"
        , `Smart
        , "$(b,smart) is like $(b,fit-or-vertical) but try to fit \
           arguments on their line if they fit." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_fun_sig= x})
      (fun conf -> conf.break_fun_sig)

  let break_infix =
    let doc = "Break sequence of infix operators." in
    let names = ["break-infix"] in
    let all =
      [ ( "wrap"
        , `Wrap
        , "$(b,wrap) will group simple expressions and try to format them \
           in a single line." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_infix= x})
      (fun conf -> conf.break_infix)

  let break_infix_before_func =
    let doc =
      "Break infix operators whose right arguments are anonymous functions \
       specially: do not break after the operator so that the first line \
       of the function appears docked at the end of line after the \
       operator."
    in
    let names = ["break-infix-before-func"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with break_infix_before_func= x})
      (fun conf -> conf.break_infix_before_func)

  let break_separators =
    let doc =
      "Break before or after separators such as `;` in list or record \
       expressions."
    in
    let names = ["break-separators"] in
    let all =
      [ ( "before"
        , `Before
        , "$(b,before) breaks the expressions before the separator." )
      ; ( "after"
        , `After
        , "$(b,after) breaks the expressions after the separator." )
      ; ( "after-and-docked"
        , `After_and_docked
        , "$(b,after-and-docked) breaks the expressions after the \
           separator and docks the brackets for records." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_separators= x})
      (fun conf -> conf.break_separators)

  let break_sequences =
    let doc =
      "Force sequence expressions to break irrespective of margin."
    in
    let names = ["break-sequences"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with break_sequences= x})
      (fun conf -> conf.break_sequences)

  let break_string_literals =
    let doc = "Break string literals." in
    let names = ["break-string-literals"] in
    let all =
      [ ( "wrap"
        , `Wrap
        , "$(b,wrap) mode wraps string literals at the margin. Quoted \
           strings such as $(i,{id|...|id}) are preserved." )
      ; ( "newlines"
        , `Newlines
        , "$(b,newlines) mode breaks lines at newlines." )
      ; ( "never"
        , `Never
        , "$(b,never) mode formats string literals as they are parsed, in \
           particular, with escape sequences expanded." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_string_literals= x})
      (fun conf -> conf.break_string_literals)

  let break_struct =
    let doc = "Break struct-end module items." in
    let names = ["break-struct"] in
    let all =
      [ ( "force"
        , `Force
        , "$(b,force) will break struct-end phrases unconditionally." )
      ; ( "natural"
        , `Natural
        , "$(b,natural) will break struct-end phrases naturally at the \
           margin." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_struct= Poly.(x = `Force)})
      (fun conf -> if conf.break_struct then `Force else `Natural)

  let cases_exp_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of cases expressions ($(docv) columns), except for \
       nested `match` or `try` expressions."
    in
    let names = ["cases-exp-indent"] in
    C.int ~names ~default:4 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with cases_exp_indent= x})
      (fun conf -> conf.cases_exp_indent)

  let disable =
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(b,[@@@ocamlformat \
       \"enable\"]) instead of $(b,[@@@ocamlformat \"disable=false\"])"
    in
    C.flag ~names:["disable"] ~default:false ~doc ~section
      (fun conf x -> {conf with disable= x})
      (fun conf -> conf.disable)

  let doc_comments =
    let doc = "Doc comments position." in
    let names = ["doc-comments"] in
    let all =
      [ ( "after"
        , `After
        , "$(b,after) puts doc comments after the corresponding code." )
      ; ( "before"
        , `Before
        , "$(b,before) puts comments before the corresponding code." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with doc_comments= x})
      (fun conf -> conf.doc_comments)

  let doc_comments_padding =
    let docv = "PADDING" in
    let doc =
      "Add $(docv) spaces before doc comments in type declarations."
    in
    let names = ["doc-comments-padding"] in
    C.int ~names ~default:2 ~doc ~docv ~section
      (fun conf x -> {conf with doc_comments_padding= x})
      (fun conf -> conf.doc_comments_padding)

  let doc_comments_tag_only =
    let doc = "Position of doc comments with only tags." in
    let names = ["doc-comments-tag-only"] in
    let all =
      [ ("default", `Default, "$(b,default) means no special treatment.")
      ; ("fit", `Fit, "$(b,fit) puts doc comments on the same line.") ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with doc_comments_tag_only= x})
      (fun conf -> conf.doc_comments_tag_only)

  let escape_chars =
    let doc = "Escape encoding for character literals." in
    let names = ["escape-chars"] in
    let all =
      [ ( "preserve"
        , `Preserve
        , "$(b,preserve) escapes ASCII control codes but leaves the upper \
           128 characters unchanged." )
      ; ( "decimal"
        , `Decimal
        , "$(b,decimal) produces ASCII printable characters using decimal \
           escape sequences as needed." )
      ; ( "hexadecimal"
        , `Hexadecimal
        , "$(b,hexadecimal) mode escapes every character." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with escape_chars= x})
      (fun conf -> conf.escape_chars)

  let escape_strings =
    let doc =
      "Escape encoding for string literals. See `--escape-chars` for the \
       interpretation of the modes."
    in
    let names = ["escape-strings"] in
    let all =
      [ ("preserve", `Preserve, "")
      ; ("decimal", `Decimal, "")
      ; ("hexadecimal", `Hexadecimal, "") ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with escape_strings= x})
      (fun conf -> conf.escape_strings)

  let extension_sugar =
    let doc = "Extension formatting." in
    let names = ["extension-sugar"] in
    let all = [("preserve", `Preserve, ""); ("always", `Always, "")] in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with extension_sugar= x})
      (fun conf -> conf.extension_sugar)

  let field_space =
    let doc =
      "Whether or not to use a space between a field name and the rhs. \
       This option affects records and objects."
    in
    let names = ["field-space"] in
    let all =
      [ ( "tight"
        , `Tight
        , "$(b,tight) does not use a space between a field name and the \
           punctuation symbol (`:` or `=`)." )
      ; ("loose", `Loose, "$(b,loose) does.") ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with field_space= x})
      (fun conf -> conf.field_space)

  let if_then_else =
    let doc = "If-then-else formatting." in
    let names = ["if-then-else"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) tries to format an if-then-else expression on a \
           single line." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks branches if they do not \
           fit on a single line." )
      ; ( "keyword-first"
        , `Keyword_first
        , "$(b,keyword-first) formats if-then-else expressions such that \
           the if-then-else keywords are the first on the line." )
      ; ( "k-r"
        , `K_R
        , "$(b,k-r) formats if-then-else expressions with parentheses that \
           match the K&R style." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with if_then_else= x})
      (fun conf -> conf.if_then_else)

  let indent_after_in =
    let docv = "COLS" in
    let doc =
      "Indentation ($(docv) columns) after `let ... in`, unless followed \
       by another `let`."
    in
    let names = ["indent-after-in"] in
    C.int ~names ~default:0 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with indent_after_in= x})
      (fun conf -> conf.indent_after_in)

  let indicate_multiline_delimiters =
    let doc =
      "Print a space inside a delimiter to indicate that its matching \
       delimiter is on a different line."
    in
    let names = ["indicate-multiline-delimiters"] in
    let default = true in
    C.flag ~names ~default ~doc ~section
      (fun conf x -> {conf with indicate_multiline_delimiters= x})
      (fun conf -> conf.indicate_multiline_delimiters)

  let indicate_nested_or_patterns =
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let names = ["indicate-nested-or-patterns"] in
    let all =
      [ ( "space"
        , `Space
        , "$(b,space) starts lines of nested or-patterns with \" |\" \
           rather than \"| \"." )
      ; ( "unsafe-no"
        , `Unsafe_no
        , "$(b,unsafe-no) does not indicate nested or-patterns. Warning: \
           this can produce confusing code where a short body of a match \
           case is visually hidden by surrounding long patterns, leading \
           to misassociation between patterns and body expressions." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with indicate_nested_or_patterns= x})
      (fun conf -> conf.indicate_nested_or_patterns)

  let infix_precedence =
    let doc =
      "Use indentation or also discretionary parentheses to explicitly \
       disambiguate precedences of infix operators."
    in
    let names = ["infix-precedence"] in
    let all =
      [ ( "indent"
        , `Indent
        , "$(b,indent) uses indentation to explicitly disambiguate \
           precedences of infix operators." )
      ; ( "parens"
        , `Parens
        , "$(b,parens) uses parentheses to explicitly disambiguate \
           precedences of infix operators." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with infix_precedence= x})
      (fun conf -> conf.infix_precedence)

  let leading_nested_match_parens =
    let doc = "Nested match parens formatting." in
    let names = ["leading-nested-match-parens"] in
    C.flag ~default:false ~names ~doc ~section ~allow_inline:false
      (fun conf x -> {conf with leading_nested_match_parens= x})
      (fun conf -> conf.leading_nested_match_parens)

  let let_and =
    let doc = "Style of let_and." in
    let names = ["let-and"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format `let p = e and p = e` in a \
           single line." )
      ; ("sparse", `Sparse, "$(b,sparse) will always break between them.")
      ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with let_and= x})
      (fun conf -> conf.let_and)

  let let_binding_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of let binding expressions ($(docv) columns) if they do \
       not fit on a single line."
    in
    let names = ["let-binding-indent"] in
    C.int ~names ~default:2 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with let_binding_indent= x})
      (fun conf -> conf.let_binding_indent)

  let let_binding_spacing =
    let doc = "Spacing between let binding." in
    let names = ["let-binding-spacing"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) spacing separates adjacent let bindings in a \
           module according to module-item-spacing." )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) places two open lines between a multi-line \
           module-level let binding and the next." )
      ; ( "double-semicolon"
        , `Double_semicolon
        , "$(b,double-semicolon) places double semicolons and an open line \
           between a multi-line module-level let binding and the next." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with let_binding_spacing= x})
      (fun conf -> conf.let_binding_spacing)

  let let_module =
    let doc = "Module binding formatting." in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) does not break a line after the $(i,let module ... \
           =) and before the $(i,in) if the module declaration does not \
           fit on a single line." )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) breaks a line after $(i,let module ... =) and \
           before the $(i,in) if the module declaration does not fit on a \
           single line." ) ]
    in
    C.choice ~names:["let-module"] ~all ~doc ~section
      (fun conf x -> {conf with let_module= x})
      (fun conf -> conf.let_module)

  let let_open =
    let doc = "Module open formatting." in
    let all =
      [ ("preserve", `Preserve, "$(b,preserve) keeps the original style.")
      ; ( "short"
        , `Short
        , "$(b,short) means the $(i,Module.(...)) style is used." )
      ; ( "long"
        , `Long
        , "$(b,long) means the $(i,let open Module in (...)) style is used."
        )
      ; ("auto", `Auto, "$(b,auto) means the one fitting best is used.") ]
    in
    C.choice ~names:["let-open"] ~all ~doc ~section
      (fun conf x -> {conf with let_open= x})
      (fun conf -> conf.let_open)

  let margin =
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    C.int ~names:["m"; "margin"] ~default:80 ~doc ~docv ~section
      ~allow_inline:false
      (fun conf x -> {conf with margin= x})
      (fun conf -> conf.margin)

  let module_item_spacing =
    let doc = "Spacing between items of structures and signatures." in
    let names = ["module-item-spacing"] in
    let all =
      [ ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break a line between two items." )
      ; ( "preserve"
        , `Preserve
        , "$(b,preserve) will not leave open lines between one-liners of \
           similar sorts unless there is an open line in the input." )
      ; ( "compact"
        , `Compact
        , "$(b,compact) will not leave open lines between one-liners of \
           similar sorts." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with module_item_spacing= x})
      (fun conf -> conf.module_item_spacing)

  let ocp_indent_compat =
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let names = ["ocp-indent-compat"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with ocp_indent_compat= x})
      (fun conf -> conf.ocp_indent_compat)

  let parens_ite =
    let doc =
      "Uses parentheses around if-then-else branches that spread across \
       multiple lines."
    in
    let names = ["parens-ite"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with parens_ite= x})
      (fun conf -> conf.parens_ite)

  let parens_tuple =
    let doc = "Parens tuple expressions." in
    let names = ["parens-tuple"] in
    let all =
      [ ( "always"
        , `Always
        , "$(b,always) always uses parentheses around tuples." )
      ; ( "multi-line-only"
        , `Multi_line_only
        , "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuples." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with parens_tuple= x})
      (fun conf -> conf.parens_tuple)

  let parens_tuple_patterns =
    let doc = "Parens tuple patterns." in
    let names = ["parens-tuple-patterns"] in
    let all =
      [ ( "multi-line-only"
        , `Multi_line_only
        , "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuple patterns." )
      ; ( "always"
        , `Always
        , "$(b,always) always uses parentheses around tuples patterns." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with parens_tuple_patterns= x})
      (fun conf -> conf.parens_tuple_patterns)

  let parse_docstrings =
    let doc = "Parse and format docstrings." in
    let names = ["parse-docstrings"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with parse_docstrings= x})
      (fun conf -> conf.parse_docstrings)

  let sequence_blank_line =
    let doc = "Blank line between expressions of a sequence." in
    let names = ["sequence-blank-line"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will not keep any blank line between expressions \
           of a sequence." )
      ; ( "preserve-one"
        , `Preserve_one
        , "$(b,preserve) will keep a blank line between two expressions of \
           a sequence if the input contains at least one." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with sequence_blank_line= x})
      (fun conf -> conf.sequence_blank_line)

  let sequence_style =
    let doc = "Style of sequence." in
    let names = ["sequence-style"] in
    let all =
      [ ( "separator"
        , `Separator
        , "$(b,separator) puts spaces before and after semicolons." )
      ; ( "terminator"
        , `Terminator
        , "$(b,terminator) only puts spaces after semicolons." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with sequence_style= x})
      (fun conf -> conf.sequence_style)

  let single_case =
    let doc =
      "Style of pattern matching expressions with only a single case."
    in
    let names = ["single-case"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format a single case on a single line."
        )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break the line before a single case." )
      ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with single_case= x})
      (fun conf -> conf.single_case)

  let space_around_collection_expressions =
    let doc =
      "Add a space inside the delimiters of collection expressions (lists, \
       arrays, records)."
    in
    let names = ["space-around-collection-expressions"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with space_around_collection_expressions= x})
      (fun conf -> conf.space_around_collection_expressions)

  let type_decl =
    let doc = "Style of type declaration." in
    let names = ["type-decl"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format constructors and records \
           definition in a single line." )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break between constructors and record \
           fields." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with type_decl= x})
      (fun conf -> conf.type_decl)

  let type_decl_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of type declarations ($(docv) columns) if they do not \
       fit on a single line."
    in
    let names = ["type-decl-indent"] in
    C.int ~names ~default:2 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with type_decl_indent= x})
      (fun conf -> conf.type_decl_indent)

  let wrap_comments =
    let doc =
      "Wrap comments and docstrings. Comments and docstrings are divided \
       into paragraphs by open lines (two or more consecutive newlines), \
       and each paragraph is wrapped at the margin. Multi-line comments \
       with vertically-aligned asterisks on the left margin are not \
       wrapped. Consecutive comments with both left and right margin \
       aligned are not wrapped either."
    in
    C.flag ~default:false ~names:["wrap-comments"] ~doc ~section
      (fun conf x -> {conf with wrap_comments= x})
      (fun conf -> conf.wrap_comments)

  let wrap_fun_args =
    let default = true in
    let doc = "Style for function call." in
    let names = ["wrap-fun-args"] in
    C.flag ~default ~names ~doc ~section
      (fun conf wrap_fun_args -> {conf with wrap_fun_args})
      (fun conf -> conf.wrap_fun_args)
end

(* Flags that can be modified in the config file that don't affect
   formatting *)

let project_root_witness = [".git"; ".hg"; "dune-project"]

let section = `Operational

let docs = C.section_name section

let comment_check =
  let default = true in
  let doc =
    "UNSAFE: Control whether to check comments and documentation comments. \
     May be set in $(b,.ocamlformat)."
  in
  C.flag ~default ~names:["comment-check"] ~doc ~section
    (fun conf x -> {conf with comment_check= x})
    (fun conf -> conf.comment_check)

let disable_conf_attrs =
  let doc = "Disable configuration in attributes." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-attrs"] ~doc ~docs)

let disable_conf_files =
  let doc = "Disable .ocamlformat configuration files." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-files"] ~doc ~docs)

let disable_outside_detected_project =
  let doc =
    Format.sprintf
      "$(b,Warning:) this option is $(b,deprecated) and will be removed in \
       OCamlFormat v1.0."
  in
  let default = false in
  mk ~default
    Arg.(
      value & flag & info ["disable-outside-detected-project"] ~doc ~docs)

let enable_outside_detected_project =
  let witness =
    String.concat ~sep:" or "
      (List.map project_root_witness ~f:(fun name ->
           Format.sprintf "$(b,%s)" name))
  in
  let doc =
    Format.sprintf
      "Read $(b,.ocamlformat) config files outside the current project. \
       The project root of an input file is taken to be the nearest \
       ancestor directory that contains a %s file. Formatting is enabled \
       even if no $(b,.ocamlformat) configuration file is found."
      witness
  in
  let default = false in
  mk ~default
    Arg.(value & flag & info ["enable-outside-detected-project"] ~doc ~docs)

let max_iters =
  let docv = "N" in
  let doc =
    "Fail if output of formatting does not stabilize within $(docv) \
     iterations. May be set in $(b,.ocamlformat)."
  in
  C.int ~names:["n"; "max-iters"] ~default:10 ~doc ~docv ~section
    (fun conf x -> {conf with max_iters= x})
    (fun conf -> conf.max_iters)

let quiet =
  let doc = "Quiet. May be set in $(b,.ocamlformat)." in
  C.flag ~default:false ~names:["q"; "quiet"] ~doc ~section
    (fun conf x -> {conf with quiet= x})
    (fun conf -> conf.quiet)

(* Other Flags *)

let check =
  let doc =
    "Check whether the input files already are formatted. Mutually \
     exclusive with --inplace and --output."
  in
  mk ~default:false Arg.(value & flag & info ["check"] ~doc ~docs)

let config =
  let doc =
    "Aggregate options. Options are specified as a comma-separated list of \
     pairs: \
     $(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL)."
  in
  let env = Arg.env_var "OCAMLFORMAT" in
  let default = [] in
  let assoc = Arg.(pair ~sep:'=' string string) in
  let list_assoc = Arg.(list ~sep:',' assoc) in
  mk ~default
    Arg.(
      value & opt list_assoc default & info ["c"; "config"] ~doc ~docs ~env)

let debug =
  let doc = "Generate debugging output." in
  let default = false in
  mk ~default Arg.(value & flag & info ["g"; "debug"] ~doc ~docs)

let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["i"; "inplace"] ~doc ~docs)

let inputs =
  let docv = "SRC" in
  let file_or_dash =
    let parse, print = Arg.non_dir_file in
    let parse = function "-" -> `Ok "-" | s -> parse s in
    (parse, print)
  in
  let doc =
    "Input files. At least one is required, and exactly one without \
     $(b,--inplace). If $(b,-) is passed, will read from stdin."
  in
  let default = [] in
  mk ~default
    Arg.(value & pos_all file_or_dash default & info [] ~doc ~docv ~docs)

let kind : [`Impl | `Intf | `Use_file] option ref =
  let doc =
    "Parse file with unrecognized extension as an implementation."
  in
  let impl = (Some `Impl, Arg.info ["impl"] ~doc ~docs) in
  let doc = "Parse file with unrecognized extension as an interface." in
  let intf = (Some `Intf, Arg.info ["intf"] ~doc ~docs) in
  let doc = "Parse file with unrecognized extension as a use_file." in
  let use_file = (Some `Use_file, Arg.info ["use-file"] ~doc ~docs) in
  let default = None in
  mk ~default Arg.(value & vflag default [impl; intf; use_file])

let name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting. Defaults to the input \
     file name. Some options can be specified in configuration files named \
     '.ocamlformat' in the same or a parent directory of $(docv), see \
     documentation of other options for details."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some string) default & info ["name"] ~doc ~docs ~docv)

let ocp_indent_options =
  [ ( "base"
    , Some
        ( "let-binding-indent"
        , "$(b,base) is an alias for $(b,let-binding-indent)."
        , Fn.id ) )
  ; ( "type"
    , Some
        ( "type-decl-indent"
        , "$(b,type) is an alias for $(b,type-decl-indent)."
        , Fn.id ) )
  ; ( "in"
    , Some
        ( "indent-after-in"
        , "$(b,in) is an alias for $(b,indent-after-in)."
        , Fn.id ) )
  ; ("with", None)
  ; ( "match_clause"
    , Some
        ( "cases-exp-indent"
        , "$(b,match_clause) is an alias for $(b,cases-exp-indent)."
        , Fn.id ) )
  ; ("ppx_stritem_ext", None)
  ; ("max_indent", None)
  ; ("strict_with", None)
  ; ("strict_else", None)
  ; ("strict_comments", None)
  ; ("align_ops", None)
  ; ("align_params", None) ]

let ocp_indent_config =
  let doc =
    let open Format in
    let supported =
      let l =
        List.filter_map ocp_indent_options ~f:(fun (_, o) ->
            Option.map o ~f:(fun (_, doc, _) -> doc))
      in
      if List.is_empty l then ""
      else
        asprintf " %a"
          (pp_print_list
             ~pp_sep:(fun fs () -> fprintf fs "@ ")
             (fun fs s -> fprintf fs "%s" s))
          l
    in
    asprintf "Read .ocp-indent configuration files.%s" supported
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["ocp-indent-config"] ~doc ~docs)

let output =
  let docv = "DST" in
  let doc =
    "Output file. Mutually exclusive with --inplace. Write to stdout if \
     omitted."
  in
  let default = None in
  mk ~default
    Arg.(
      value
      & opt (some string) default
      & info ["o"; "output"] ~doc ~docs ~docv)

let print_config =
  let doc =
    "Print the configuration determined by the environment variable, the \
     configuration files, preset profiles and command line. Attributes are \
     not considered. If many input files are specified, only print the \
     configuration for the first file. If no input file is specified, \
     print the configuration for the root directory if specified, or for \
     the current working directory otherwise."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["print-config"] ~doc ~docs)

let root =
  let docv = "DIR" in
  let doc =
    "Root of the project. If specified, only take into account \
     .ocamlformat configuration files inside $(docv) and its \
     subdirectories."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some dir) default & info ["root"] ~doc ~docs ~docv)

let no_version_check =
  let doc =
    "Do no check version matches the one specified in .ocamlformat."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-version-check"] ~doc ~docs)

let ocamlformat_profile =
  { assignment_operator= C.default Formatting.assignment_operator
  ; break_cases= C.default Formatting.break_cases
  ; break_collection_expressions=
      C.default Formatting.break_collection_expressions
  ; break_infix= C.default Formatting.break_infix
  ; break_infix_before_func= C.default Formatting.break_infix_before_func
  ; break_fun_decl= C.default Formatting.break_fun_decl
  ; break_fun_sig= C.default Formatting.break_fun_sig
  ; break_separators= C.default Formatting.break_separators
  ; break_sequences= C.default Formatting.break_sequences
  ; break_string_literals= C.default Formatting.break_string_literals
  ; break_struct= Poly.(C.default Formatting.break_struct = `Force)
  ; cases_exp_indent= C.default Formatting.cases_exp_indent
  ; comment_check= C.default comment_check
  ; disable= C.default Formatting.disable
  ; doc_comments= C.default Formatting.doc_comments
  ; doc_comments_padding= C.default Formatting.doc_comments_padding
  ; doc_comments_tag_only= C.default Formatting.doc_comments_tag_only
  ; escape_chars= C.default Formatting.escape_chars
  ; escape_strings= C.default Formatting.escape_strings
  ; extension_sugar= C.default Formatting.extension_sugar
  ; field_space= C.default Formatting.field_space
  ; if_then_else= C.default Formatting.if_then_else
  ; indent_after_in= C.default Formatting.indent_after_in
  ; indicate_multiline_delimiters=
      C.default Formatting.indicate_multiline_delimiters
  ; indicate_nested_or_patterns=
      C.default Formatting.indicate_nested_or_patterns
  ; infix_precedence= C.default Formatting.infix_precedence
  ; leading_nested_match_parens=
      C.default Formatting.leading_nested_match_parens
  ; let_and= C.default Formatting.let_and
  ; let_binding_indent= C.default Formatting.let_binding_indent
  ; let_binding_spacing= C.default Formatting.let_binding_spacing
  ; let_module= C.default Formatting.let_module
  ; let_open= C.default Formatting.let_open
  ; margin= C.default Formatting.margin
  ; max_iters= C.default max_iters
  ; module_item_spacing= C.default Formatting.module_item_spacing
  ; ocp_indent_compat= C.default Formatting.ocp_indent_compat
  ; parens_ite= C.default Formatting.parens_ite
  ; parens_tuple= C.default Formatting.parens_tuple
  ; parens_tuple_patterns= C.default Formatting.parens_tuple_patterns
  ; parse_docstrings= C.default Formatting.parse_docstrings
  ; quiet= C.default quiet
  ; sequence_blank_line= C.default Formatting.sequence_blank_line
  ; sequence_style= C.default Formatting.sequence_style
  ; single_case= C.default Formatting.single_case
  ; space_around_collection_expressions=
      C.default Formatting.space_around_collection_expressions
  ; type_decl= C.default Formatting.type_decl
  ; type_decl_indent= C.default Formatting.type_decl_indent
  ; wrap_comments= C.default Formatting.wrap_comments
  ; wrap_fun_args= C.default Formatting.wrap_fun_args }

let conventional_profile =
  { ocamlformat_profile with
    break_cases= `Fit
  ; break_infix_before_func= false
  ; break_separators= `After_and_docked
  ; break_sequences= true
  ; field_space= `Loose
  ; indicate_nested_or_patterns= `Unsafe_no
  ; sequence_style= `Terminator
  ; space_around_collection_expressions= true }

let compact_profile =
  { ocamlformat_profile with
    break_cases= `Fit
  ; break_collection_expressions= `Wrap
  ; break_infix= `Wrap
  ; break_fun_decl= `Wrap
  ; break_fun_sig= `Wrap
  ; break_sequences= false
  ; break_struct= false
  ; doc_comments_tag_only= `Fit
  ; field_space= `Tight
  ; if_then_else= `Compact
  ; indicate_nested_or_patterns= `Space
  ; leading_nested_match_parens= false
  ; let_and= `Compact
  ; let_binding_spacing= `Compact
  ; let_module= `Compact
  ; module_item_spacing= `Compact
  ; single_case= `Compact
  ; space_around_collection_expressions= false
  ; type_decl= `Compact
  ; wrap_fun_args= true }

let sparse_profile =
  { ocamlformat_profile with
    break_cases= `Nested
  ; break_collection_expressions= `Fit_or_vertical
  ; break_infix= `Fit_or_vertical
  ; break_fun_decl= `Smart
  ; break_fun_sig= `Smart
  ; break_sequences= true
  ; break_struct= true
  ; field_space= `Loose
  ; if_then_else= `Keyword_first
  ; indicate_nested_or_patterns= `Space
  ; leading_nested_match_parens= true
  ; let_and= `Sparse
  ; let_binding_spacing= `Sparse
  ; let_module= `Sparse
  ; module_item_spacing= `Sparse
  ; single_case= `Sparse
  ; space_around_collection_expressions= true
  ; type_decl= `Sparse
  ; wrap_fun_args= false }

let janestreet_profile =
  { assignment_operator= `Begin_line
  ; break_cases= `Fit_or_vertical
  ; break_collection_expressions=
      ocamlformat_profile.break_collection_expressions
  ; break_infix= `Fit_or_vertical
  ; break_infix_before_func= true
  ; break_fun_decl= `Fit_or_vertical
  ; break_fun_sig= `Fit_or_vertical
  ; break_separators= `Before
  ; break_sequences= true
  ; break_string_literals= `Wrap
  ; break_struct= ocamlformat_profile.break_struct
  ; cases_exp_indent= 2
  ; comment_check= true
  ; disable= false
  ; doc_comments= `Before
  ; doc_comments_padding= 1
  ; doc_comments_tag_only= `Fit
  ; escape_chars= `Preserve
  ; escape_strings= `Preserve
  ; extension_sugar= `Preserve
  ; field_space= `Loose
  ; if_then_else= `Keyword_first
  ; indent_after_in= 0
  ; indicate_multiline_delimiters= false
  ; indicate_nested_or_patterns= `Unsafe_no
  ; infix_precedence= `Parens
  ; leading_nested_match_parens= true
  ; let_and= `Sparse
  ; let_binding_indent= 2
  ; let_binding_spacing= `Double_semicolon
  ; let_module= `Sparse
  ; let_open= `Preserve
  ; margin= 90
  ; max_iters= ocamlformat_profile.max_iters
  ; module_item_spacing= `Compact
  ; ocp_indent_compat= true
  ; parens_ite= true
  ; parens_tuple= `Multi_line_only
  ; parens_tuple_patterns= `Multi_line_only
  ; parse_docstrings= false
  ; quiet= ocamlformat_profile.quiet
  ; sequence_blank_line= `Compact
  ; sequence_style= `Terminator
  ; single_case= `Sparse
  ; space_around_collection_expressions= true
  ; type_decl= `Sparse
  ; type_decl_indent= 2
  ; wrap_comments= false
  ; wrap_fun_args= false }

let selected_profile_ref = ref (Some ocamlformat_profile)

let (_profile : t option C.t) =
  let doc =
    "Select a preset profile which sets $(i,all) options, overriding lower \
     priority configuration."
  in
  let names = profile_option_names in
  let all =
    [ ( "ocamlformat"
      , Some ocamlformat_profile
      , "The $(b,ocamlformat) profile aims to take advantage of the \
         strengths of a parsetree-based auto-formatter, and to limit the \
         consequences of the weaknesses imposed by the current \
         implementation. This is a style which optimizes for what the \
         formatter can do best, rather than to match the style of any \
         existing code. General guidelines that have directed the design \
         include: Legibility, in the sense of making it as hard as \
         possible for quick visual parsing to give the wrong \
         interpretation, is of highest priority; Whenever possible the \
         high-level structure of the code should be obvious by looking \
         only at the left margin, in particular, it should not be \
         necessary to visually jump from left to right hunting for \
         critical keywords, tokens, etc; All else equal compact code is \
         preferred as reading without scrolling is easier, so indentation \
         or white space is avoided unless it helps legibility; Attention \
         has been given to making some syntactic gotchas visually obvious."
      )
    ; ( "compact"
      , Some compact_profile
      , "The $(b,compact) profile is similar to $(b,ocamlformat) but opts \
         for a generally more compact code style." )
    ; ( "sparse"
      , Some sparse_profile
      , "The $(b,sparse) profile is similar to $(b,ocamlformat) but opts \
         for a generally more sparse code style." )
    ; ( "conventional"
      , Some conventional_profile
      , "The $(b,conventional) profile aims to be as familiar and \
         \"conventional\" appearing as the available options allow." )
    ; ( "janestreet"
      , Some janestreet_profile
      , "The $(b,janestreet) profile is used at Jane Street." ) ]
  in
  C.choice ~names ~all ~doc ~section ~has_default:false
    (fun conf p ->
      selected_profile_ref := p ;
      Option.value p ~default:conf)
    (fun _ -> !selected_profile_ref)

let validate () =
  let inputs_len = List.length !inputs in
  let has_stdin = List.exists ~f:(String.equal "-") !inputs in
  if !disable_outside_detected_project then
    Format.eprintf
      "Warning: option `--disable-outside-detected-project` is deprecated \
       and will be removed in OCamlFormat v1.0." ;
  if !print_config then `Ok ()
  else if inputs_len = 0 then
    `Error (false, "Must specify at least one input file, or `-` for stdin")
  else if has_stdin && inputs_len > 1 then
    `Error (false, "Cannot specify stdin together with other inputs")
  else if has_stdin && Option.is_none !name then
    `Error (false, "Must specify name when reading from stdin")
  else if !inplace && Option.is_some !output then
    `Error (false, "Cannot specify --output with --inplace")
  else if !check && !inplace then
    `Error (false, "Cannot specify --inplace with --check")
  else if !check && Option.is_some !output then
    `Error (false, "Cannot specify --output with --check")
  else if (not (!inplace || !check)) && inputs_len > 1 then
    `Error
      ( false
      , "Must specify exactly one input file without --inplace or --check"
      )
  else `Ok ()

;;
parse info validate

let ocp_indent_normal_profile =
  [ ("base", "2")
  ; ("type", "2")
  ; ("in", "0")
  ; ("with", "0")
  ; ("match_clause", "2")
  ; ("ppx_stritem_ext", "2")
  ; ("max_indent", "4")
  ; ("strict_with", "never")
  ; ("strict_else", "always")
  ; ("strict_comments", "false")
  ; ("align_ops", "true")
  ; ("align_params", "auto") ]

let ocp_indent_apprentice_profile =
  [ ("base", "2")
  ; ("type", "4")
  ; ("in", "2")
  ; ("with", "2")
  ; ("match_clause", "4")
  ; ("ppx_stritem_ext", "2")
  ; ("strict_with", "never")
  ; ("strict_else", "always")
  ; ("strict_comments", "false")
  ; ("align_ops", "true")
  ; ("align_params", "always") ]

let ocp_indent_janestreet_profile =
  [ ("base", "2")
  ; ("type", "2")
  ; ("in", "0")
  ; ("with", "0")
  ; ("match_clause", "2")
  ; ("ppx_stritem_ext", "2")
  ; ("max_indent", "2")
  ; ("strict_with", "auto")
  ; ("strict_else", "always")
  ; ("strict_comments", "true")
  ; ("align_ops", "true")
  ; ("align_params", "always") ]

let root =
  Option.map !root ~f:Fpath.(fun x -> v x |> to_absolute |> normalize)

let enable_outside_detected_project =
  !enable_outside_detected_project && Option.is_none root

let parse_line config ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.version value || !no_version_check then
          Ok config
        else
          Error
            (`Bad_value
              ( value
              , Format.sprintf "expecting %s but got %s" Version.version
                  value ))
    | name, `File x ->
        C.update ~config
          ~from:(`Parsed (`File x))
          ~name ~value ~inline:false
    | name, `Attribute ->
        if !disable_conf_attrs then (
          if not config.quiet then
            Format.eprintf
              "Warning: Configuration in attribute %S ignored.\n" s ;
          Ok config )
        else
          C.update ~config
            ~from:(`Parsed `Attribute)
            ~name ~value ~inline:true
  in
  let update_ocp_indent_option ~config ~from ~name ~value =
    let opt =
      List.Assoc.find_exn ocp_indent_options ~equal:String.equal name
    in
    match opt with
    | None -> Ok config
    | Some (ocamlformat_opt, _doc, f) ->
        update ~config ~from ~name:ocamlformat_opt ~value:(f value)
  in
  let rec update_many ~config ~from = function
    | [] -> Ok config
    | (name, value) :: t -> (
      match update_ocp_indent_option ~config ~from ~name ~value with
      | Ok c -> update_many ~config:c ~from t
      | Error e -> Error e )
  in
  let s =
    match String.index s '#' with
    | Some i -> String.sub s ~pos:0 ~len:i
    | None -> s
  in
  let s = String.strip s in
  match String.split ~on:'=' s with
  | [] | [""] -> Ok config
  | [name; value] ->
      let name = String.strip name in
      let value = String.strip value in
      if List.Assoc.mem ocp_indent_options ~equal:String.equal name then
        update_ocp_indent_option ~config ~from ~name ~value
      else update ~config ~from ~name ~value
  | [s] -> (
    match String.strip s with
    | "" -> impossible "previous match"
    (* special case for disable/enable *)
    | "enable" -> update ~config ~from ~name:"disable" ~value:"false"
    | "normal" -> update_many ~config ~from ocp_indent_normal_profile
    | "apprentice" ->
        update_many ~config ~from ocp_indent_apprentice_profile
    | "JaneStreet" ->
        Result.( >>= )
          (update ~config ~from ~name:"profile" ~value:"janestreet")
          (fun config ->
            update_many ~config ~from ocp_indent_janestreet_profile)
    | name -> update ~config ~from ~name ~value:"true" )
  | _ -> Error (`Malformed s)

let is_project_root dir =
  match root with
  | Some root -> Fpath.equal dir root
  | None ->
      List.exists project_root_witness ~f:(fun name ->
          Fpath.(exists (dir / name)))

let dot_ocp_indent = ".ocp-indent"

let dot_ocamlformat = ".ocamlformat"

let dot_ocamlformat_ignore = ".ocamlformat-ignore"

let rec collect_files ~segs ~ignores ~files =
  match segs with
  | [] | [""] -> (ignores, files, None)
  | "" :: upper_segs -> collect_files ~segs:upper_segs ~ignores ~files
  | _ :: upper_segs ->
      let dir =
        String.concat ~sep:Fpath.dir_sep (List.rev segs) |> Fpath.v
      in
      let files =
        let filename = Fpath.(dir / dot_ocamlformat) in
        if Fpath.exists filename then `Ocamlformat filename :: files
        else files
      in
      let ignores =
        let filename = Fpath.(dir / dot_ocamlformat_ignore) in
        if Fpath.exists filename then filename :: ignores else ignores
      in
      let files =
        let filename = Fpath.(dir / dot_ocp_indent) in
        if Fpath.exists filename then `Ocp_indent filename :: files
        else files
      in
      if is_project_root dir && not enable_outside_detected_project then
        (ignores, files, Some dir)
      else collect_files ~segs:upper_segs ~ignores ~files

let read_config_file conf filename_kind =
  match filename_kind with
  | `Ocp_indent _ when not !ocp_indent_config -> conf
  | `Ocp_indent filename | `Ocamlformat filename -> (
    try
      In_channel.with_file (Fpath.to_string filename) ~f:(fun ic ->
          let c, errors, _ =
            In_channel.fold_lines ic ~init:(conf, [], 1)
              ~f:(fun (conf, errors, num) line ->
                match
                  parse_line conf ~from:(`File (filename, num)) line
                with
                | Ok conf -> (conf, errors, Int.succ num)
                | Error e -> (conf, e :: errors, Int.succ num))
          in
          match List.rev errors with
          | [] -> c
          | l ->
              let kind =
                match filename_kind with
                | `Ocp_indent _ -> dot_ocp_indent
                | `Ocamlformat _ -> dot_ocamlformat
              in
              user_error
                (Format.sprintf "malformed %s file" kind)
                (List.map l ~f:(function
                  | `Malformed line -> ("invalid format", Sexp.Atom line)
                  | `Misplaced (name, _) ->
                      ("not allowed here", Sexp.Atom name)
                  | `Unknown (name, _value) ->
                      ("unknown option", Sexp.Atom name)
                  | `Bad_value (name, reason) ->
                      ( "bad value for"
                      , Sexp.List [Sexp.Atom name; Sexp.Atom reason] ))))
    with Sys_error _ -> conf )

let update_using_env conf =
  let f (config, errors) (name, value) =
    match C.update ~config ~from:`Env ~name ~value ~inline:false with
    | Ok c -> (c, errors)
    | Error e -> (config, e :: errors)
  in
  let conf, errors = List.fold_left !config ~init:(conf, []) ~f in
  try
    match List.rev errors with
    | [] -> conf
    | l ->
        user_error "malformed OCAMLFORMAT environment variable"
          (List.map l ~f:(function
            | `Malformed line -> ("invalid format", Sexp.Atom line)
            | `Misplaced (name, _) -> ("not allowed here", Sexp.Atom name)
            | `Unknown (name, _value) -> ("unknown option", Sexp.Atom name)
            | `Bad_value (name, reason) ->
                ( "bad value for"
                , Sexp.List [Sexp.Atom name; Sexp.Atom reason] )))
  with Sys_error _ -> conf

type 'a input = {kind: 'a; name: string; file: string; conf: t}

type action =
  | In_out of [`Impl | `Intf | `Use_file] input * string option
  | Inplace of [`Impl | `Intf | `Use_file] input list
  | Check of [`Impl | `Intf | `Use_file] input list

let kind_of fname =
  match !kind with
  | Some kind -> kind
  | None -> (
    match Filename.extension fname with
    | ".ml" -> `Impl
    | ".mli" -> `Intf
    | ".mlt" -> `Use_file
    | _ -> `Impl )

let xdg_config =
  let xdg_config_home =
    match Caml.Sys.getenv_opt "XDG_CONFIG_HOME" with
    | None | Some "" -> (
      match Caml.Sys.getenv_opt "HOME" with
      | None | Some "" -> None
      | Some home -> Some Fpath.(v home / ".config") )
    | Some xdg_config_home -> Some (Fpath.v xdg_config_home)
  in
  match xdg_config_home with
  | Some xdg_config_home ->
      let filename = Fpath.(xdg_config_home / "ocamlformat") in
      if Fpath.exists filename then Some filename else None
  | None -> None

let is_ignored ~quiet ~ignores ~filename =
  let drop_line l = String.is_empty l || String.is_prefix l ~prefix:"#" in
  (* process deeper files first *)
  let ignores = List.rev ignores in
  List.find_map ignores ~f:(fun ignore_file ->
      let dir, _ = Fpath.split_base ignore_file in
      try
        In_channel.with_file (Fpath.to_string ignore_file) ~f:(fun ch ->
            let lines =
              In_channel.input_lines ch
              |> List.mapi ~f:(fun i s -> (i + 1, String.strip s))
              |> List.filter ~f:(fun (_, l) -> not (drop_line l))
            in
            List.find_map lines ~f:(fun (lno, line) ->
                match Fpath.of_string line with
                | Ok file_on_current_line ->
                    let f = Fpath.(dir // file_on_current_line) in
                    if Fpath.equal filename f then Some (ignore_file, lno)
                    else None
                | Error (`Msg msg) ->
                    if not quiet then
                      Format.eprintf "File %a, line %d:\nWarning: %s\n"
                        (Fpath.pp ~pretty:true) ignore_file lno msg ;
                    None))
      with Sys_error err ->
        if not quiet then
          Format.eprintf "Warning: ignoring %a, %s\n"
            (Fpath.pp ~pretty:true) ignore_file err ;
        None)

let build_config ~file =
  let file_abs = Fpath.(v file |> to_absolute |> normalize) in
  let dir = Fpath.(file_abs |> split_base |> fst) in
  let segs = Fpath.segs dir |> List.rev in
  let ignores, files, project_root =
    collect_files ~segs ~ignores:[] ~files:[]
  in
  let files =
    match (xdg_config, enable_outside_detected_project) with
    | None, _ | Some _, false -> files
    | Some f, true -> `Ocamlformat f :: files
  in
  let files = if !disable_conf_files then [] else files in
  let conf =
    List.fold files ~init:ocamlformat_profile ~f:read_config_file
    |> update_using_env |> C.update_using_cmdline
  in
  let no_ocamlformat_files =
    let f = function `Ocamlformat _ -> false | `Ocp_indent _ -> true in
    List.for_all files ~f
  in
  if no_ocamlformat_files && not enable_outside_detected_project then (
    ( if not conf.quiet then
      let reason =
        match project_root with
        | Some root ->
            Format.sprintf
              "no [.ocamlformat] was found within the project (root: %s)"
              (Fpath.to_string ~pretty:true root)
        | None -> "no project root was found"
      in
      Format.eprintf
        "File %S:@\n\
         Warning: Ocamlformat disabled because \
         [--enable-outside-detected-project] is not set and %s@\n\
         %!"
        file reason ) ;
    {conf with disable= true} )
  else
    match is_ignored ~quiet:conf.quiet ~ignores ~filename:file_abs with
    | None -> conf
    | Some (ignored_in_file, lno) ->
        if !debug then
          Format.eprintf "File %a: ignored in %a:%d@\n"
            (Fpath.pp ~pretty:true) file_abs (Fpath.pp ~pretty:true)
            ignored_in_file lno ;
        {conf with disable= true}

;;
if !print_config then
  let file =
    match (!name, !inputs) with
    | Some file, _ | None, file :: _ -> file
    | None, [] ->
        let root = Option.value root ~default:(Fpath.cwd ()) in
        Fpath.(root / dot_ocamlformat |> to_string)
  in
  C.print_config (build_config ~file)

let action =
  if !inplace then
    Inplace
      (List.map !inputs ~f:(fun file ->
           let name = Option.value !name ~default:file in
           {kind= kind_of file; name; file; conf= build_config ~file:name}))
  else if !check then
    Check
      (List.map !inputs ~f:(fun file ->
           let name = Option.value !name ~default:file in
           let conf = build_config ~file:name in
           let conf = {conf with max_iters= 1} in
           {kind= kind_of file; name; file; conf}))
  else
    match !inputs with
    | [input_file] ->
        let name = Option.value !name ~default:input_file in
        In_out
          ( { kind= kind_of name
            ; name
            ; file= input_file
            ; conf= build_config ~file:name }
          , !output )
    | _ ->
        if !print_config then Caml.exit 0
        else impossible "checked by validate"

and debug = !debug

and check = !check

let parse_line_in_attribute = parse_line ~from:`Attribute
