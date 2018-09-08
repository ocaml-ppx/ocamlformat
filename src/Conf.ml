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
  { break_cases: [`Fit | `Nested | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; break_string_literals: [`Newlines | `Never | `Wrap]
  ; break_struct: bool
  ; comment_check: bool
  ; disable: bool
  ; doc_comments: [`Before | `After]
  ; escape_chars: [`Decimal | `Hexadecimal | `Preserve]
  ; escape_strings: [`Decimal | `Hexadecimal | `Preserve]
  ; extension_sugar: [`Preserve | `Always]
  ; field_space: [`Tight | `Loose]
  ; if_then_else: [`Compact | `Keyword_first]
  ; indicate_nested_or_patterns: bool
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_open: [`Preserve | `Auto | `Short | `Long]
  ; margin: int
  ; max_iters: int
  ; module_item_spacing: [`Compact | `Sparse]
  ; ocp_indent_compat: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; quiet: bool
  ; sequence_style: [`Separator | `Terminator]
  ; type_decl: [`Compact | `Sparse]
  ; wrap_comments: bool
  ; wrap_fun_args: bool }

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

open Cmdliner

module C : sig
  type config = t

  type 'a t

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> (config -> 'a -> config)
    -> 'a t

  val section_name : [`Formatting | `Operational] -> string

  val choice :
    ?has_default:bool -> all:(string * 'a * string) list -> 'a option_decl

  val flag : default:bool -> bool option_decl

  val int : default:int -> docv:string -> int option_decl

  val default : 'a t -> 'a

  val update_using_cmdline : config -> verbose:bool -> config

  val update :
       config:config
    -> verbose:bool
    -> from:[`File of string * int | `Config | `Commandline | `Attribute]
    -> name:string
    -> value:string
    -> inline:bool
    -> ( config
       , [ `Unknown of string * string
         | `Bad_value of string * string
         | `Malformed of string
         | `Misplaced of string * string ] )
       Result.t
end = struct
  type config = t

  type 'a t =
    { names: string list
    ; parse: string -> 'a
    ; update: config -> 'a -> config
    ; allow_inline: bool
    ; cmdline_get: unit -> 'a option
    ; to_string: 'a -> string
    ; default: 'a }

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> (config -> 'a -> config)
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

  let choice ?(has_default = true) ~all ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) update =
    let _, default, _ = List.hd_exn all in
    let doc =
      generated_choice_doc ~allow_inline ~all ~doc ~section ~has_default
    in
    let docv = generated_choice_docv ~all in
    let opt_names = List.map all ~f:(fun (x, y, _) -> (x, y)) in
    let to_string v' =
      List.find_map_exn all ~f:(fun (str, v, _) ->
          if Poly.equal v v' then Some str else None )
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
            Option.some_if (String.equal n s) v )
      with
      | Some v -> v
      | None ->
          user_error
            (Printf.sprintf "Unknown %s value: %S" (List.hd_exn names) s)
            []
    in
    let r = mk ~default:None term in
    let cmdline_get () = !r in
    let opt =
      {names; parse; update; cmdline_get; allow_inline; default; to_string}
    in
    store := Pack opt :: !store ;
    opt

  let flag ~default ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) update =
    let open Cmdliner in
    let invert_flag = default in
    let names_for_cmdline =
      if invert_flag then
        List.filter_map names ~f:(fun n ->
            if String.length n = 1 then None else Some ("no-" ^ n) )
      else names
    in
    let doc = generated_flag_doc ~allow_inline ~doc ~section in
    let docs = section_name section in
    let term = Arg.(value & flag & info names_for_cmdline ~doc ~docs) in
    let parse = Bool.of_string in
    let r = mk ~default term in
    let to_string = Bool.to_string in
    let cmdline_get () = if !r then Some (not invert_flag) else None in
    let opt =
      {names; parse; update; cmdline_get; allow_inline; default; to_string}
    in
    store := Pack opt :: !store ;
    opt

  let int ~default ~docv ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) update =
    let open Cmdliner in
    let doc = generated_int_doc ~allow_inline ~doc ~section ~default in
    let docs = section_name section in
    let term =
      Arg.(value & opt (some int) None & info names ~doc ~docs ~docv)
    in
    let parse = Int.of_string in
    let r = mk ~default:None term in
    let to_string = Int.to_string in
    let cmdline_get () = !r in
    let opt =
      {names; parse; update; cmdline_get; allow_inline; default; to_string}
    in
    store := Pack opt :: !store ;
    opt

  let log_update ~from ~name ~value =
    match from with
    | `Attribute -> ()
    | (`File _ | `Config | `Commandline) as from ->
        let from =
          match from with
          | `File (file, lnum) -> Format.sprintf " (%s:%d)" file lnum
          | `Config -> " (Environment variable or --config)"
          | `Commandline -> " (command line)"
        in
        Format.eprintf "%s=%s%s@\n%!" name value from

  let update ~config ~verbose ~from ~name ~value ~inline =
    List.find_map !store
      ~f:(fun (Pack {names; parse; update; allow_inline}) ->
        if List.exists names ~f:(String.equal name) then
          if inline && not allow_inline then
            Some (Error (`Misplaced (name, value)))
          else
            match parse value with
            | packed_value ->
                if verbose then log_update ~from ~name ~value ;
                Some (Ok (update config packed_value))
            | exception _ -> Some (Error (`Bad_value (name, value)))
        else None )
    |> Option.value ~default:(Error (`Unknown (name, value)))

  let default {default} = default

  let update_using_cmdline config ~verbose =
    let on_pack config (Pack {names; cmdline_get; update; to_string}) =
      let name = List.hd_exn names in
      match cmdline_get () with
      | None -> config
      | Some x ->
          let config = update config x in
          if verbose then
            log_update ~from:`Commandline ~name ~value:(to_string x) ;
          config
    in
    List.fold !store ~init:config ~f:on_pack
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
        "(*) .ocamlformat files in current and all ancestor directories \
         for each input file are used, as well as the global .ocamlformat \
         file defined in $(b,\\$XDG_CONFIG_HOME/ocamlformat). The global \
         .ocamlformat file has the lowest priority, then the closer the \
         directory is to the processed file, the higher the priority." ]
  in
  Term.info "ocamlformat" ~version:Version.version ~doc ~man

(** Options affecting formatting *)
module Formatting = struct
  let section = `Formatting

  let break_cases =
    let doc = "Break pattern match cases." in
    let names = ["break-cases"] in
    let all =
      [ ( "fit"
        , `Fit
        , "Specifying $(b,fit) lets pattern matches break at the margin \
           naturally." )
      ; ( "nested"
        , `Nested
        , "$(b,nested) forces a break after nested or-patterns to \
           highlight the case body." )
      ; ( "all"
        , `All
        , "$(b,all) forces all pattern matches to break across lines." ) ]
    in
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with break_cases= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with break_collection_expressions= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with break_infix= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with break_string_literals= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with break_struct= Poly.(x = `Force)} )

  let disable =
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(b,[@@@ocamlformat \
       \"enable\"]) instead of $(b,[@@@ocamlformat \"disable=false\"])"
    in
    C.flag ~names:["disable"] ~default:false ~doc ~section (fun conf x ->
        {conf with disable= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with doc_comments= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with escape_chars= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with escape_strings= x} )

  let extension_sugar =
    let doc = "Extension formatting." in
    let names = ["extension-sugar"] in
    let all = [("preserve", `Preserve, ""); ("always", `Always, "")] in
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with extension_sugar= x} )

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
           punctuation symbol (`:`or `=`)." )
      ; ("loose", `Loose, "$(b,loose) does.") ]
    in
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with field_space= x} )

  let if_then_else =
    let doc = "If-then-else formatting." in
    let names = ["if-then-else"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) tries to format an if-then-else expression on a \
           single line." )
      ; ( "keyword-first"
        , `Keyword_first
        , "$(b,keyword-first) formats if-then-else expressions such that \
           the if-then-else keywords are the first on the line." ) ]
    in
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with if_then_else= x} )

  let indicate_nested_or_patterns =
    let default = true in
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let names = ["indicate-nested-or-patterns"] in
    C.flag ~names ~default ~doc ~section (fun conf x ->
        {conf with indicate_nested_or_patterns= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with infix_precedence= x} )

  let leading_nested_match_parens =
    let doc = "Nested match parens formatting." in
    let names = ["leading-nested-match-parens"] in
    C.flag ~default:false ~names ~doc ~section ~allow_inline:false
      (fun conf x -> {conf with leading_nested_match_parens= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with let_binding_spacing= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x -> {conf with let_and= x})

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
    C.choice ~names:["let-open"] ~all ~doc ~section (fun conf x ->
        {conf with let_open= x} )

  let margin =
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    C.int ~names:["m"; "margin"] ~default:80 ~doc ~docv ~section
      ~allow_inline:false (fun conf x -> {conf with margin= x} )

  let module_item_spacing =
    let doc = "Spacing between items of structures and signatures." in
    let names = ["module-item-spacing"] in
    let all =
      [ ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break a line between two items." )
      ; ( "compact"
        , `Compact
        , "$(b,compact) will not leave open lines between one-liners of \
           similar sorts." ) ]
    in
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with module_item_spacing= x} )

  let ocp_indent_compat =
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let names = ["ocp-indent-compat"] in
    C.flag ~default:false ~names ~doc ~section (fun conf x ->
        {conf with ocp_indent_compat= x} )

  let parens_tuple =
    let doc = "Parens tuples." in
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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with parens_tuple= x} )

  let sequence_style =
    let doc = "Style of sequence." in
    let names = ["sequence-style"] in
    let all =
      [("separator", `Separator, ""); ("terminator", `Terminator, "")]
    in
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with sequence_style= x} )

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
    C.choice ~names ~all ~doc ~section (fun conf x ->
        {conf with type_decl= x} )

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
      (fun conf x -> {conf with wrap_comments= x} )

  let wrap_fun_args =
    let default = true in
    let doc = "Style for function call and function definition." in
    let names = ["wrap-fun-args"] in
    C.flag ~default ~names ~doc ~section (fun conf wrap_fun_args ->
        {conf with wrap_fun_args} )
end

(* Flags that can be modified in the config file that don't affect
   formatting *)

let project_root_witness = [".git"; ".hg"; "dune-project"]

let section = `Operational

let docs = C.section_name section

let comment_check =
  let default = true in
  let doc =
    "UNSAFE: Control wether to check comments and documentation comments. \
     May be set in $(b,.ocamlformat)."
  in
  C.flag ~default ~names:["comment-check"] ~doc ~section (fun conf x ->
      {conf with comment_check= x} )

let disable_outside_project =
  let witness =
    String.concat ~sep:" or "
      (List.map project_root_witness ~f:(fun name ->
           Format.sprintf "$(b,%s)" name ))
  in
  let doc =
    Format.sprintf
      "Do not read $(b,.ocamlformat) config files outside the current \
       project. The project root of an input file is taken to be the \
       nearest ancestor directory that contains a %s file. If no config \
       file is found, formatting is disabled."
      witness
  in
  let default = false in
  mk ~default
    Arg.(value & flag & info ["disable-outside-project"] ~doc ~docs)

let max_iters =
  let docv = "N" in
  let doc =
    "Fail if output of formatting does not stabilize within $(docv) \
     iterations. May be set in $(b,.ocamlformat)."
  in
  C.int ~names:["n"; "max-iters"] ~default:10 ~doc ~docv ~section
    (fun conf x -> {conf with max_iters= x} )

let quiet =
  let doc = "Quiet. May be set in $(b,.ocamlformat)." in
  C.flag ~default:false ~names:["q"; "quiet"] ~doc ~section (fun conf x ->
      {conf with quiet= x} )

(* Other Flags *)

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

let kind : [`Impl | `Intf | `Use_file] ref =
  let doc =
    "Parse file with unrecognized extension as an implementation."
  in
  let impl = (`Impl, Arg.info ["impl"] ~doc ~docs) in
  let doc = "Parse file with unrecognized extension as an interface." in
  let intf = (`Intf, Arg.info ["intf"] ~doc ~docs) in
  let doc = "Parse file with unrecognized extension as a use_file." in
  let use_file = (`Use_file, Arg.info ["use-file"] ~doc ~docs) in
  let default = `Impl in
  mk ~default Arg.(value & vflag default [impl; intf; use_file])

let name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting. Defaults to the input \
     file name. Some options can be specified in configuration files named \
     '.ocamlformat' in the same or a parent directory of $(docv), see \
     documentation of other options for details. Mutually exclusive with \
     --inplace."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some string) default & info ["name"] ~doc ~docs ~docv)

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
  let doc = "Print config" in
  let default = false in
  mk ~default Arg.(value & flag & info ["print-config"] ~doc ~docs)

let no_version_check =
  let doc =
    "Do no check version matches the one specified in .ocamlformat."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-version-check"] ~doc ~docs)

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

let default =
  { break_cases= C.default Formatting.break_cases
  ; break_collection_expressions=
      C.default Formatting.break_collection_expressions
  ; break_infix= C.default Formatting.break_infix
  ; break_string_literals= C.default Formatting.break_string_literals
  ; break_struct= Poly.(C.default Formatting.break_struct = `Force)
  ; comment_check= C.default comment_check
  ; disable= C.default Formatting.disable
  ; doc_comments= C.default Formatting.doc_comments
  ; escape_chars= C.default Formatting.escape_chars
  ; escape_strings= C.default Formatting.escape_strings
  ; extension_sugar= C.default Formatting.extension_sugar
  ; field_space= C.default Formatting.field_space
  ; if_then_else= C.default Formatting.if_then_else
  ; indicate_nested_or_patterns=
      C.default Formatting.indicate_nested_or_patterns
  ; infix_precedence= C.default Formatting.infix_precedence
  ; leading_nested_match_parens=
      C.default Formatting.leading_nested_match_parens
  ; let_and= C.default Formatting.let_and
  ; let_binding_spacing= C.default Formatting.let_binding_spacing
  ; let_open= C.default Formatting.let_open
  ; margin= C.default Formatting.margin
  ; max_iters= C.default max_iters
  ; module_item_spacing= C.default Formatting.module_item_spacing
  ; ocp_indent_compat= C.default Formatting.ocp_indent_compat
  ; parens_tuple= C.default Formatting.parens_tuple
  ; quiet= C.default quiet
  ; sequence_style= C.default Formatting.sequence_style
  ; type_decl= C.default Formatting.type_decl
  ; wrap_comments= C.default Formatting.wrap_comments
  ; wrap_fun_args= C.default Formatting.wrap_fun_args }

let janestreet_profile =
  { default with
    break_cases= `Fit
  ; break_infix= `Fit_or_vertical
  ; break_string_literals= `Wrap
  ; doc_comments= `Before
  ; extension_sugar= `Preserve
  ; field_space= `Loose
  ; if_then_else= `Keyword_first
  ; indicate_nested_or_patterns= false
  ; infix_precedence= `Parens
  ; leading_nested_match_parens= true
  ; let_and= `Sparse
  ; let_open= `Preserve
  ; margin= 90
  ; ocp_indent_compat= true
  ; parens_tuple= `Multi_line_only
  ; sequence_style= `Terminator
  ; type_decl= `Sparse
  ; wrap_fun_args= false
  ; module_item_spacing= `Compact }

let (_profile : t option C.t) =
  let doc =
    "Preset profiles which set $(i,all) options, overriding lower priority \
     configuration."
  in
  let names = ["p"; "profile"] in
  let all =
    [ ( "default"
      , Some default
      , "$(b,default) sets each option to its default value." )
    ; ( "janestreet"
      , Some janestreet_profile
      , "$(b,janestreet) is the profile used at JaneStreet." ) ]
  in
  C.choice ~names ~all ~doc ~section ~has_default:false (fun conf p ->
      Option.value p ~default:conf )

let validate () =
  if List.is_empty !inputs then
    `Error (false, "Must specify at least one input file, or `-` for stdin")
  else if
    List.equal ~equal:String.equal !inputs ["-"] && Option.is_none !name
  then `Error (false, "Must specify name when reading from stdin")
  else if !inplace && Option.is_some !name then
    `Error (false, "Cannot specify --name with --inplace")
  else if !inplace && Option.is_some !output then
    `Error (false, "Cannot specify --output with --inplace")
  else if (not !inplace) && List.length !inputs > 1 then
    `Error (false, "Must specify exactly one input file without --inplace")
  else `Ok ()

;;
parse info validate

let parse_line config ~verbose ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.version value || !no_version_check then
          Ok config
        else Error (`Bad_value (value, name))
    | name, (`File _ | `Config | `Commandline) ->
        C.update ~config ~verbose ~from ~name ~value ~inline:false
    | name, `Attribute ->
        C.update ~config ~verbose ~from ~name ~value ~inline:true
  in
  let s =
    match String.index s '#' with
    | Some i -> String.sub s ~pos:0 ~len:i
    | None -> s
  in
  let s = String.strip s in
  match String.split ~on:'=' s with
  | [] | [""] -> Ok config
  | [name; value] -> update ~config ~from ~name ~value
  | [s] -> (
    match
      ( List.filter (String.split ~on:' ' s) ~f:(fun s ->
            not (String.is_empty s) )
      , from )
    with
    | ([] | [""]), _ -> impossible "previous match"
    | [name; value], `File (filename, lnum) ->
        (* tolerate space separated [var value] to compatibility with older
           config file format *)
        if not config.quiet then
          Format.eprintf
            "File %S, line %d:\n\
             Warning: Using deprecated ocamlformat config syntax.\n\
             Please use `%s = %s`\n"
            filename lnum name value ;
        update ~config ~from ~name ~value
    (* special case for disable/enable *)
    | ["enable"], _ -> update ~config ~from ~name:"disable" ~value:"false"
    | [name], _ -> update ~config ~from ~name ~value:"true"
    | _ -> Error (`Malformed s) )
  | _ -> Error (`Malformed s)

let is_project_root dir =
  List.exists project_root_witness ~f:(fun name ->
      Caml.Sys.file_exists (Filename.concat dir name) )

let rec collect_files ~dir acc =
  let acc =
    let filename = Filename.concat dir ".ocamlformat" in
    if Caml.Sys.file_exists filename then filename :: acc else acc
  in
  if is_project_root dir && !disable_outside_project then (acc, Some dir)
  else
    let dir' = Filename.dirname dir in
    if (not (String.equal dir dir')) && Caml.Sys.file_exists dir then
      collect_files ~dir:dir' acc
    else if !disable_outside_project then ([], None)
    else (acc, None)

let read_config_file ~verbose conf filename =
  try
    In_channel.with_file filename ~f:(fun ic ->
        let c, errors, _ =
          In_channel.fold_lines ic ~init:(conf, [], 1)
            ~f:(fun (conf, errors, num) line ->
              match
                parse_line conf ~verbose ~from:(`File (filename, num)) line
              with
              | Ok conf -> (conf, errors, Int.succ num)
              | Error e -> (conf, e :: errors, Int.succ num) )
        in
        match List.rev errors with
        | [] -> c
        | l ->
            user_error "malformed .ocamlformat file"
              (List.map l ~f:(function
                | `Malformed line -> ("invalid format", Sexp.Atom line)
                | `Misplaced (name, _) ->
                    ("not allowed here", Sexp.Atom name)
                | `Unknown (name, _value) ->
                    ("unknown option", Sexp.Atom name)
                | `Bad_value (name, value) ->
                    ( "bad value for"
                    , Sexp.List [Sexp.Atom name; Sexp.Atom value] ) )) )
  with Sys_error _ -> conf

let to_absolute file =
  Filename.(if is_relative file then concat (Unix.getcwd ()) file else file)

let update_using_env ~verbose conf =
  let f (config, errors) (name, value) =
    match
      C.update ~config ~verbose ~from:`Config ~name ~value ~inline:false
    with
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
            | `Bad_value (name, value) ->
                ( "bad value for"
                , Sexp.List [Sexp.Atom name; Sexp.Atom value] ) ))
  with Sys_error _ -> conf

type 'a input = {kind: 'a; name: string; file: string; conf: t}

type action =
  | In_out of [`Impl | `Intf | `Use_file] input * string option
  | Inplace of [`Impl | `Intf | `Use_file] input list

let kind_of fname =
  match Filename.extension fname with
  | ".ml" -> `Impl
  | ".mli" -> `Intf
  | ".mlt" -> `Use_file
  | _ -> !kind

let xdg_config =
  match Caml.Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some xdg_config_home ->
      let filename =
        Filename.concat xdg_config_home "ocamlformat/.ocamlformat"
      in
      if Caml.Sys.file_exists filename then Some filename else None
  | None -> None

let build_config ~filename =
  let files, project_root =
    collect_files ~dir:(Filename.dirname (to_absolute filename)) []
  in
  let files =
    match (xdg_config, !disable_outside_project) with
    | None, _ | Some _, true -> files
    | Some f, false -> f :: files
  in
  let verbose = !print_config in
  if verbose then
    Option.iter project_root ~f:(Format.eprintf "project-root=%s@\n%!") ;
  let conf =
    List.fold files ~init:default ~f:(read_config_file ~verbose)
    |> update_using_env ~verbose
    |> C.update_using_cmdline ~verbose
  in
  if !disable_outside_project && List.is_empty files then (
    ( if not conf.quiet then
      let reason =
        match project_root with
        | Some root ->
            Printf.sprintf
              "no [.ocamlformat] was found within the project (root: %s)"
              root
        | None -> "no project root was found"
      in
      Format.eprintf
        "File %S:@\n\
         Warning: Ocamlformat disabled because [--disable-outside-project] \
         was given and %s@\n\
         %!"
        filename reason ) ;
    {conf with disable= true} )
  else conf

let action =
  if !inplace then
    Inplace
      (List.map !inputs ~f:(fun file ->
           { kind= kind_of file
           ; name= file
           ; file
           ; conf= build_config ~filename:file } ))
  else
    match !inputs with
    | [input_file] ->
        let name = Option.value !name ~default:input_file in
        In_out
          ( { kind= kind_of name
            ; name
            ; file= input_file
            ; conf= build_config ~filename:name }
          , !output )
    | _ -> impossible "checked by validate"

and debug = !debug

let parse_line_in_attribute = parse_line ~from:`Attribute ~verbose:false
