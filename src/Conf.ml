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
    -> update:(config -> 'a -> config)
    -> env:Term.env_info
    -> doc:string
    -> allow_inline:bool
    -> section:[`Formatting | `Operational]
    -> 'a t

  val section_name : [`Formatting | `Operational] -> string

  val choice : all:(string * 'a * string) list -> 'a option_decl

  val flag : default:bool -> bool option_decl

  val int : default:int -> docv:string -> int option_decl

  val get : 'a t -> 'a

  val update :
       config:config
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
    ; get: unit -> 'a }

  type 'a option_decl =
       names:string list
    -> update:(config -> 'a -> config)
    -> env:Term.env_info
    -> doc:string
    -> allow_inline:bool
    -> section:[`Formatting | `Operational]
    -> 'a t

  type pack = Pack : 'a t -> pack

  let store = ref []

  let in_attributes ~section cond =
    if cond || Poly.(section = `Operational) then ""
    else "Cannot be set in attributes."

  let generated_choice_doc ~allow_inline ~all ~doc ~section =
    let each_opt =
      let f acc (_, _, d) = Format.sprintf "%s@,%s" acc d in
      List.fold all ~init:"" ~f
    in
    Format.sprintf "%s %s %s" doc each_opt
      (in_attributes ~section allow_inline)

  let generated_flag_doc ~allow_inline ~doc ~section =
    Format.sprintf "%s %s" doc (in_attributes ~section allow_inline)

  let generated_int_doc ~allow_inline ~doc ~section =
    Format.sprintf "%s %s" doc (in_attributes ~section allow_inline)

  let section_name = function
    | `Formatting -> Cmdliner.Manpage.s_options ^ " (CODE FORMATTING STYLE)"
    | `Operational -> Cmdliner.Manpage.s_options

  let choice ~all ~names ~update ~env ~doc ~allow_inline ~section =
    let open Cmdliner in
    let _, default, _ = List.hd_exn all in
    let doc = generated_choice_doc ~allow_inline ~all ~doc ~section in
    let opt_names = List.map all ~f:(fun (x, y, _) -> (x, y)) in
    let docs = section_name section in
    let term =
      Arg.(
        value & opt (enum opt_names) default & info names ~doc ~docs ~env)
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
    let r = mk ~default term in
    let get () = !r in
    let opt = {names; parse; update; get; allow_inline} in
    store := Pack opt :: !store ;
    opt

  let flag ~default ~names ~update ~env ~doc ~allow_inline ~section =
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
    let term =
      Arg.(value & flag & info names_for_cmdline ~doc ~docs ~env)
    in
    let parse = Bool.of_string in
    let r = mk ~default term in
    let get () = if invert_flag then not !r else !r in
    let opt = {names; parse; update; get; allow_inline} in
    store := Pack opt :: !store ;
    opt

  let int ~default ~docv ~names ~update ~env ~doc ~allow_inline ~section =
    let open Cmdliner in
    let doc = generated_int_doc ~allow_inline ~doc ~section in
    let docs = section_name section in
    let term =
      Arg.(value & opt int default & info names ~doc ~docs ~docv ~env)
    in
    let parse = Int.of_string in
    let r = mk ~default term in
    let get () = !r in
    let opt = {names; parse; update; get; allow_inline} in
    store := Pack opt :: !store ;
    opt

  let update ~config ~name ~value ~inline =
    List.find_map !store
      ~f:(fun (Pack {names; parse; update; allow_inline}) ->
        if List.exists names ~f:(String.equal name) then
          if inline && not allow_inline then
            Some (Error (`Misplaced (name, value)))
          else
            try Some (Ok (update config (parse value))) with _ ->
              Some (Error (`Bad_value (name, value)))
        else None )
    |> Option.value ~default:(Error (`Unknown (name, value)))

  let get {get} = get ()
end

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "$(tname) automatically formats OCaml code."
    ; `S (C.section_name `Formatting)
    ; `P
        "Unless otherwise noted, any option $(b,--option)=$(b,value) \
         detailed in this section can be set in a configuration file with \
         an '$(b,option) = $(b,value)' line, or with an $(b,ocamlformat) \
         \"$(b,option)=$(b,value)\" attribute." ]
  in
  Term.info "ocamlformat" ~version:Version.version ~doc ~man

(** Options affecting formatting *)
module Formatting = struct
  let section = `Formatting

  let break_cases =
    let doc = "Break pattern match cases." in
    let env = Arg.env_var "OCAMLFORMAT_BREAK_CASES" in
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
    C.choice ~names ~all ~doc ~env ~allow_inline:true
      ~update:(fun conf x -> {conf with break_cases= x})
      ~section

  let break_infix =
    let doc = "Break sequence of infix operators." in
    let env = Arg.env_var "OCAMLFORMAT_BREAK_INFIX" in
    let names = ["break-infix"] in
    let all =
      [ ( "wrap"
        , `Wrap
        , "$(b,wrap) will group simple expressions and try to format them \
           in a single line." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks expression if they do \
           not fit on a single line." ) ]
    in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with break_infix= x})
      ~section

  let break_string_literals =
    let doc = "Break string literals." in
    let env = Arg.env_var "OCAMLFORMAT_BREAK_STRING_LITERALS" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with break_string_literals= x})
      ~section

  let break_struct =
    let doc = "Break struct-end module items." in
    let env = Arg.env_var "OCAMLFORMAT_BREAK_STRUCT" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true ~update:(fun conf x ->
        {conf with break_struct= Poly.(x = `Force)} )

  let disable =
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(i,[@@@ocamlformat \
       \"enable\"]) instead of $(i,[@@@ocamlformat \"disable=false\"])"
    in
    let env = Arg.env_var "OCAMLFORMAT_DISABLE" in
    C.flag ~names:["disable"] ~default:false ~doc ~env ~allow_inline:true
      ~update:(fun conf x -> {conf with disable= x})
      ~section

  let doc_comments =
    let doc = "Doc comments position." in
    let env = Arg.env_var "OCAMLFORMAT_DOC_COMMENTS" in
    let names = ["doc-comments"] in
    let all =
      [ ( "after"
        , `After
        , "$(b,after) puts doc comments after the corresponding code." )
      ; ( "before"
        , `Before
        , "$(b,before) puts comments before the corresponding code." ) ]
    in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with doc_comments= x})
      ~section

  let escape_chars =
    let doc = "Escape encoding for character literals." in
    let env = Arg.env_var "OCAMLFORMAT_ESCAPE_CHARS" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with escape_chars= x})
      ~section

  let escape_strings =
    let doc =
      "Escape encoding for string literals. See `--escape-chars` for the \
       interpretation of the modes."
    in
    let env = Arg.env_var "OCAMLFORMAT_ESCAPE_STRINGS" in
    let names = ["escape-strings"] in
    let all =
      [ ("preserve", `Preserve, "")
      ; ("decimal", `Decimal, "")
      ; ("hexadecimal", `Hexadecimal, "") ]
    in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with escape_strings= x})
      ~section

  let extension_sugar =
    let doc = "Extension formatting." in
    let env = Arg.env_var "OCAMLFORMAT_EXTENSION_SUGAR" in
    let names = ["extension-sugar"] in
    let all = [("preserve", `Preserve, ""); ("always", `Always, "")] in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with extension_sugar= x})
      ~section

  let field_space =
    let doc =
      "Whether or not to use a space between a field name and the rhs. \
       This option affects records and objects."
    in
    let env = Arg.env_var "OCAMLFORMAT_FIELD_SPACE" in
    let names = ["field-space"] in
    let all =
      [ ( "tight"
        , `Tight
        , "$(b,tight) does not use a space between a field name and the \
           punctuation symbol (`:`or `=`)." )
      ; ("loose", `Loose, "$(b,loose) does.") ]
    in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with field_space= x})
      ~section

  let if_then_else =
    let doc = "If-then-else formatting." in
    let env = Arg.env_var "OCAMLFORMAT_IF_THEN_ELSE" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with if_then_else= x})
      ~section

  let indicate_nested_or_patterns =
    let default = true in
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let env = Arg.env_var "OCAMLFORMAT_INDICATE_NESTED_OR_PATTERNS" in
    let names = ["indicate-nested-or-patterns"] in
    C.flag ~names ~env ~default ~doc ~allow_inline:true ~section
      ~update:(fun conf x -> {conf with indicate_nested_or_patterns= x} )

  let infix_precedence =
    let doc =
      "Use indentation or also discretionary parentheses to explicitly \
       disambiguate precedences of infix operators."
    in
    let env = Arg.env_var "OCAMLFORMAT_INFIX_PRECEDENCE" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with infix_precedence= x})
      ~section

  let leading_nested_match_parens =
    let doc = "Nested match parens formatting." in
    let env = Arg.env_var "OCAMLFORMAT_LEADING_NESTED_MATCH_PARENS" in
    let names = ["leading-nested-match-parens"] in
    C.flag ~default:false ~names ~env ~doc ~allow_inline:false ~section
      ~update:(fun conf x -> {conf with leading_nested_match_parens= x} )

  let let_and =
    let doc = "Style of let_and." in
    let env = Arg.env_var "OCAMLFORMAT_LET_AND" in
    let names = ["let-and"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format `let p = e and p = e` in a \
           single line." )
      ; ("sparse", `Sparse, "$(b,sparse) will always break between them.")
      ]
    in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with let_and= x})
      ~section

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
    let env = Arg.env_var "OCAMLFORMAT_LET_OPEN" in
    C.choice ~names:["let-open"] ~all ~doc ~env ~allow_inline:true
      ~update:(fun conf x -> {conf with let_open= x})
      ~section

  let margin =
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    let env = Arg.env_var "OCAMLFORMAT_MARGIN" in
    C.int ~names:["m"; "margin"] ~default:80 ~doc ~docv ~env ~section
      ~allow_inline:false ~update:(fun conf x -> {conf with margin= x} )

  let module_item_spacing =
    let doc = "Spacing between items of structures and signatures." in
    let env = Arg.env_var "OCAMLFORMAT_MODULE_ITEM_SPACING" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with module_item_spacing= x})
      ~section

  let ocp_indent_compat =
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let env = Arg.env_var "OCAMLFORMAT_OCP_INDENT_COMPAT" in
    let names = ["ocp-indent-compat"] in
    C.flag ~default:false ~names ~env ~doc ~allow_inline:true ~section
      ~update:(fun conf x -> {conf with ocp_indent_compat= x} )

  let parens_tuple =
    let doc = "Parens tuples." in
    let env = Arg.env_var "OCAMLFORMAT_PARENS_TUPLE" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with parens_tuple= x})
      ~section

  let sequence_style =
    let doc = "Style of sequence." in
    let env = Arg.env_var "OCAMLFORMAT_SEQUENCE_STYLE" in
    let names = ["sequence-style"] in
    let all =
      [("separator", `Separator, ""); ("terminator", `Terminator, "")]
    in
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with sequence_style= x})
      ~section

  let type_decl =
    let doc = "Style of type declaration." in
    let env = Arg.env_var "OCAMLFORMAT_TYPE_DECL" in
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
    C.choice ~names ~all ~env ~doc ~allow_inline:true
      ~update:(fun conf x -> {conf with type_decl= x})
      ~section

  let wrap_comments =
    let doc =
      "Wrap comments and docstrings. Comments and docstrings are divided \
       into paragraphs by open lines (two or more consecutive newlines), \
       and each paragraph is wrapped at the margin. Multi-line comments \
       with vertically-aligned asterisks on the left margin are not \
       wrapped. Consecutive comments with both left and right margin \
       aligned are not wrapped either."
    in
    let env = Arg.env_var "OCAMLFORMAT_WRAP_COMMENTS" in
    C.flag ~default:false ~names:["wrap-comments"] ~doc ~env ~section
      ~allow_inline:true ~update:(fun conf x -> {conf with wrap_comments= x}
    )

  let wrap_fun_args =
    let default = true in
    let doc = "Style for function call and function definition." in
    let env = Arg.env_var "OCAMLFORMAT_WRAP_FUN_ARGS" in
    let names = ["wrap-fun-args"] in
    C.flag ~default ~names ~env ~doc ~allow_inline:true ~section
      ~update:(fun conf wrap_fun_args -> {conf with wrap_fun_args} )
end

(* Flags that can be modified in the config file that don't affect
   formatting *)

let section = `Operational

let docs = C.section_name section

let comment_check =
  let default = true in
  let doc =
    "UNSAFE: Control wether to check comments and documentation comments."
  in
  let env = Arg.env_var "OCAMLFORMAT_COMMENT_CHECK" in
  C.flag ~default ~names:["comment-check"] ~doc ~env ~allow_inline:false
    ~update:(fun conf x -> {conf with comment_check= x})
    ~section

let max_iters =
  let docv = "N" in
  let doc =
    "Fail if output of formatting does not stabilize within $(docv) \
     iterations."
  in
  let env = Arg.env_var "OCAMLFORMAT_MAX_ITERS" in
  C.int ~names:["n"; "max-iters"] ~default:10 ~doc ~docv ~env ~section
    ~allow_inline:false ~update:(fun conf x -> {conf with max_iters= x} )

let quiet =
  let doc = "Quiet." in
  let env = Arg.env_var "OCAMLFORMAT_QUIET" in
  C.flag ~default:false ~names:["q"; "quiet"] ~doc ~env ~allow_inline:false
    ~update:(fun conf x -> {conf with quiet= x})
    ~section

let no_version_check =
  let doc =
    "Do no check version matches the one specified in .ocamlformat."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-version-check"] ~doc ~docs)

(* Other Flags *)

let debug =
  let doc = "Generate debugging output." in
  let env = Arg.env_var "OCAMLFORMAT_DEBUG" in
  let default = false in
  mk ~default Arg.(value & flag & info ["g"; "debug"] ~doc ~docs ~env)

let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["i"; "inplace"] ~doc ~docs)

let inputs =
  let docv = "SRC" in
  let doc =
    "Input files. At least one is required, and exactly one without \
     --inplace."
  in
  let default = [] in
  mk ~default Arg.(value & pos_all file default & info [] ~doc ~docv ~docs)

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

let validate () =
  if List.is_empty !inputs then
    `Error (false, "Must specify at least one input file.")
  else if !inplace && Option.is_some !name then
    `Error (false, "Cannot specify --name with --inplace")
  else if !inplace && Option.is_some !output then
    `Error (false, "Cannot specify --output with --inplace")
  else if (not !inplace) && List.length !inputs > 1 then
    `Error (false, "Must specify only one input file without --inplace")
  else `Ok ()

;;
parse info validate

let parse_line config ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.version value || !no_version_check then
          Ok config
        else Error (`Bad_value (value, name))
    | name, `File _ -> C.update ~config ~name ~value ~inline:false
    | name, `Attribute -> C.update ~config ~name ~value ~inline:true
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
      ( List.filter
          ~f:(fun s -> not (String.is_empty s))
          (String.split ~on:' ' s)
      , from )
    with
    | ([] | [""]), _ -> impossible "previous match"
    | [name; value], `File (filename, lnum) ->
        (* tolerate space separated [var value] to compatibility with older
           config file format *)
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

let rec read_conf_files conf ~dir =
  let dir' = Filename.dirname dir in
  if (not (String.equal dir dir')) && Caml.Sys.file_exists dir then
    let conf = read_conf_files conf ~dir:dir' in
    try
      let filename = Filename.concat dir ".ocamlformat" in
      In_channel.with_file filename ~f:(fun ic ->
          let c, errors, _ =
            In_channel.fold_lines ic ~init:(conf, [], 1)
              ~f:(fun (conf, errors, num) line ->
                match
                  parse_line conf ~from:(`File (filename, num)) line
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
  else conf

let to_absolute file =
  Filename.(if is_relative file then concat (Unix.getcwd ()) file else file)

let read_config ~filename conf =
  read_conf_files conf ~dir:(Filename.dirname (to_absolute filename))

let config =
  { break_cases= C.get Formatting.break_cases
  ; break_infix= C.get Formatting.break_infix
  ; break_string_literals= C.get Formatting.break_string_literals
  ; break_struct= Poly.(C.get Formatting.break_struct = `Force)
  ; comment_check= C.get comment_check
  ; disable= C.get Formatting.disable
  ; doc_comments= C.get Formatting.doc_comments
  ; escape_chars= C.get Formatting.escape_chars
  ; escape_strings= C.get Formatting.escape_strings
  ; extension_sugar= C.get Formatting.extension_sugar
  ; field_space= C.get Formatting.field_space
  ; if_then_else= C.get Formatting.if_then_else
  ; indicate_nested_or_patterns=
      C.get Formatting.indicate_nested_or_patterns
  ; infix_precedence= C.get Formatting.infix_precedence
  ; leading_nested_match_parens=
      C.get Formatting.leading_nested_match_parens
  ; let_and= C.get Formatting.let_and
  ; let_open= C.get Formatting.let_open
  ; margin= C.get Formatting.margin
  ; max_iters= C.get max_iters
  ; module_item_spacing= C.get Formatting.module_item_spacing
  ; ocp_indent_compat= C.get Formatting.ocp_indent_compat
  ; parens_tuple= C.get Formatting.parens_tuple
  ; quiet= C.get quiet
  ; sequence_style= C.get Formatting.sequence_style
  ; type_decl= C.get Formatting.type_decl
  ; wrap_comments= C.get Formatting.wrap_comments
  ; wrap_fun_args= C.get Formatting.wrap_fun_args }

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

let action =
  if !inplace then
    Inplace
      (List.map !inputs ~f:(fun file ->
           { kind= kind_of file
           ; name= file
           ; file
           ; conf= read_config ~filename:file config } ))
  else
    match !inputs with
    | [input_file] ->
        let name = Option.value !name ~default:input_file in
        In_out
          ( { kind= kind_of name
            ; name
            ; file= input_file
            ; conf= read_config ~filename:name config }
          , !output )
    | _ -> impossible "checked by validate"

and debug = !debug

let parse_line_in_attribute = parse_line ~from:`Attribute
