(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options *)

type fmt_opts =
  { align_pattern_matching_bar: [`Paren | `Keyword]
  ; assignment_operator: [`Begin_line | `End_line]
  ; break_before_in: [`Fit_or_vertical | `Auto]
  ; break_cases: [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; break_infix_before_func: bool
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_separators: [`Before | `After]
  ; break_sequences: bool
  ; break_string_literals: [`Auto | `Never]
  ; break_struct: bool
  ; cases_exp_indent: int
  ; cases_matching_exp_indent: [`Normal | `Compact]
  ; disambiguate_non_breaking_match: bool
  ; doc_comments: [`Before | `Before_except_val | `After_when_possible]
  ; doc_comments_padding: int
  ; doc_comments_tag_only: [`Fit | `Default]
  ; dock_collection_brackets: bool
  ; exp_grouping: [`Parens | `Preserve]
  ; extension_indent: int
  ; field_space: [`Tight | `Loose | `Tight_decl]
  ; function_indent: int
  ; function_indent_nested: [`Always | `Auto | `Never]
  ; if_then_else: [`Compact | `Fit_or_vertical | `Keyword_first | `K_R]
  ; indent_after_in: int
  ; indicate_multiline_delimiters: [`No | `Space | `Closing_on_separate_line]
  ; indicate_nested_or_patterns: [`Space | `Unsafe_no]
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_indent: int
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_module: [`Compact | `Sparse]
  ; line_endings: [`Lf | `Crlf]
  ; margin: int
  ; match_indent: int
  ; match_indent_nested: [`Always | `Auto | `Never]
  ; max_indent: int option
  ; module_item_spacing: [`Compact | `Preserve | `Sparse]
  ; nested_match: [`Wrap | `Align]
  ; ocp_indent_compat: bool
  ; parens_ite: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; parens_tuple_patterns: [`Always | `Multi_line_only]
  ; parse_docstrings: bool
  ; parse_toplevel_phrases: bool
  ; sequence_blank_line: [`Compact | `Preserve_one]
  ; sequence_style: [`Before | `Separator | `Terminator]
  ; single_case: [`Compact | `Sparse]
  ; space_around_arrays: bool
  ; space_around_lists: bool
  ; space_around_records: bool
  ; space_around_variants: bool
  ; stritem_extension_indent: int
  ; type_decl: [`Compact | `Sparse]
  ; type_decl_indent: int
  ; wrap_comments: bool
  ; wrap_fun_args: bool }

type opr_opts =
  { comment_check: bool
  ; debug: bool
  ; disable: bool
  ; margin_check: bool
  ; max_iters: int
  ; ocaml_version: Ocaml_version.t
  ; quiet: bool
  ; range: string -> Range.t }

type t = {fmt_opts: fmt_opts; opr_opts: opr_opts}

let profile_option_names = ["p"; "profile"]

open Cmdliner

let warn_raw, collect_warnings =
  let delay_warning = ref false in
  let delayed_warning_list = ref [] in
  let warn_ s =
    if !delay_warning then delayed_warning_list := s :: !delayed_warning_list
    else Format.eprintf "%s%!" s
  in
  let collect_warnings f =
    let old_flag, old_list = (!delay_warning, !delayed_warning_list) in
    delay_warning := true ;
    delayed_warning_list := [] ;
    let res = f () in
    let collected = List.rev !delayed_warning_list in
    delay_warning := old_flag ;
    delayed_warning_list := old_list ;
    (res, fun () -> List.iter ~f:warn_ collected)
  in
  (warn_, collect_warnings)

let warn ?filename ?lnum fmt =
  Format.kasprintf
    (fun s ->
      let loc : string =
        match (filename, lnum) with
        | Some file, Some lnum ->
            Format.asprintf "File %a, line %d:@\n" Fpath.pp file lnum
        | Some file, None -> Format.asprintf "File %a@\n" Fpath.pp file
        | None, _ -> ""
      in
      warn_raw (Format.asprintf "%sWarning: %s@\n" loc s) )
    fmt

module C = Config_option.Make (struct
  type config = t

  let profile_option_names = profile_option_names

  let warn (config : config) fmt =
    Format.kasprintf
      (fun s -> if not config.opr_opts.quiet then warn "%s" s)
      fmt
end)

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "$(tname) automatically formats OCaml code."
    ; `S (C.section_name C.Formatting `Valid)
    ; `P
        "Unless otherwise noted, any option \
         $(b,--)$(i,option)$(b,=)$(i,VAL) detailed in this section can be \
         set in many ways, its value is determined in the following order \
         (of increasing priority): the default value is used if no other \
         value is specified. The value of a boolean option $(b,--foo) or \
         $(b,--no-foo) can be modified in an $(b,.ocamlformat) \
         configuration file with '$(b,foo = ){$(b,true),$(b,false)}', it \
         can be done for any other option with an '$(b,option = )$(i,VAL)' \
         line (*), or using the OCAMLFORMAT environment variable: \
         $(b,OCAMLFORMAT=)$(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL), \
         or as an optional parameter on the command line, or with a global \
         $(b,[@@@ocamlformat \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute \
         in the processed file, or with an $(b,[@@ocamlformat \
         \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute on expression in \
         the processed file."
    ; `P
        "(*) $(b,.ocamlformat) files in current and all ancestor \
         directories for each input file are used, applied from top to \
         bottom, overriding the settings each time a file is applied, \
         stopping at the project root. If no project root and no \
         $(b,ocamlformat) file has been found, and if the option \
         $(b,enable-outside-detected-project) is set, the global \
         $(b,ocamlformat) file defined in $(b,\\$XDG_CONFIG_HOME) (or in \
         $(b,\\$HOME/.config) if $(b,\\$XDG_CONFIG_HOME) is undefined) is \
         used."
    ; `P
        "If the $(b,disable) option is not set, an $(b,.ocamlformat-ignore) \
         file specifies files that OCamlFormat should ignore. Each line in \
         an $(b,.ocamlformat-ignore) file specifies a filename relative to \
         the directory containing the $(b,.ocamlformat-ignore) file. \
         Shell-style regular expressions are supported. Lines starting with \
         $(b,#) are ignored and can be used as comments."
    ; `P
        "If the $(b,disable) option is set, an $(b,.ocamlformat-enable) \
         file specifies files that OCamlFormat should format even when the \
         $(b,disable) option is set. Each line in an \
         $(b,.ocamlformat-enable) file specifies a filename relative to the \
         directory containing the $(b,.ocamlformat-enable) file. \
         Shell-style regular expressions are supported. Lines starting with \
         $(b,#) are ignored and can be used as comments."
    ; `S (C.section_name C.Operational `Valid)
    ; `P
        "Unless mentioned otherwise non-formatting options cannot be set in \
         attributes or $(b,.ocamlformat) files." ]
  in
  Cmd.info "ocamlformat" ~version:Version.current ~doc ~man

let ocaml_version_conv =
  let parse x =
    match Ocaml_version.of_string x with
    | Ok x -> `Ok x
    | Error (`Msg x) -> `Error x
  in
  (parse, Ocaml_version.pp)

module V = struct
  let v0_12 = Version.make ~major:0 ~minor:12 ~patch:None

  let v0_14 = Version.make ~major:0 ~minor:14 ~patch:None

  let v0_16 = Version.make ~major:0 ~minor:16 ~patch:None

  let v0_17 = Version.make ~major:0 ~minor:17 ~patch:None

  let v0_22 = Version.make ~major:0 ~minor:22 ~patch:None
end

(** Options affecting formatting *)
module Formatting = struct
  let kind = C.Formatting

  let update ~f c = {c with fmt_opts= f c.fmt_opts}

  let ( (* align_cases *) ) =
    let names = ["align-cases"] in
    C.removed_option ~names ~since:V.v0_22 ~msg:""

  let ( (* align_constructors_decl *) ) =
    let names = ["align-constructors-decl"] in
    C.removed_option ~names ~since:V.v0_22 ~msg:""

  let ( (* align_variants_decl *) ) =
    let names = ["align-variants-decl"] in
    C.removed_option ~names ~since:V.v0_22 ~msg:""

  let assignment_operator =
    let doc = "Position of the assignment operator." in
    let names = ["assignment-operator"] in
    let all =
      [ C.Value.make ~name:"end-line" `End_line
          "$(b,end-line) positions assignment operators (`:=` and `<-`) at \
           the end of the line and breaks after it if the whole assignment \
           expression does not fit on a single line."
      ; C.Value.make ~name:"begin-line" `Begin_line
          "$(b,begin-line) positions assignment operators (`:=` and `<-`) \
           at the beginning of the line and breaks before it if the whole \
           assignment expression does not fit on a single line." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with assignment_operator= x}) )
      (fun conf -> conf.fmt_opts.assignment_operator)

  let break_before_in =
    let doc =
      "Whether the line should break before the $(i,in) keyword of a \
       $(i,let) binding."
    in
    let names = ["break-before-in"] in
    let all =
      [ C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) will always break the line before the \
           $(i,in) keyword if the whole $(i,let) binding does not fit on a \
           single line."
      ; C.Value.make ~name:"auto" `Auto
          "$(b,auto) will only break the line if the $(i,in) keyword does \
           not fit on the previous line." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with break_before_in= x}))
      (fun conf -> conf.fmt_opts.break_before_in)

  let break_cases =
    let doc = "Break pattern match cases." in
    let names = ["break-cases"] in
    let all =
      [ C.Value.make ~name:"fit" `Fit
          "Specifying $(b,fit) lets pattern matches break at the margin \
           naturally."
      ; C.Value.make ~name:"nested" `Nested
          "$(b,nested) forces a break after nested or-patterns to highlight \
           the case body. Note that with $(b,nested), the \
           $(b,indicate-nested-or-patterns) option is not needed, and so \
           ignored."
      ; C.Value.make ~name:"toplevel" `Toplevel
          "$(b,toplevel) forces top-level cases (i.e. not nested \
           or-patterns) to break across lines, otherwise break naturally at \
           the margin."
      ; C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) tries to fit all or-patterns on the same \
           line, otherwise breaks."
      ; C.Value.make ~name:"all" `All
          "$(b,all) forces all pattern matches to break across lines." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with break_cases= x}))
      (fun conf -> conf.fmt_opts.break_cases)

  let break_collection_expressions =
    let doc =
      "Break collection expressions (lists and arrays) elements by elements."
    in
    let names = ["break-collection-expressions"] in
    let all =
      [ C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line."
      ; C.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) will group simple expressions and try to format them \
           in a single line." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with break_collection_expressions= x}) )
      (fun conf -> conf.fmt_opts.break_collection_expressions)

  let break_fun_decl =
    let doc = "Style for function declarations and types." in
    let names = ["break-fun-decl"] in
    let all =
      [ C.Value.make ~name:"wrap" `Wrap "$(b,wrap) breaks only if necessary."
      ; C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line."
      ; C.Value.make ~name:"smart" `Smart
          "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with break_fun_decl= x}))
      (fun conf -> conf.fmt_opts.break_fun_decl)

  let break_fun_sig =
    let doc = "Style for function signatures." in
    let names = ["break-fun-sig"] in
    let all =
      [ C.Value.make ~name:"wrap" `Wrap "$(b,wrap) breaks only if necessary."
      ; C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line."
      ; C.Value.make ~name:"smart" `Smart
          "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with break_fun_sig= x}))
      (fun conf -> conf.fmt_opts.break_fun_sig)

  let break_infix =
    let doc = "Break sequence of infix operators." in
    let names = ["break-infix"] in
    let all =
      [ C.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) will group simple expressions and try to format them \
           in a single line."
      ; C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with break_infix= x}))
      (fun conf -> conf.fmt_opts.break_infix)

  let break_infix_before_func =
    let doc =
      "Break infix operators whose right arguments are anonymous functions \
       specially: do not break after the operator so that the first line of \
       the function appears docked at the end of line after the operator."
    in
    let names = ["break-infix-before-func"] in
    C.flag ~default:false ~names ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with break_infix_before_func= x}) )
      (fun conf -> conf.fmt_opts.break_infix_before_func)

  let break_separators =
    let doc =
      "Break before or after separators such as `;` in list or record \
       expressions."
    in
    let names = ["break-separators"] in
    let all =
      [ C.Value.make ~name:"after" `After
          "$(b,after) breaks the expressions after the separator."
      ; C.Value.make ~name:"before" `Before
          "$(b,before) breaks the expressions before the separator." ]
    in
    C.choice ~names ~all ~doc ~kind
      ~removed_values:
        [ C.Value_removed.make ~name:"after-and-docked" ~since:V.v0_12
            ~msg:
              "One can get a similar behaviour by setting \
               `break-separators=after`, `space-around-lists=false`, and \
               `dock-collection-brackets=false`." ]
      (fun conf x -> update conf ~f:(fun f -> {f with break_separators= x}))
      (fun conf -> conf.fmt_opts.break_separators)

  let break_sequences =
    let doc =
      "Force sequence expressions to break irrespective of margin."
    in
    let names = ["break-sequences"] in
    C.flag ~default:true ~names ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with break_sequences= x}))
      (fun conf -> conf.fmt_opts.break_sequences)

  let break_string_literals =
    let doc = "Break string literals." in
    let names = ["break-string-literals"] in
    let all =
      [ C.Value.make ~name:"auto" `Auto
          "$(b,auto) mode breaks lines at newlines and wraps string \
           literals at the margin."
      ; C.Value.make ~name:"never" `Never
          "$(b,never) mode formats string literals as they are parsed, in \
           particular, with escape sequences expanded." ]
    in
    C.choice ~names ~all ~doc ~kind
      ~removed_values:
        (C.Value_removed.make_list
           ~names:["newlines"; "newlines-and-wrap"; "wrap"]
           ~since:V.v0_12
           ~msg:
             "It has been replaced by the new default `auto` value, which \
              breaks lines at newlines and wraps string literals at the \
              margin." )
      (fun conf x ->
        update conf ~f:(fun f -> {f with break_string_literals= x}) )
      (fun conf -> conf.fmt_opts.break_string_literals)

  let break_struct =
    let doc = "Break struct-end module items." in
    let names = ["break-struct"] in
    let all =
      [ C.Value.make ~name:"force" `Force
          "$(b,force) will break struct-end phrases unconditionally."
      ; C.Value.make ~name:"natural" `Natural
          "$(b,natural) will break struct-end phrases naturally at the \
           margin." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with break_struct= Poly.(x = `Force)}) )
      (fun conf -> if conf.fmt_opts.break_struct then `Force else `Natural)

  let cases_exp_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of cases expressions ($(docv) columns). See also the \
       $(b,cases-matching-exp-indent) and $(b,nested-match) options."
    in
    let names = ["cases-exp-indent"] in
    C.any Arg.int ~names ~default:4 ~doc ~docv ~kind ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with cases_exp_indent= x}))
      (fun conf -> conf.fmt_opts.cases_exp_indent)

  let cases_matching_exp_indent =
    let doc =
      "Indentation of cases right-hand sides which are `match` or `try` \
       expressions."
    in
    let names = ["cases-matching-exp-indent"] in
    let all =
      [ C.Value.make ~name:"normal" `Normal
          "$(b,normal) indents as it would any other expression."
      ; C.Value.make ~name:"compact" `Compact
          "$(b,compact) forces an indentation of 2, unless \
           $(b,nested-match) is set to $(b,align) and we're on the last \
           case." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with cases_matching_exp_indent= x}) )
      (fun conf -> conf.fmt_opts.cases_matching_exp_indent)

  let disambiguate_non_breaking_match =
    let doc =
      "Add parentheses around matching constructs that fit on a single line."
    in
    C.flag
      ~names:["disambiguate-non-breaking-match"]
      ~default:false ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with disambiguate_non_breaking_match= x})
        )
      (fun conf -> conf.fmt_opts.disambiguate_non_breaking_match)

  let doc_comments =
    let doc = "Doc comments position." in
    let names = ["doc-comments"] in
    let all =
      [ C.Value.make ~name:"after-when-possible" `After_when_possible
          "$(b,after-when-possible) puts doc comments after the \
           corresponding code. This option has no effect on variant \
           declarations because that would change their meaning and on \
           structures, signatures and objects for readability."
      ; C.Value.make ~name:"before-except-val" `Before_except_val
          "$(b,before-except-val) puts doc comments before the \
           corresponding code, but puts doc comments of $(b,val) and \
           $(b,external) declarations after the corresponding declarations."
      ; C.Value.make ~name:"before" `Before
          "$(b,before) puts comments before the corresponding code." ]
    in
    C.choice ~names ~all ~doc ~kind
      ~removed_values:
        [ C.Value_removed.make ~name:"after" ~since:V.v0_14
            ~msg:
              "This value has been renamed `after-when-possible` to take \
               into account the technical limitations of ocamlformat, the \
               behavior is unchanged." ]
      (fun conf x -> update conf ~f:(fun f -> {f with doc_comments= x}))
      (fun conf -> conf.fmt_opts.doc_comments)

  let doc_comments_padding =
    let docv = "PADDING" in
    let doc =
      "Add $(docv) spaces before doc comments in type declarations."
    in
    let names = ["doc-comments-padding"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with doc_comments_padding= x}) )
      (fun conf -> conf.fmt_opts.doc_comments_padding)

  let doc_comments_tag_only =
    let doc = "Position of doc comments with only tags." in
    let names = ["doc-comments-tag-only"] in
    let all =
      [ C.Value.make ~name:"default" `Default
          "$(b,default) means no special treatment."
      ; C.Value.make ~name:"fit" `Fit
          "$(b,fit) puts doc comments on the same line." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with doc_comments_tag_only= x}) )
      (fun conf -> conf.fmt_opts.doc_comments_tag_only)

  let ( (* doc_comments_val *) ) =
    let names = ["doc-comments-val"] in
    let msg =
      "If you are using `doc-comments-val=before` in combination with \
       `doc-comments=before` then only `doc-comments=before` is now \
       required to achive the same behavior. If you are using \
       `doc-comments-val=before` in combination with `doc-comments=after` \
       this behavior is not available anymore. If you are using \
       `doc-comments-val=after` in combination with `doc-comments=before` \
       please now use `doc-comments=before-except-val`. If you are using \
       `doc-comments-val=after` in combination with `doc-comments=after` \
       then only `doc-comments=after-when-possible` is now required to \
       achieve the same behavior. If you are using `doc-comments-val=unset` \
       the same behavior can now be achieved by setting `doc-comments` \
       only."
    in
    C.removed_option ~names ~since:V.v0_16 ~msg

  let dock_collection_brackets =
    let doc =
      "Dock the brackets of lists, arrays and records, so that when the \
       collection does not fit on a single line the brackets are opened on \
       the preceding line and closed on the following line."
    in
    let names = ["dock-collection-brackets"] in
    C.flag ~default:true ~names ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with dock_collection_brackets= x}) )
      (fun conf -> conf.fmt_opts.dock_collection_brackets)

  let concrete_syntax_preserved_msg =
    "Concrete syntax will now always be preserved."

  let ( (* escape_chars *) ) =
    let names = ["escape-chars"] in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~since:V.v0_16 ~msg

  let ( (* escape_strings *) ) =
    let names = ["escape-strings"] in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~since:V.v0_16 ~msg

  let exp_grouping =
    let doc = "Style of expression grouping." in
    let names = ["exp-grouping"] in
    let all =
      [ C.Value.make ~name:"parens" `Parens
          "$(b,parens) groups expressions using parentheses."
      ; C.Value.make ~name:"preserve" `Preserve
          "$(b,preserve) preserves the original grouping syntax \
           (parentheses or $(i,begin)/$(i,end))." ]
    in
    C.choice ~names ~all ~doc ~kind ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with exp_grouping= x}))
      (fun conf -> conf.fmt_opts.exp_grouping)

  let extension_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of items inside extension nodes ($(docv) columns)."
    in
    let names = ["extension-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with extension_indent= x}))
      (fun conf -> conf.fmt_opts.extension_indent)

  let ( (* extension_sugar *) ) =
    let names = ["extension-sugar"] in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~since:V.v0_17 ~msg

  let field_space =
    let doc =
      "Whether or not to use a space between a field name and the \
       punctuation symbol (`:` or `=`) preceding the rhs. This option \
       affects records and objects."
    in
    let names = ["field-space"] in
    let all =
      [ C.Value.make ~name:"loose" `Loose "$(b,loose) uses a space."
      ; C.Value.make ~name:"tight" `Tight "$(b,tight) does not use a space."
      ; C.Value.make ~name:"tight-decl" `Tight_decl
          "$(b,tight-decl) is $(b,tight) for declarations and $(b,loose) \
           for instantiations." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with field_space= x}))
      (fun conf -> conf.fmt_opts.field_space)

  let function_indent =
    let docv = "COLS" in
    let doc = "Indentation of function cases ($(docv) columns)." in
    let names = ["function-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with function_indent= x}))
      (fun conf -> conf.fmt_opts.function_indent)

  let function_indent_nested =
    let doc =
      "Whether the $(b,function-indent) parameter should be applied even \
       when in a sub-block."
    in
    let names = ["function-indent-nested"] in
    let all =
      [ C.Value.make ~name:"never" `Never
          "$(b,never) only applies $(b,function-indent) if the function \
           block starts a line."
      ; C.Value.make ~name:"always" `Always
          "$(b,always) always apply $(b,function-indent)."
      ; C.Value.make ~name:"auto" `Auto
          "$(b,auto) applies $(b,function-indent) when seen fit." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with function_indent_nested= x}) )
      (fun conf -> conf.fmt_opts.function_indent_nested)

  let if_then_else =
    let doc = "If-then-else formatting." in
    let names = ["if-then-else"] in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) tries to format an if-then-else expression on a \
           single line."
      ; C.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks branches if they do not \
           fit on a single line."
      ; C.Value.make ~name:"keyword-first" `Keyword_first
          "$(b,keyword-first) formats if-then-else expressions such that \
           the if-then-else keywords are the first on the line."
      ; C.Value.make ~name:"k-r" `K_R
          "$(b,k-r) formats if-then-else expressions with parentheses that \
           match the K&R style." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with if_then_else= x}))
      (fun conf -> conf.fmt_opts.if_then_else)

  let indent_after_in =
    let docv = "COLS" in
    let doc =
      "Indentation ($(docv) columns) after `let ... in`, unless followed by \
       another `let`."
    in
    let names = ["indent-after-in"] in
    C.any Arg.int ~names ~default:0 ~doc ~docv ~kind ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with indent_after_in= x}))
      (fun conf -> conf.fmt_opts.indent_after_in)

  let indicate_multiline_delimiters =
    let doc =
      "How to indicate that two matching delimiters live on different lines."
    in
    let names = ["indicate-multiline-delimiters"] in
    let all =
      [ C.Value.make ~name:"no" `No
          "$(b, no) doesn't do anything special to indicate the closing \
           delimiter."
      ; C.Value.make ~name:"space" `Space
          "$(b,space) prints a space inside the delimiter to indicate the \
           matching one is on a different line."
      ; C.Value.make ~name:"closing-on-separate-line"
          `Closing_on_separate_line
          "$(b, closing-on-separate-line) makes sure that the closing \
           delimiter is on its own line." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with indicate_multiline_delimiters= x})
        )
      (fun conf -> conf.fmt_opts.indicate_multiline_delimiters)

  let indicate_nested_or_patterns =
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let names = ["indicate-nested-or-patterns"] in
    let all =
      [ C.Value.make ~name:"unsafe-no" `Unsafe_no
          "$(b,unsafe-no) does not indicate nested or-patterns. Warning: \
           this can produce confusing code where a short body of a match \
           case is visually hidden by surrounding long patterns, leading to \
           misassociation between patterns and body expressions."
      ; C.Value.make ~name:"space" `Space
          "$(b,space) starts lines of nested or-patterns with \" |\" rather \
           than \"| \"." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with indicate_nested_or_patterns= x}) )
      (fun conf -> conf.fmt_opts.indicate_nested_or_patterns)

  let infix_precedence =
    let doc =
      "Use indentation or also discretionary parentheses to explicitly \
       disambiguate precedences of infix operators."
    in
    let names = ["infix-precedence"] in
    let all =
      [ C.Value.make ~name:"indent" `Indent
          "$(b,indent) uses indentation to explicitly disambiguate \
           precedences of infix operators."
      ; C.Value.make ~name:"parens" `Parens
          "$(b,parens) uses parentheses to explicitly disambiguate \
           precedences of infix operators." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with infix_precedence= x}))
      (fun conf -> conf.fmt_opts.infix_precedence)

  let leading_nested_match_parens =
    let doc = "Nested match parens formatting." in
    let names = ["leading-nested-match-parens"] in
    C.flag ~default:false ~names ~doc ~kind ~allow_inline:false
      (fun conf x ->
        update conf ~f:(fun f -> {f with leading_nested_match_parens= x}) )
      (fun conf -> conf.fmt_opts.leading_nested_match_parens)

  let let_and =
    let doc = "Style of let_and." in
    let names = ["let-and"] in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format `let p = e and p = e` in a \
           single line."
      ; C.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break between them." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with let_and= x}))
      (fun conf -> conf.fmt_opts.let_and)

  let let_binding_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of let binding expressions ($(docv) columns) if they do \
       not fit on a single line."
    in
    let names = ["let-binding-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~kind ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with let_binding_indent= x}))
      (fun conf -> conf.fmt_opts.let_binding_indent)

  let let_binding_spacing =
    let doc = "Spacing between let binding." in
    let names = ["let-binding-spacing"] in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) spacing separates adjacent let bindings in a module \
           according to module-item-spacing."
      ; C.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) places two open lines between a multi-line \
           module-level let binding and the next."
      ; C.Value.make ~name:"double-semicolon" `Double_semicolon
          "$(b,double-semicolon) places double semicolons and an open line \
           between a multi-line module-level let binding and the next." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with let_binding_spacing= x}) )
      (fun conf -> conf.fmt_opts.let_binding_spacing)

  let let_module =
    let doc = "Module binding formatting." in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) does not break a line after the $(i,let module ... \
           =) and before the $(i,in) if the module declaration does not fit \
           on a single line."
      ; C.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) breaks a line after $(i,let module ... =) and before \
           the $(i,in) if the module declaration does not fit on a single \
           line." ]
    in
    C.choice ~names:["let-module"] ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with let_module= x}))
      (fun conf -> conf.fmt_opts.let_module)

  let ( (* let_open *) ) =
    let names = ["let-open"] in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~since:V.v0_17 ~msg

  let line_endings =
    let doc = "Line endings used." in
    let all =
      [ C.Value.make ~name:"lf" `Lf "$(b,lf) uses Unix line endings."
      ; C.Value.make ~name:"crlf" `Crlf
          "$(b,crlf) uses Windows line endings." ]
    in
    C.choice ~names:["line-endings"] ~all ~doc ~allow_inline:false ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with line_endings= x}))
      (fun conf -> conf.fmt_opts.line_endings)

  let margin =
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    C.any Arg.int ~names:["m"; "margin"] ~default:80 ~doc ~docv ~kind
      ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with margin= x}))
      (fun conf -> conf.fmt_opts.margin)

  let match_indent =
    let docv = "COLS" in
    let doc = "Indentation of match/try cases ($(docv) columns)." in
    let names = ["match-indent"] in
    C.any Arg.int ~names ~default:0 ~doc ~docv ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with match_indent= x}))
      (fun conf -> conf.fmt_opts.match_indent)

  let match_indent_nested =
    let doc =
      "Whether the $(b,match-indent) parameter should be applied even when \
       in a sub-block."
    in
    let names = ["match-indent-nested"] in
    let all =
      [ C.Value.make ~name:"never" `Never
          "$(b,never) only applies $(b,match-indent) if the match block \
           starts a line."
      ; C.Value.make ~name:"always" `Always
          "$(b,always) always apply $(b,match-indent)."
      ; C.Value.make ~name:"auto" `Auto
          "$(b,auto) applies $(b,match-indent) when seen fit." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with match_indent_nested= x}) )
      (fun conf -> conf.fmt_opts.match_indent_nested)

  let default_max_indent =
    (* Creating a fresh formatter in case the value of max-indent has been
       changed for stdout. *)
    let fs = Format.formatter_of_buffer (Buffer.create 0) in
    Int.to_string (Format.pp_get_max_indent fs ())

  let max_indent =
    let docv = "COLS" in
    let doc =
      "Maximum offset ($(docv) columns) added to a new line in addition to \
       the offset of the previous line."
    in
    C.any
      Arg.(some ~none:default_max_indent int)
      ~names:["max-indent"] ~doc ~docv ~kind ~default:None
      ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with max_indent= x}))
      (fun conf -> conf.fmt_opts.max_indent)

  let module_item_spacing =
    let doc = "Spacing between items of structures and signatures." in
    let names = ["module-item-spacing"] in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) will not leave open lines between one-liners of \
           similar sorts."
      ; C.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break a line between two items."
      ; C.Value.make ~name:"preserve" `Preserve
          "$(b,preserve) will not leave open lines between one-liners of \
           similar sorts unless there is an open line in the input." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with module_item_spacing= x}) )
      (fun conf -> conf.fmt_opts.module_item_spacing)

  let nested_match =
    let doc =
      "Style of a pattern-matching nested in the last case of another \
       pattern-matching."
    in
    let names = ["nested-match"] in
    let all =
      [ C.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) wraps the nested pattern-matching with parentheses and \
           adds indentation."
      ; C.Value.make ~name:"align" `Align
          "$(b,align) vertically aligns the nested pattern-matching under \
           the encompassing pattern-matching." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with nested_match= x}))
      (fun conf -> conf.fmt_opts.nested_match)

  let ocp_indent_compat =
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let names = ["ocp-indent-compat"] in
    C.flag ~default:false ~names ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with ocp_indent_compat= x}))
      (fun conf -> conf.fmt_opts.ocp_indent_compat)

  let parens_ite =
    let doc =
      "Uses parentheses around if-then-else branches that spread across \
       multiple lines."
    in
    let names = ["parens-ite"] in
    C.flag ~default:false ~names ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with parens_ite= x}))
      (fun conf -> conf.fmt_opts.parens_ite)

  let parens_tuple =
    let doc = "Parens tuple expressions." in
    let names = ["parens-tuple"] in
    let all =
      [ C.Value.make ~name:"always" `Always
          "$(b,always) always uses parentheses around tuples."
      ; C.Value.make ~name:"multi-line-only" `Multi_line_only
          "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuples." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with parens_tuple= x}))
      (fun conf -> conf.fmt_opts.parens_tuple)

  let parens_tuple_patterns =
    let doc = "Parens tuple patterns." in
    let names = ["parens-tuple-patterns"] in
    let all =
      [ C.Value.make ~name:"multi-line-only" `Multi_line_only
          "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuple patterns."
      ; C.Value.make ~name:"always" `Always
          "$(b,always) always uses parentheses around tuples patterns." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with parens_tuple_patterns= x}) )
      (fun conf -> conf.fmt_opts.parens_tuple_patterns)

  let parse_docstrings =
    let doc = "Parse and format docstrings." in
    let names = ["parse-docstrings"] in
    C.flag ~default:false ~names ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with parse_docstrings= x}))
      (fun conf -> conf.fmt_opts.parse_docstrings)

  let parse_toplevel_phrases =
    let doc = "Parse and format toplevel phrases and their output." in
    let names = ["parse-toplevel-phrases"] in
    C.flag ~default:false ~names ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with parse_toplevel_phrases= x}) )
      (fun conf -> conf.fmt_opts.parse_toplevel_phrases)

  let sequence_blank_line =
    let doc = "Blank line between expressions of a sequence." in
    let names = ["sequence-blank-line"] in
    let all =
      [ C.Value.make ~name:"preserve-one" `Preserve_one
          "$(b,preserve) will keep a blank line between two expressions of \
           a sequence if the input contains at least one."
      ; C.Value.make ~name:"compact" `Compact
          "$(b,compact) will not keep any blank line between expressions of \
           a sequence." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with sequence_blank_line= x}) )
      (fun conf -> conf.fmt_opts.sequence_blank_line)

  let sequence_style =
    let doc = "Style of sequence." in
    let names = ["sequence-style"] in
    let all =
      [ C.Value.make ~name:"terminator" `Terminator
          "$(b,terminator) only puts spaces after semicolons."
      ; C.Value.make ~name:"separator" `Separator
          "$(b,separator) puts spaces before and after semicolons."
      ; C.Value.make ~name:"before" `Before
          "$(b,before) breaks the sequence before semicolons." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with sequence_style= x}))
      (fun conf -> conf.fmt_opts.sequence_style)

  let single_case =
    let doc =
      "Style of pattern matching expressions with only a single case."
    in
    let names = ["single-case"] in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format a single case on a single line."
      ; C.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break the line before a single case." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with single_case= x}))
      (fun conf -> conf.fmt_opts.single_case)

  let space_around_arrays =
    let doc = "Add a space inside the delimiters of arrays." in
    let names = ["space-around-arrays"] in
    C.flag ~default:true ~names ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with space_around_arrays= x}) )
      (fun conf -> conf.fmt_opts.space_around_arrays)

  let space_around_lists =
    let doc = "Add a space inside the delimiters of lists." in
    let names = ["space-around-lists"] in
    C.flag ~default:true ~names ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with space_around_lists= x}))
      (fun conf -> conf.fmt_opts.space_around_lists)

  let space_around_records =
    let doc = "Add a space inside the delimiters of records." in
    let names = ["space-around-records"] in
    C.flag ~default:true ~names ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with space_around_records= x}) )
      (fun conf -> conf.fmt_opts.space_around_records)

  let space_around_variants =
    let doc = "Add a space inside the delimiters of variants." in
    let names = ["space-around-variants"] in
    C.flag ~default:true ~names ~doc ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with space_around_variants= x}) )
      (fun conf -> conf.fmt_opts.space_around_variants)

  let stritem_extension_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of structure items inside extension nodes ($(docv) \
       columns)."
    in
    let names = ["stritem-extension-indent"] in
    C.any Arg.int ~names ~default:0 ~doc ~docv ~kind
      (fun conf x ->
        update conf ~f:(fun f -> {f with stritem_extension_indent= x}) )
      (fun conf -> conf.fmt_opts.stritem_extension_indent)

  let type_decl =
    let doc = "Style of type declaration." in
    let names = ["type-decl"] in
    let all =
      [ C.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format constructors and records \
           definition in a single line."
      ; C.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break between constructors and record \
           fields." ]
    in
    C.choice ~names ~all ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with type_decl= x}))
      (fun conf -> conf.fmt_opts.type_decl)

  let type_decl_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of type declarations ($(docv) columns) if they do not \
       fit on a single line."
    in
    let names = ["type-decl-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~kind ~allow_inline:false
      (fun conf x -> update conf ~f:(fun f -> {f with type_decl_indent= x}))
      (fun conf -> conf.fmt_opts.type_decl_indent)

  let wrap_comments =
    let doc =
      "Wrap comments and docstrings. Comments and docstrings are divided \
       into paragraphs by open lines (two or more consecutive newlines), \
       and each paragraph is wrapped at the margin. Multi-line comments \
       with vertically-aligned asterisks on the left margin are not \
       wrapped. Consecutive comments with both left and right margin \
       aligned are not wrapped either."
    in
    C.flag ~default:false ~names:["wrap-comments"] ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with wrap_comments= x}))
      (fun conf -> conf.fmt_opts.wrap_comments)

  let wrap_fun_args =
    let default = true in
    let doc = "Style for function call." in
    let names = ["wrap-fun-args"] in
    C.flag ~default ~names ~doc ~kind
      (fun conf wrap_fun_args ->
        update conf ~f:(fun f -> {f with wrap_fun_args}) )
      (fun conf -> conf.fmt_opts.wrap_fun_args)
end

(* Flags that can be modified in the config file that don't affect
   formatting *)

let kind = C.Operational

let docs = C.section_name kind `Valid

module Operational = struct
  let update ~f c = {c with opr_opts= f c.opr_opts}

  let comment_check =
    let default = true in
    let doc =
      "Control whether to check comments and documentation comments. Unsafe \
       to turn off. May be set in $(b,.ocamlformat)."
    in
    C.flag ~default ~names:["comment-check"] ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with comment_check= x}))
      (fun conf -> conf.opr_opts.comment_check)

  let debug =
    let doc = "Generate debugging output." in
    let default = false in
    C.flag ~default ~names:["g"; "debug"] ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with debug= x}))
      (fun conf -> conf.opr_opts.debug)

  let disable =
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(b,[@@@ocamlformat \
       \"enable\"]) instead of $(b,[@@@ocamlformat \"disable=false\"])."
    in
    C.flag ~names:["disable"] ~default:false ~doc ~kind ~allow_inline:true
      (fun conf x -> update conf ~f:(fun f -> {f with disable= x}))
      (fun conf -> conf.opr_opts.disable)

  let margin_check =
    let doc = "Emit a warning if the formatted output exceeds the margin." in
    C.flag ~default:false ~names:["margin-check"] ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with margin_check= x}))
      (fun conf -> conf.opr_opts.margin_check)

  let max_iters =
    let docv = "N" in
    let doc =
      "Fail if output of formatting does not stabilize within $(docv) \
       iterations. May be set in $(b,.ocamlformat)."
    in
    C.any Arg.int ~names:["n"; "max-iters"] ~default:10 ~doc ~docv ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with max_iters= x}))
      (fun conf -> conf.opr_opts.max_iters)

  let ocaml_version =
    let docv = "V" in
    let doc = "Version of OCaml syntax of the output." in
    let default = Ocaml_version.Releases.v4_04_0 in
    C.any ocaml_version_conv ~names:["ocaml-version"] ~default ~doc ~docv
      ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with ocaml_version= x}))
      (fun conf -> conf.opr_opts.ocaml_version)

  let quiet =
    let doc = "Quiet. May be set in $(b,.ocamlformat)." in
    C.flag ~default:false ~names:["q"; "quiet"] ~doc ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with quiet= x}))
      (fun conf -> conf.opr_opts.quiet)

  let range =
    let doc =
      "Apply the formatting to a range of lines. Must be included between 1 \
       and the number of lines of the input. If a range is invalid the \
       whole input is considered. Warning: only supported in conbination \
       with `--numeric` for now."
    in
    let default = Range.make ?range:None in
    let docv = "X-Y" in
    C.any Range.conv ~names:["range"] ~default ~doc ~docv ~kind
      (fun conf x -> update conf ~f:(fun f -> {f with range= x}))
      (fun conf -> conf.opr_opts.range)
end

let disable_conf_attrs =
  let doc = "Disable configuration in attributes." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-attrs"] ~doc ~docs)

let disable_conf_files =
  let doc = "Disable .ocamlformat configuration files." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-files"] ~doc ~docs)

let ( (* disable_outside_detected_project *) ) =
  let msg =
    "OCamlFormat is disabled outside of a detected project by default, to \
     enable the opposite behavior use `enable-outside-detected-project`."
  in
  let names = ["disable-outside-detected-project"] in
  C.removed_option ~names ~since:V.v0_22 ~msg

let enable_outside_detected_project =
  let witness =
    String.concat ~sep:" or "
      (List.map File_system.project_root_witness ~f:(fun name ->
           Format.sprintf "$(b,%s)" name ) )
  in
  let doc =
    Format.sprintf
      "Read $(b,.ocamlformat) config files outside the current project when \
       no project root has been detected for the input file. The project \
       root of an input file is taken to be the nearest ancestor directory \
       that contains a %s file. If $(b,.ocamlformat) config files are \
       located in the same directory or parents they are applied, if no \
       $(b,.ocamlformat) file is found then the global configuration \
       defined in $(b,\\$XDG_CONFIG_HOME/.ocamlformat) (or in \
       $(b,\\$HOME/.config/.ocamlformat) if $(b,\\$XDG_CONFIG_HOME) is \
       undefined) is applied."
      witness
  in
  let default = false in
  mk ~default
    Arg.(value & flag & info ["enable-outside-detected-project"] ~doc ~docs)

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
  let env = Cmd.Env.info "OCAMLFORMAT" in
  let default = [] in
  let assoc = Arg.(pair ~sep:'=' string string) in
  let list_assoc = Arg.(list ~sep:',' assoc) in
  mk ~default
    Arg.(
      value & opt list_assoc default & info ["c"; "config"] ~doc ~docs ~env )

let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["i"; "inplace"] ~doc ~docs)

type file = Stdin | File of string

let inputs =
  let docv = "SRC" in
  let file_or_dash =
    let parse, print = Arg.non_dir_file in
    let print fmt = function
      | Stdin -> print fmt "<standard input>"
      | File x -> print fmt x
    in
    let parse = function
      | "-" -> `Ok Stdin
      | s -> (
        match parse s with `Ok x -> `Ok (File x) | `Error x -> `Error x )
    in
    (parse, print)
  in
  let doc =
    "Input files. At least one is required, and exactly one without \
     $(b,--inplace). If $(b,-) is passed, will read from stdin."
  in
  let default = [] in
  mk ~default
    Arg.(value & pos_all file_or_dash default & info [] ~doc ~docv ~docs)

let kind : Syntax.t option ref =
  let doc = "Parse input as an implementation." in
  let impl = (Some Syntax.Use_file, Arg.info ["impl"] ~doc ~docs) in
  let doc = "Parse input as an interface." in
  let intf = (Some Syntax.Signature, Arg.info ["intf"] ~doc ~docs) in
  let doc = "Deprecated. Same as $(b,impl)." in
  let use_file = (Some Syntax.Use_file, Arg.info ["use-file"] ~doc ~docs) in
  let doc = "Parse input as toplevel phrases with their output." in
  let repl_file =
    (Some Syntax.Repl_file, Arg.info ["repl-file"] ~doc ~docs)
  in
  let default = None in
  mk ~default Arg.(value & vflag default [impl; intf; use_file; repl_file])

let name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting and starting point when \
     searching for '.ocamlformat' files. Defaults to the input file name. \
     Some options can be specified in configuration files named \
     '.ocamlformat' in the same or a parent directory of $(docv), see \
     documentation of other options for details."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some string) default & info ["name"] ~doc ~docs ~docv)

let numeric =
  let doc =
    "Instead of re-formatting the file, output one integer per line \
     corresponding to the indentation value, printing as many values as \
     lines in the document."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["numeric"] ~doc ~docs)

let ocp_indent_options_doc =
  let alias ocp_indent ocamlformat =
    Printf.sprintf "$(b,%s) is an alias for $(b,%s)." ocp_indent ocamlformat
  in
  let multi_alias ocp_indent l_ocamlformat =
    Format.asprintf "$(b,%s) sets %a." ocp_indent
      (Format.pp_print_list
         ~pp_sep:(fun fs () -> Format.fprintf fs " and ")
         (fun fs x -> Format.fprintf fs "$(b,%s)" x) )
      l_ocamlformat
  in
  [ alias "base" "let-binding-indent"
  ; alias "type" "type-decl-indent"
  ; alias "in" "indent-after-in"
  ; multi_alias "with" ["function-indent"; "match-indent"]
  ; alias "match_clause" "cases-exp-indent"
  ; alias "ppx_stritem_ext" "stritem-extension-indent"
  ; alias "max_indent" "max-indent"
  ; multi_alias "strict_with"
      ["function-indent-nested"; "match-indent-nested"] ]

let ocp_indent_config =
  let doc =
    let open Format in
    let supported =
      match ocp_indent_options_doc with
      | [] -> ""
      | docs ->
          asprintf " %a"
            (pp_print_list
               ~pp_sep:(fun fs () -> fprintf fs "@ ")
               (fun fs s -> fprintf fs "%s" s) )
            docs
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
      & info ["o"; "output"] ~doc ~docs ~docv )

let print_config =
  let doc =
    "Print the configuration determined by the environment variable, the \
     configuration files, preset profiles and command line. Attributes are \
     not considered. If many input files are specified, only print the \
     configuration for the first file. If no input file is specified, print \
     the configuration for the root directory if specified, or for the \
     current working directory otherwise."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["print-config"] ~doc ~docs)

let root =
  let docv = "DIR" in
  let doc =
    "Root of the project. If specified, only take into account .ocamlformat \
     configuration files inside $(docv) and its subdirectories."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some dir) default & info ["root"] ~doc ~docs ~docv)

let no_version_check =
  let doc =
    "Do not check that the version matches the one specified in \
     .ocamlformat."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-version-check"] ~doc ~docs)

let ignore_invalid_options =
  let doc = "Ignore invalid options (e.g. in .ocamlformat)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["ignore-invalid-option"] ~doc ~docs)

let ocamlformat_profile =
  { align_pattern_matching_bar= `Paren
  ; assignment_operator= `End_line
  ; break_before_in= `Fit_or_vertical
  ; break_cases= `Nested
  ; break_collection_expressions= `Fit_or_vertical
  ; break_infix= `Wrap
  ; break_infix_before_func= true
  ; break_fun_decl= `Wrap
  ; break_fun_sig= `Wrap
  ; break_separators= `Before
  ; break_sequences= false
  ; break_string_literals= `Auto
  ; break_struct= true
  ; cases_exp_indent= 4
  ; cases_matching_exp_indent= `Compact
  ; disambiguate_non_breaking_match= false
  ; doc_comments= `Before_except_val
  ; doc_comments_padding= 2
  ; doc_comments_tag_only= `Default
  ; dock_collection_brackets= false
  ; exp_grouping= `Parens
  ; extension_indent= 2
  ; field_space= `Tight
  ; function_indent= 2
  ; function_indent_nested= `Never
  ; if_then_else= `Compact
  ; indent_after_in= 0
  ; indicate_multiline_delimiters= `Space
  ; indicate_nested_or_patterns= `Space
  ; infix_precedence= `Indent
  ; leading_nested_match_parens= false
  ; let_and= `Compact
  ; let_binding_indent= 2
  ; let_binding_spacing= `Compact
  ; let_module= `Compact
  ; line_endings= `Lf
  ; margin= 80
  ; match_indent= 0
  ; match_indent_nested= `Never
  ; max_indent= None
  ; module_item_spacing= `Sparse
  ; nested_match= `Wrap
  ; ocp_indent_compat= false
  ; parens_ite= false
  ; parens_tuple= `Always
  ; parens_tuple_patterns= `Multi_line_only
  ; parse_docstrings= false
  ; parse_toplevel_phrases= false
  ; sequence_blank_line= `Compact
  ; sequence_style= `Separator
  ; single_case= `Compact
  ; space_around_arrays= false
  ; space_around_lists= false
  ; space_around_records= false
  ; space_around_variants= false
  ; stritem_extension_indent= 0
  ; type_decl= `Compact
  ; type_decl_indent= 2
  ; wrap_comments= false
  ; wrap_fun_args= true }

let conventional_profile =
  { align_pattern_matching_bar= `Paren
  ; assignment_operator= C.default Formatting.assignment_operator
  ; break_before_in= C.default Formatting.break_before_in
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
  ; cases_matching_exp_indent= C.default Formatting.cases_matching_exp_indent
  ; disambiguate_non_breaking_match=
      C.default Formatting.disambiguate_non_breaking_match
  ; doc_comments= C.default Formatting.doc_comments
  ; doc_comments_padding= C.default Formatting.doc_comments_padding
  ; doc_comments_tag_only= C.default Formatting.doc_comments_tag_only
  ; dock_collection_brackets= C.default Formatting.dock_collection_brackets
  ; exp_grouping= C.default Formatting.exp_grouping
  ; extension_indent= C.default Formatting.extension_indent
  ; field_space= C.default Formatting.field_space
  ; function_indent= C.default Formatting.function_indent
  ; function_indent_nested= C.default Formatting.function_indent_nested
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
  ; line_endings= C.default Formatting.line_endings
  ; margin= C.default Formatting.margin
  ; match_indent= C.default Formatting.match_indent
  ; match_indent_nested= C.default Formatting.match_indent_nested
  ; max_indent= C.default Formatting.max_indent
  ; module_item_spacing= C.default Formatting.module_item_spacing
  ; nested_match= C.default Formatting.nested_match
  ; ocp_indent_compat= C.default Formatting.ocp_indent_compat
  ; parens_ite= C.default Formatting.parens_ite
  ; parens_tuple= C.default Formatting.parens_tuple
  ; parens_tuple_patterns= C.default Formatting.parens_tuple_patterns
  ; parse_docstrings= C.default Formatting.parse_docstrings
  ; parse_toplevel_phrases= C.default Formatting.parse_toplevel_phrases
  ; sequence_blank_line= C.default Formatting.sequence_blank_line
  ; sequence_style= C.default Formatting.sequence_style
  ; single_case= C.default Formatting.single_case
  ; space_around_arrays= C.default Formatting.space_around_arrays
  ; space_around_lists= C.default Formatting.space_around_lists
  ; space_around_records= C.default Formatting.space_around_records
  ; space_around_variants= C.default Formatting.space_around_variants
  ; stritem_extension_indent= C.default Formatting.stritem_extension_indent
  ; type_decl= C.default Formatting.type_decl
  ; type_decl_indent= C.default Formatting.type_decl_indent
  ; wrap_comments= C.default Formatting.wrap_comments
  ; wrap_fun_args= C.default Formatting.wrap_fun_args }

let default_profile = conventional_profile

let janestreet_profile =
  { align_pattern_matching_bar= `Keyword
  ; assignment_operator= `Begin_line
  ; break_before_in= `Fit_or_vertical
  ; break_cases= `Fit_or_vertical
  ; break_collection_expressions=
      ocamlformat_profile.break_collection_expressions
  ; break_infix= `Fit_or_vertical
  ; break_infix_before_func= true
  ; break_fun_decl= `Fit_or_vertical
  ; break_fun_sig= `Fit_or_vertical
  ; break_separators= `Before
  ; break_sequences= true
  ; break_string_literals= `Auto
  ; break_struct= ocamlformat_profile.break_struct
  ; cases_exp_indent= 2
  ; cases_matching_exp_indent= `Normal
  ; disambiguate_non_breaking_match= false
  ; doc_comments= `Before
  ; doc_comments_padding= 1
  ; doc_comments_tag_only= `Fit
  ; dock_collection_brackets= false
  ; exp_grouping= `Parens
  ; extension_indent= 2
  ; field_space= `Loose
  ; function_indent= 2
  ; function_indent_nested= `Never
  ; if_then_else= `Keyword_first
  ; indent_after_in= 0
  ; indicate_multiline_delimiters= `No
  ; indicate_nested_or_patterns= `Unsafe_no
  ; infix_precedence= `Parens
  ; leading_nested_match_parens= true
  ; let_and= `Sparse
  ; let_binding_indent= 2
  ; let_binding_spacing= `Double_semicolon
  ; let_module= `Sparse
  ; line_endings= `Lf
  ; margin= 90
  ; match_indent= 0
  ; match_indent_nested= `Never
  ; max_indent= Some 2
  ; module_item_spacing= `Compact
  ; nested_match= `Wrap
  ; ocp_indent_compat= true
  ; parens_ite= true
  ; parens_tuple= `Multi_line_only
  ; parens_tuple_patterns= `Multi_line_only
  ; parse_docstrings= false
  ; parse_toplevel_phrases= false
  ; sequence_blank_line= `Compact
  ; sequence_style= `Terminator
  ; single_case= `Sparse
  ; space_around_arrays= true
  ; space_around_lists= true
  ; space_around_records= true
  ; space_around_variants= true
  ; stritem_extension_indent= 0
  ; type_decl= `Sparse
  ; type_decl_indent= 2
  ; wrap_comments= false
  ; wrap_fun_args= false }

let selected_profile_ref = ref (Some default_profile)

let (_profile : fmt_opts option C.t) =
  let doc =
    "Select a preset profile which sets $(i,all) options, overriding lower \
     priority configuration."
  in
  let names = profile_option_names in
  let all =
    [ C.Value.make ~name:"conventional" (Some conventional_profile)
        "The $(b,conventional) profile aims to be as familiar and \
         \"conventional\" appearing as the available options allow."
    ; C.Value.make ~name:"default" (Some default_profile)
        "$(b,default) is an alias for the $(b,conventional) profile."
    ; C.Value.make ~name:"ocamlformat" (Some ocamlformat_profile)
        "The $(b,ocamlformat) profile aims to take advantage of the \
         strengths of a parsetree-based auto-formatter, and to limit the \
         consequences of the weaknesses imposed by the current \
         implementation. This is a style which optimizes for what the \
         formatter can do best, rather than to match the style of any \
         existing code. General guidelines that have directed the design \
         include: Legibility, in the sense of making it as hard as possible \
         for quick visual parsing to give the wrong interpretation, is of \
         highest priority; Whenever possible the high-level structure of \
         the code should be obvious by looking only at the left margin, in \
         particular, it should not be necessary to visually jump from left \
         to right hunting for critical keywords, tokens, etc; All else \
         equal compact code is preferred as reading without scrolling is \
         easier, so indentation or white space is avoided unless it helps \
         legibility; Attention has been given to making some syntactic \
         gotchas visually obvious."
    ; C.Value.make ~name:"janestreet" (Some janestreet_profile)
        "The $(b,janestreet) profile is used at Jane Street." ]
  in
  C.choice ~names ~all ~doc ~kind:C.Formatting
    ~removed_values:
      [ C.Value_removed.make ~name:"compact" ~since:V.v0_22 ~msg:""
      ; C.Value_removed.make ~name:"sparse" ~since:V.v0_22 ~msg:"" ]
    (fun conf p ->
      selected_profile_ref := p ;
      let new_fmt_opts = Option.value p ~default:conf.fmt_opts in
      {conf with fmt_opts= new_fmt_opts} )
    (fun _ -> !selected_profile_ref)

let parse_line config ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.current value || !no_version_check then
          Ok config
        else
          Error
            (Config_option.Error.Version_mismatch
               {read= value; installed= Version.current} )
    | name, `File x ->
        C.update ~config ~from:(`Parsed (`File x)) ~name ~value ~inline:false
    | name, `Attribute ->
        if !disable_conf_attrs then (
          warn "Configuration in attribute %S ignored." s ;
          Ok config )
        else
          C.update ~config ~from:(`Parsed `Attribute) ~name ~value
            ~inline:true
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
      update ~config ~from ~name ~value
  | [s] -> (
    match String.strip s with
    | "" -> impossible "previous match"
    (* special case for disable/enable *)
    | "enable" -> update ~config ~from ~name:"disable" ~value:"false"
    | name -> update ~config ~from ~name ~value:"true" )
  | _ -> Error (Config_option.Error.Malformed s)

(** Do not escape from [build_config] *)
exception Conf_error of string

let failwith_user_errors ~from errors =
  let open Format in
  let pp_error pp e = pp_print_string pp (Config_option.Error.to_string e) in
  let pp_errors = pp_print_list ~pp_sep:pp_print_newline pp_error in
  let msg = asprintf "Error while parsing %s:@ %a" from pp_errors errors in
  raise (Conf_error msg)

let update_from_ocp_indent c (oic : IndentConfig.t) =
  let convert_threechoices = function
    | IndentConfig.Always -> `Always
    | Never -> `Never
    | Auto -> `Auto
  in
  { c with
    fmt_opts=
      { c.fmt_opts with
        let_binding_indent= oic.i_base
      ; type_decl_indent= oic.i_type
      ; indent_after_in= oic.i_in
      ; function_indent= oic.i_with
      ; match_indent= oic.i_with
      ; cases_exp_indent= oic.i_match_clause
      ; stritem_extension_indent= oic.i_ppx_stritem_ext
      ; max_indent= oic.i_max_indent
      ; function_indent_nested= convert_threechoices oic.i_strict_with
      ; match_indent_nested= convert_threechoices oic.i_strict_with } }

let read_config_file conf = function
  | File_system.Ocp_indent file -> (
      let filename = Fpath.to_string file in
      try
        let ocp_indent_conf = IndentConfig.default in
        In_channel.with_file filename ~f:(fun ic ->
            let ocp_indent_conf, errors, _ =
              In_channel.fold_lines ic ~init:(ocp_indent_conf, [], 1)
                ~f:(fun (conf, errors, lnum) line ->
                  try
                    ( IndentConfig.update_from_string ocp_indent_conf line
                    , errors
                    , lnum + 1 )
                  with
                  | Invalid_argument e when !ignore_invalid_options ->
                      warn ~filename:file ~lnum "%s" e ;
                      (conf, errors, lnum + 1)
                  | Invalid_argument e ->
                      ( conf
                      , Config_option.Error.Unknown (e, None) :: errors
                      , lnum + 1 ) )
            in
            match List.rev errors with
            | [] -> update_from_ocp_indent conf ocp_indent_conf
            | l -> failwith_user_errors ~from:filename l )
      with Sys_error _ -> conf )
  | File_system.Ocamlformat file -> (
      let filename = Fpath.to_string file in
      try
        In_channel.with_file filename ~f:(fun ic ->
            let c, errors, _ =
              In_channel.fold_lines ic ~init:(conf, [], 1)
                ~f:(fun (conf, errors, num) line ->
                  let from = `File (file, num) in
                  match parse_line conf ~from line with
                  | Ok conf -> (conf, errors, Int.succ num)
                  | Error _ when !ignore_invalid_options ->
                      warn ~filename:file ~lnum:num
                        "ignoring invalid options %S" line ;
                      (conf, errors, Int.succ num)
                  | Error e -> (conf, e :: errors, Int.succ num) )
            in
            match List.rev errors with
            | [] -> c
            | l -> failwith_user_errors ~from:filename l )
      with Sys_error _ -> conf )

let update_using_env conf =
  let f (config, errors) (name, value) =
    match C.update ~config ~from:`Env ~name ~value ~inline:false with
    | Ok c -> (c, errors)
    | Error e -> (config, e :: errors)
  in
  let conf, errors = List.fold_left !config ~init:(conf, []) ~f in
  match List.rev errors with
  | [] -> conf
  | l -> failwith_user_errors ~from:"OCAMLFORMAT environment variable" l

let is_in_listing_file ~listings ~filename =
  let drop_line l = String.is_empty l || String.is_prefix l ~prefix:"#" in
  (* process deeper files first *)
  let listings = List.rev listings in
  List.find_map listings ~f:(fun listing_file ->
      let dir, _ = Fpath.split_base listing_file in
      try
        In_channel.with_file (Fpath.to_string listing_file) ~f:(fun ch ->
            let lines =
              In_channel.input_lines ch
              |> List.mapi ~f:(fun i s -> (i + 1, String.strip s))
              |> List.filter ~f:(fun (_, l) -> not (drop_line l))
            in
            List.find_map lines ~f:(fun (lno, line) ->
                match Fpath.of_string line with
                | Ok file_on_current_line -> (
                    let f = Fpath.(dir // file_on_current_line) in
                    if Fpath.equal filename f then Some (listing_file, lno)
                    else
                      try
                        let filename = Fpath.to_string filename in
                        let re =
                          let pathname = true and anchored = true in
                          let f = Fpath.to_string f in
                          Re.(Glob.glob ~pathname ~anchored f |> compile)
                        in
                        Option.some_if (Re.execp re filename)
                          (listing_file, lno)
                      with Re.Glob.Parse_error ->
                        warn ~filename:listing_file ~lnum:lno
                          "pattern %s cannot be parsed" line ;
                        None )
                | Error (`Msg msg) ->
                    warn ~filename:listing_file ~lnum:lno "%s" msg ;
                    None ) )
      with Sys_error err ->
        warn "ignoring %a, %s" Fpath.pp listing_file err ;
        None )

let default =
  { fmt_opts= default_profile
  ; opr_opts=
      { comment_check= C.default Operational.comment_check
      ; debug= C.default Operational.debug
      ; disable= C.default Operational.disable
      ; margin_check= C.default Operational.margin_check
      ; max_iters= C.default Operational.max_iters
      ; ocaml_version= C.default Operational.ocaml_version
      ; quiet= C.default Operational.quiet
      ; range= C.default Operational.range } }

let build_config ~enable_outside_detected_project ~root ~file ~is_stdin =
  let vfile = Fpath.v file in
  let file_abs = Fpath.(vfile |> to_absolute |> normalize) in
  let fs =
    File_system.make ~enable_outside_detected_project
      ~disable_conf_files:!disable_conf_files
      ~ocp_indent_config:!ocp_indent_config ~root ~file:file_abs
  in
  let conf =
    List.fold fs.configuration_files ~init:default ~f:read_config_file
    |> update_using_env |> C.update_using_cmdline
  in
  if
    (not is_stdin)
    && (not (File_system.has_ocamlformat_file fs))
    && not enable_outside_detected_project
  then (
    (let why =
       match fs.project_root with
       | Some root ->
           Format.sprintf
             "no [.ocamlformat] was found within the project (root: %s)"
             (Fpath.to_string ~relativize:true root)
       | None -> "no project root was found"
     in
     warn ~filename:vfile
       "Ocamlformat disabled because [--enable-outside-detected-project] is \
        not set and %s"
       why ) ;
    Operational.update conf ~f:(fun f -> {f with disable= true}) )
  else
    let listings =
      if conf.opr_opts.disable then fs.enable_files else fs.ignore_files
    in
    match is_in_listing_file ~listings ~filename:file_abs with
    | Some (file, lno) ->
        let status =
          if conf.opr_opts.disable then "enabled" else "ignored"
        in
        if conf.opr_opts.debug then
          Format.eprintf "File %a: %s in %a:%d@\n" Fpath.pp file_abs status
            Fpath.pp file lno ;
        Operational.update conf ~f:(fun f -> {f with disable= not f.disable})
    | None -> conf

let build_config ~enable_outside_detected_project ~root ~file ~is_stdin =
  try
    let conf, warn_now =
      collect_warnings (fun () ->
          build_config ~enable_outside_detected_project ~root ~file ~is_stdin )
    in
    if not conf.opr_opts.quiet then warn_now () ;
    Ok conf
  with Conf_error msg -> Error msg

let kind_of_ext fname =
  match Filename.extension fname with
  | ".ml" | ".mlt" | ".eliom" -> Some Syntax.Use_file
  | ".mli" | ".eliomi" -> Some Syntax.Signature
  | _ -> None

let validate_inputs () =
  match (!inputs, !kind, !name) with
  | [], _, _ -> Ok `No_input
  | [Stdin], None, None ->
      Error
        "Must specify at least one of --name, --impl or --intf when reading \
         from stdin"
  | [Stdin], Some kind, name -> Ok (`Stdin (name, kind))
  | [Stdin], None, Some name -> (
    match kind_of_ext name with
    | Some kind -> Ok (`Stdin (Some name, kind))
    | None ->
        Error
          "Cannot deduce file kind from passed --name. Please specify \
           --impl or --intf" )
  | [File f], Some kind, name -> Ok (`Single_file (kind, name, f))
  | [File f], None, name ->
      let kind =
        Option.value ~default:f name
        |> kind_of_ext
        |> Option.value ~default:Syntax.Use_file
      in
      Ok (`Single_file (kind, name, f))
  | _ :: _ :: _, Some _, _ ->
      Error "Cannot specify --impl or --intf with multiple inputs"
  | _ :: _ :: _, _, Some _ ->
      Error "Cannot specify --name with multiple inputs"
  | (_ :: _ :: _ as inputs), None, None ->
      List.map inputs ~f:(function
        | Stdin -> Error "Cannot specify stdin together with other inputs"
        | File f ->
            let kind = Option.value ~default:Use_file (kind_of_ext f) in
            Ok (kind, f) )
      |> Result.all
      |> Result.map ~f:(fun files -> `Several_files files)

let validate_action () =
  match
    List.filter_map
      ~f:(fun s -> s)
      [ Option.map ~f:(fun o -> (`Output o, "--output")) !output
      ; Option.some_if !inplace (`Inplace, "--inplace")
      ; Option.some_if !check (`Check, "--check")
      ; Option.some_if !print_config (`Print_config, "--print-config")
      ; Option.some_if !numeric (`Numeric, "--numeric") ]
  with
  | [] -> Ok `No_action
  | [(action, _)] -> Ok action
  | (_, a1) :: (_, a2) :: _ ->
      Error (Printf.sprintf "Cannot specify %s with %s" a1 a2)

type input = {kind: Syntax.t; name: string; file: file; conf: t}

type action =
  | In_out of input * string option
  | Inplace of input list
  | Check of input list
  | Print_config of t
  | Numeric of input

let make_action ~enable_outside_detected_project ~root action inputs =
  let make_file ?name kind file =
    let name = Option.value ~default:file name in
    let+ conf =
      build_config ~enable_outside_detected_project ~root ~file:name
        ~is_stdin:false
    in
    Ok {kind; name; file= File file; conf}
  in
  let make_stdin ?(name = "<standard input>") kind =
    let+ conf =
      build_config ~enable_outside_detected_project ~root ~file:name
        ~is_stdin:false
    in
    Ok {kind; name; file= Stdin; conf}
  in
  let make_input = function
    | `Single_file (kind, name, f) -> make_file ?name kind f
    | `Stdin (name, kind) -> make_stdin ?name kind
  in
  let make_inputs = function
    | (`Single_file _ | `Stdin _) as inp ->
        let+ inp = make_input inp in
        Ok [inp]
    | `Several_files files ->
        let+ inputs =
          List.fold_left files ~init:(Ok []) ~f:(fun acc (kind, file) ->
              let+ acc = acc in
              let+ file = make_file kind file in
              Ok (file :: acc) )
        in
        Ok (List.rev inputs)
  in
  match (action, inputs) with
  | `Print_config, inputs ->
      let file, is_stdin =
        match inputs with
        | `Stdin _ -> ("-", true)
        | `Single_file (_, _, f) -> (f, false)
        | `Several_files ((_, f) :: _) -> (f, false)
        | `Several_files [] | `No_input ->
            (File_system.root_ocamlformat_file ~root |> Fpath.to_string, true)
      in
      let+ conf =
        build_config ~enable_outside_detected_project ~root ~file ~is_stdin
      in
      Ok (Print_config conf)
  | (`No_action | `Output _ | `Inplace | `Check | `Numeric), `No_input ->
      Error "Must specify at least one input file, or `-` for stdin"
  | (`No_action | `Output _ | `Numeric), `Several_files _ ->
      Error
        "Must specify exactly one input file without --inplace or --check"
  | `Inplace, `Stdin _ ->
      Error "Cannot specify stdin together with --inplace"
  | `No_action, ((`Single_file _ | `Stdin _) as inp) ->
      let+ inp = make_input inp in
      Ok (In_out (inp, None))
  | `Output output, ((`Single_file _ | `Stdin _) as inp) ->
      let+ inp = make_input inp in
      Ok (In_out (inp, Some output))
  | `Inplace, ((`Single_file _ | `Several_files _) as inputs) ->
      let+ inputs = make_inputs inputs in
      Ok (Inplace inputs)
  | `Check, ((`Single_file _ | `Several_files _ | `Stdin _) as inputs) ->
      let+ inputs = make_inputs inputs in
      Ok (Check inputs)
  | `Numeric, ((`Stdin _ | `Single_file _) as inp) ->
      let+ inp = make_input inp in
      Ok (Numeric inp)

let validate () =
  let root =
    Option.map !root ~f:Fpath.(fun x -> v x |> to_absolute |> normalize)
  in
  let enable_outside_detected_project =
    !enable_outside_detected_project && Option.is_none root
  in
  match
    let+ action = validate_action () in
    let+ inputs = validate_inputs () in
    make_action ~enable_outside_detected_project ~root action inputs
  with
  | Error e -> `Error (false, e)
  | Ok action -> `Ok action

let action () = parse info validate

open Parsetree

let update ?(quiet = false) c {attr_name= {txt; loc}; attr_payload; _} =
  let result =
    match txt with
    | "ocamlformat" -> (
      match attr_payload with
      | PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc=
                        Pexp_constant
                          {pconst_desc= Pconst_string (str, _, None); _}
                    ; pexp_attributes= []
                    ; _ }
                  , [] )
            ; _ } ] ->
          parse_line ~from:`Attribute c str
          |> Result.map_error ~f:Config_option.Error.to_string
      | _ -> Error "Invalid format: String expected" )
    | _ when String.is_prefix ~prefix:"ocamlformat." txt ->
        Error
          (Format.sprintf "Invalid format: Unknown suffix %S"
             (String.chop_prefix_exn ~prefix:"ocamlformat." txt) )
    | _ -> Ok c
  in
  match result with
  | Ok conf -> conf
  | Error error ->
      let w = Warnings.Attribute_payload (txt, error) in
      if (not c.opr_opts.quiet) && not quiet then Warning.print_warning loc w ;
      c

let update_value config ~name ~value =
  C.update ~config ~from:`Commandline ~name ~value ~inline:false

let print_config = C.print_config
