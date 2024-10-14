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

module Location = Migrate_ast.Location
include Conf_t

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

let warn ~loc fmt =
  Format.kasprintf
    (fun s ->
      warn_raw
        (Format.asprintf "%!@{<loc>%a@}:@,@{<warning>Warning@}: %s\n%!"
           Location.print_loc loc s ) )
    fmt

module Decl = Conf_decl
module Store = Decl.Store

let conventional_profile from =
  let elt content = Elt.make content from in
  { align_symbol_open_paren= elt true
  ; assignment_operator= elt `End_line
  ; break_around_multiline_strings= elt false
  ; break_before_in= elt `Fit_or_vertical
  ; break_cases= elt `Fit
  ; break_collection_expressions= elt `Fit_or_vertical
  ; break_colon= elt `After
  ; break_infix= elt `Wrap
  ; break_infix_before_func= elt false
  ; break_fun_decl= elt `Wrap
  ; break_fun_sig= elt `Wrap
  ; break_separators= elt `After
  ; break_sequences= elt true
  ; break_string_literals= elt `Auto
  ; break_struct= elt true
  ; cases_exp_indent= elt 4
  ; cases_matching_exp_indent= elt `Normal
  ; disambiguate_non_breaking_match= elt false
  ; doc_comments= elt `After_when_possible
  ; doc_comments_padding= elt 2
  ; doc_comments_tag_only= elt `Default
  ; dock_collection_brackets= elt true
  ; exp_grouping= elt `Parens
  ; extension_indent= elt 2
  ; field_space= elt `Loose
  ; function_indent= elt 2
  ; function_indent_nested= elt `Never
  ; if_then_else= elt `Compact
  ; indent_after_in= elt 0
  ; indicate_multiline_delimiters= elt `No
  ; indicate_nested_or_patterns= elt `Unsafe_no
  ; infix_precedence= elt `Indent
  ; leading_nested_match_parens= elt false
  ; let_and= elt `Compact
  ; let_binding_indent= elt 2
  ; let_binding_deindent_fun= elt true
  ; let_binding_spacing= elt `Compact
  ; let_module= elt `Compact
  ; line_endings= elt `Lf
  ; margin= elt 80
  ; match_indent= elt 0
  ; match_indent_nested= elt `Never
  ; max_indent= elt None
  ; module_item_spacing= elt `Compact
  ; nested_match= elt `Wrap
  ; ocp_indent_compat= elt false
  ; parens_ite= elt false
  ; parens_tuple= elt `Always
  ; parens_tuple_patterns= elt `Multi_line_only
  ; parse_docstrings= elt true
  ; parse_toplevel_phrases= elt false
  ; sequence_blank_line= elt `Preserve_one
  ; sequence_style= elt `Terminator
  ; single_case= elt `Compact
  ; space_around_arrays= elt true
  ; space_around_lists= elt true
  ; space_around_records= elt true
  ; space_around_variants= elt true
  ; stritem_extension_indent= elt 0
  ; type_decl= elt `Compact
  ; type_decl_indent= elt 2
  ; wrap_comments= elt false
  ; wrap_docstrings= elt true
  ; wrap_fun_args= elt true }

let default_profile = conventional_profile

let ocamlformat_profile from =
  let elt content = Elt.make content from in
  { align_symbol_open_paren= elt true
  ; assignment_operator= elt `End_line
  ; break_around_multiline_strings= elt false
  ; break_before_in= elt `Fit_or_vertical
  ; break_cases= elt `Nested
  ; break_collection_expressions= elt `Fit_or_vertical
  ; break_colon= elt `After
  ; break_infix= elt `Wrap
  ; break_infix_before_func= elt true
  ; break_fun_decl= elt `Wrap
  ; break_fun_sig= elt `Wrap
  ; break_separators= elt `Before
  ; break_sequences= elt false
  ; break_string_literals= elt `Auto
  ; break_struct= elt true
  ; cases_exp_indent= elt 4
  ; cases_matching_exp_indent= elt `Compact
  ; disambiguate_non_breaking_match= elt false
  ; doc_comments= elt `Before_except_val
  ; doc_comments_padding= elt 2
  ; doc_comments_tag_only= elt `Default
  ; dock_collection_brackets= elt false
  ; exp_grouping= elt `Parens
  ; extension_indent= elt 2
  ; field_space= elt `Tight
  ; function_indent= elt 2
  ; function_indent_nested= elt `Never
  ; if_then_else= elt `Compact
  ; indent_after_in= elt 0
  ; indicate_multiline_delimiters= elt `Space
  ; indicate_nested_or_patterns= elt `Space
  ; infix_precedence= elt `Indent
  ; leading_nested_match_parens= elt false
  ; let_and= elt `Compact
  ; let_binding_indent= elt 2
  ; let_binding_deindent_fun= elt true
  ; let_binding_spacing= elt `Compact
  ; let_module= elt `Compact
  ; line_endings= elt `Lf
  ; margin= elt 80
  ; match_indent= elt 0
  ; match_indent_nested= elt `Never
  ; max_indent= elt None
  ; module_item_spacing= elt `Sparse
  ; nested_match= elt `Wrap
  ; ocp_indent_compat= elt false
  ; parens_ite= elt false
  ; parens_tuple= elt `Always
  ; parens_tuple_patterns= elt `Multi_line_only
  ; parse_docstrings= elt false
  ; parse_toplevel_phrases= elt false
  ; sequence_blank_line= elt `Compact
  ; sequence_style= elt `Separator
  ; single_case= elt `Compact
  ; space_around_arrays= elt false
  ; space_around_lists= elt false
  ; space_around_records= elt false
  ; space_around_variants= elt false
  ; stritem_extension_indent= elt 0
  ; type_decl= elt `Compact
  ; type_decl_indent= elt 2
  ; wrap_comments= elt false
  ; wrap_docstrings= elt true
  ; wrap_fun_args= elt true }

let janestreet_profile from =
  let elt content = Elt.make content from in
  { align_symbol_open_paren= elt false
  ; assignment_operator= elt `Begin_line
  ; break_around_multiline_strings= elt true
  ; break_before_in= elt `Fit_or_vertical
  ; break_cases= elt `Fit_or_vertical
  ; break_collection_expressions=
      elt (ocamlformat_profile from).break_collection_expressions.v
  ; break_colon= elt `Before
  ; break_infix= elt `Fit_or_vertical
  ; break_infix_before_func= elt true
  ; break_fun_decl= elt `Fit_or_vertical
  ; break_fun_sig= elt `Fit_or_vertical
  ; break_separators= elt `Before
  ; break_sequences= elt true
  ; break_string_literals= elt `Auto
  ; break_struct= elt (ocamlformat_profile from).break_struct.v
  ; cases_exp_indent= elt 2
  ; cases_matching_exp_indent= elt `Normal
  ; disambiguate_non_breaking_match= elt false
  ; doc_comments= elt `Before
  ; doc_comments_padding= elt 1
  ; doc_comments_tag_only= elt `Fit
  ; dock_collection_brackets= elt false
  ; exp_grouping= elt `Parens
  ; extension_indent= elt 2
  ; field_space= elt `Loose
  ; function_indent= elt 2
  ; function_indent_nested= elt `Never
  ; if_then_else= elt `Keyword_first
  ; indent_after_in= elt 0
  ; indicate_multiline_delimiters= elt `No
  ; indicate_nested_or_patterns= elt `Unsafe_no
  ; infix_precedence= elt `Parens
  ; leading_nested_match_parens= elt true
  ; let_and= elt `Sparse
  ; let_binding_indent= elt 2
  ; let_binding_deindent_fun= elt false
  ; let_binding_spacing= elt `Double_semicolon
  ; let_module= elt `Sparse
  ; line_endings= elt `Lf
  ; margin= elt 90
  ; match_indent= elt 0
  ; match_indent_nested= elt `Never
  ; max_indent= elt None
  ; module_item_spacing= elt `Compact
  ; nested_match= elt `Wrap
  ; ocp_indent_compat= elt true
  ; parens_ite= elt true
  ; parens_tuple= elt `Multi_line_only
  ; parens_tuple_patterns= elt `Multi_line_only
  ; parse_docstrings= elt false
  ; parse_toplevel_phrases= elt false
  ; sequence_blank_line= elt `Compact
  ; sequence_style= elt `Terminator
  ; single_case= elt `Sparse
  ; space_around_arrays= elt true
  ; space_around_lists= elt true
  ; space_around_records= elt true
  ; space_around_variants= elt true
  ; stritem_extension_indent= elt 2
  ; type_decl= elt `Sparse
  ; type_decl_indent= elt 2
  ; wrap_comments= elt false
  ; wrap_docstrings= elt false
  ; wrap_fun_args= elt false }

let default =
  let elt content = Elt.make content `Default in
  { fmt_opts= default_profile `Default
  ; profile= elt `default
  ; opr_opts=
      { comment_check= elt true
      ; debug= elt false
      ; disable= elt false
      ; margin_check= elt false
      ; max_iters= elt 10
      ; ocaml_version= elt Ocaml_version.Releases.v4_04_0
      ; quiet= elt false
      ; disable_conf_attrs= elt false
      ; version_check= elt true } }

module V = struct
  let v0_12 = Version.make ~major:0 ~minor:12 ~patch:None

  let v0_14 = Version.make ~major:0 ~minor:14 ~patch:None

  let v0_16 = Version.make ~major:0 ~minor:16 ~patch:None

  let v0_17 = Version.make ~major:0 ~minor:17 ~patch:None

  let v0_22 = Version.make ~major:0 ~minor:22 ~patch:None
end

let profile =
  let doc =
    "Select a preset profile which sets $(i,all) options, overriding lower \
     priority configuration."
  in
  let names = profile_option_names in
  let all =
    [ Decl.Value.make ~name:"default" `default
        "$(b,default) is an alias for the $(b,conventional) profile."
    ; Decl.Value.make ~name:"conventional" `conventional
        "The $(b,conventional) profile aims to be as familiar and \
         \"conventional\" appearing as the available options allow."
    ; Decl.Value.make ~name:"ocamlformat" `ocamlformat
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
    ; Decl.Value.make ~name:"janestreet" `janestreet
        "The $(b,janestreet) profile is used at Jane Street." ]
  in
  Decl.choice ~names ~all ~default ~doc ~kind:Decl.Formatting
    ~removed_values:
      [ Decl.Value_removed.make ~name:"compact" ~since:V.v0_22 ~msg:""
      ; Decl.Value_removed.make ~name:"sparse" ~since:V.v0_22 ~msg:"" ]
    (fun conf elt ->
      let p = elt.v and from = elt.from in
      let name =
        match p with
        | `default -> "default"
        | `conventional -> "conventional"
        | `ocamlformat -> "ocamlformat"
        | `janestreet -> "janestreet"
      in
      let from_p =
        let ufrom =
          match from with
          | `Default | `Updated (_, Some _) -> assert false
          | `Profile (_, uf) | `Updated (uf, _) -> uf
        in
        `Profile (name, ufrom)
      in
      let p =
        ( match p with
        | `default -> default_profile
        | `conventional -> conventional_profile
        | `ocamlformat -> ocamlformat_profile
        | `janestreet -> janestreet_profile )
          from_p
      in
      {conf with profile= elt; fmt_opts= p} )
    (fun conf -> conf.profile)

let options = Store.[elt profile]

(** Options affecting formatting *)
module Formatting = struct
  let kind = Decl.Formatting

  let update ~f c = {c with fmt_opts= f c.fmt_opts}

  let align_cases =
    let names = ["align-cases"] in
    Decl.removed_option ~names ~since:V.v0_22 ~msg:""

  let align_constructors_decl =
    let names = ["align-constructors-decl"] in
    Decl.removed_option ~names ~since:V.v0_22 ~msg:""

  let align_variants_decl =
    let names = ["align-variants-decl"] in
    Decl.removed_option ~names ~since:V.v0_22 ~msg:""

  let assignment_operator =
    let doc = "Position of the assignment operator." in
    let names = ["assignment-operator"] in
    let all =
      [ Decl.Value.make ~name:"end-line" `End_line
          "$(b,end-line) positions assignment operators (`:=` and `<-`) at \
           the end of the line and breaks after it if the whole assignment \
           expression does not fit on a single line."
      ; Decl.Value.make ~name:"begin-line" `Begin_line
          "$(b,begin-line) positions assignment operators (`:=` and `<-`) \
           at the beginning of the line and breaks before it if the whole \
           assignment expression does not fit on a single line." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with assignment_operator= elt}) )
      (fun conf -> conf.fmt_opts.assignment_operator)

  let break_before_in =
    let doc =
      "Whether the line should break before the $(i,in) keyword of a \
       $(i,let) binding."
    in
    let names = ["break-before-in"] in
    let all =
      [ Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) will always break the line before the \
           $(i,in) keyword if the whole $(i,let) binding does not fit on a \
           single line."
      ; Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) will only break the line if the $(i,in) keyword does \
           not fit on the previous line." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_before_in= elt}) )
      (fun conf -> conf.fmt_opts.break_before_in)

  let break_cases =
    let doc = "Break pattern match cases." in
    let names = ["break-cases"] in
    let all =
      [ Decl.Value.make ~name:"fit" `Fit
          "Specifying $(b,fit) lets pattern matches break at the margin \
           naturally."
      ; Decl.Value.make ~name:"nested" `Nested
          "$(b,nested) forces a break after nested or-patterns to highlight \
           the case body. Note that with $(b,nested), the \
           $(b,indicate-nested-or-patterns) option is not needed, and so \
           ignored."
      ; Decl.Value.make ~name:"toplevel" `Toplevel
          "$(b,toplevel) forces top-level cases (i.e. not nested \
           or-patterns) to break across lines, otherwise break naturally at \
           the margin."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) tries to fit all or-patterns on the same \
           line, otherwise breaks."
      ; Decl.Value.make ~name:"vertical" `Vertical
          "$(b,vertical) vertically breaks branches."
      ; Decl.Value.make ~name:"all" `All
          "$(b,all) forces all pattern matches to break across lines." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_cases= elt}))
      (fun conf -> conf.fmt_opts.break_cases)

  let break_collection_expressions =
    let doc =
      "Break collection expressions (lists and arrays) elements by elements."
    in
    let names = ["break-collection-expressions"] in
    let all =
      [ Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line."
      ; Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) will group simple expressions and try to format them \
           in a single line." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_collection_expressions= elt}) )
      (fun conf -> conf.fmt_opts.break_collection_expressions)

  let break_colon =
    let doc =
      "Break before or after colon in value binding declarations and type \
       constraints."
    in
    let names = ["break-colon"] in
    let all =
      [ Decl.Value.make ~name:"after" `After
          "$(b,after) breaks after the colon."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) breaks before the colon." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_colon= elt}))
      (fun conf -> conf.fmt_opts.break_colon)

  let break_fun_decl =
    let doc = "Style for function declarations and types." in
    let names = ["break-fun-decl"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) breaks only if necessary."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line."
      ; Decl.Value.make ~name:"smart" `Smart
          "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_fun_decl= elt}))
      (fun conf -> conf.fmt_opts.break_fun_decl)

  let break_fun_sig =
    let doc = "Style for function signatures." in
    let names = ["break-fun-sig"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) breaks only if necessary."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line."
      ; Decl.Value.make ~name:"smart" `Smart
          "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_fun_sig= elt}))
      (fun conf -> conf.fmt_opts.break_fun_sig)

  let break_infix =
    let doc = "Break sequence of infix operators." in
    let names = ["break-infix"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) will group simple expressions and try to format them \
           in a single line."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line."
      ; Decl.Value.make ~name:"wrap-or-vertical" `Wrap_or_vertical
          "$(b,wrap-or-vertical) behaves like $(b,wrap) for high precedence \
           operators and behaves like $(b,fit-or-vertical) for low \
           precedence operators." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_infix= elt}))
      (fun conf -> conf.fmt_opts.break_infix)

  let break_infix_before_func =
    let doc =
      "Break infix operators whose right arguments are anonymous functions \
       specially: do not break after the operator so that the first line of \
       the function appears docked at the end of line after the operator."
    in
    let names = ["break-infix-before-func"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_infix_before_func= elt}) )
      (fun conf -> conf.fmt_opts.break_infix_before_func)

  let break_separators =
    let doc =
      "Break before or after separators such as `;` in list or record \
       expressions."
    in
    let names = ["break-separators"] in
    let all =
      [ Decl.Value.make ~name:"after" `After
          "$(b,after) breaks the expressions after the separator."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) breaks the expressions before the separator." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      ~removed_values:
        [ Decl.Value_removed.make ~name:"after-and-docked" ~since:V.v0_12
            ~msg:
              "One can get a similar behaviour by setting \
               `break-separators=after`, `space-around-lists=false`, and \
               `dock-collection-brackets=false`." ]
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_separators= elt}) )
      (fun conf -> conf.fmt_opts.break_separators)

  let break_sequences =
    let doc =
      "Force sequence expressions to break irrespective of margin."
    in
    let names = ["break-sequences"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_sequences= elt}) )
      (fun conf -> conf.fmt_opts.break_sequences)

  let break_string_literals =
    let doc = "Break string literals." in
    let names = ["break-string-literals"] in
    let all =
      [ Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) mode breaks lines at newlines and wraps string \
           literals at the margin."
      ; Decl.Value.make ~name:"never" `Never
          "$(b,never) mode formats string literals as they are parsed, in \
           particular, with escape sequences expanded." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      ~removed_values:
        (Decl.Value_removed.make_list
           ~names:["newlines"; "newlines-and-wrap"; "wrap"]
           ~since:V.v0_12
           ~msg:
             "It has been replaced by the new default `auto` value, which \
              breaks lines at newlines and wraps string literals at the \
              margin." )
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_string_literals= elt}) )
      (fun conf -> conf.fmt_opts.break_string_literals)

  let break_struct =
    let doc = "Break struct-end module items." in
    let names = ["break-struct"] in
    let all =
      [ Decl.Value.make ~name:"force" `Force
          "$(b,force) will break struct-end phrases unconditionally."
      ; Decl.Value.make ~name:"natural" `Natural
          "$(b,natural) will break struct-end phrases naturally at the \
           margin." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f ->
            {f with break_struct= Elt.make Poly.(elt.v = `Force) elt.from} ) )
      (fun conf ->
        let elt = conf.fmt_opts.break_struct in
        if elt.v then Elt.make `Force elt.from
        else Elt.make `Natural elt.from )

  let cases_exp_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of cases expressions ($(docv) columns). See also the \
       $(b,cases-matching-exp-indent) and $(b,nested-match) options."
    in
    let names = ["cases-exp-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with cases_exp_indent= elt}) )
      (fun conf -> conf.fmt_opts.cases_exp_indent)

  let cases_matching_exp_indent =
    let doc =
      "Indentation of cases right-hand sides which are `match` or `try` \
       expressions."
    in
    let names = ["cases-matching-exp-indent"] in
    let all =
      [ Decl.Value.make ~name:"normal" `Normal
          "$(b,normal) indents as it would any other expression."
      ; Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) forces an indentation of 2, unless \
           $(b,nested-match) is set to $(b,align) and we're on the last \
           case." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with cases_matching_exp_indent= elt}) )
      (fun conf -> conf.fmt_opts.cases_matching_exp_indent)

  let disambiguate_non_breaking_match =
    let doc =
      "Add parentheses around matching constructs that fit on a single line."
    in
    Decl.flag
      ~names:["disambiguate-non-breaking-match"]
      ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f ->
            {f with disambiguate_non_breaking_match= elt} ) )
      (fun conf -> conf.fmt_opts.disambiguate_non_breaking_match)

  let doc_comments =
    let doc = "Doc comments position." in
    let names = ["doc-comments"] in
    let all =
      [ Decl.Value.make ~name:"after-when-possible" `After_when_possible
          "$(b,after-when-possible) puts doc comments after the \
           corresponding code. This option has no effect on variant \
           declarations because that would change their meaning and on \
           structures, signatures and objects for readability."
      ; Decl.Value.make ~name:"before-except-val" `Before_except_val
          "$(b,before-except-val) puts doc comments before the \
           corresponding code, but puts doc comments of $(b,val) and \
           $(b,external) declarations after the corresponding declarations."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) puts comments before the corresponding code." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      ~removed_values:
        [ Decl.Value_removed.make ~name:"after" ~since:V.v0_14
            ~msg:
              "This value has been renamed `after-when-possible` to take \
               into account the technical limitations of ocamlformat, the \
               behavior is unchanged." ]
      (fun conf elt -> update conf ~f:(fun f -> {f with doc_comments= elt}))
      (fun conf -> conf.fmt_opts.doc_comments)

  let doc_comments_padding =
    let docv = "PADDING" in
    let doc =
      "Add $(docv) spaces before doc comments in type declarations."
    in
    let names = ["doc-comments-padding"] in
    Decl.int ~names ~default ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with doc_comments_padding= elt}) )
      (fun conf -> conf.fmt_opts.doc_comments_padding)

  let doc_comments_tag_only =
    let doc = "Position of doc comments with only tags." in
    let names = ["doc-comments-tag-only"] in
    let all =
      [ Decl.Value.make ~name:"default" `Default
          "$(b,default) means no special treatment."
      ; Decl.Value.make ~name:"fit" `Fit
          "$(b,fit) puts doc comments on the same line." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with doc_comments_tag_only= elt}) )
      (fun conf -> conf.fmt_opts.doc_comments_tag_only)

  let doc_comments_val =
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
    Decl.removed_option ~names ~since:V.v0_16 ~msg

  let dock_collection_brackets =
    let doc =
      "Dock the brackets of lists, arrays and records, so that when the \
       collection does not fit on a single line the brackets are opened on \
       the preceding line and closed on the following line."
    in
    let names = ["dock-collection-brackets"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with dock_collection_brackets= elt}) )
      (fun conf -> conf.fmt_opts.dock_collection_brackets)

  let concrete_syntax_preserved_msg =
    "Concrete syntax will now always be preserved."

  let escape_chars =
    let names = ["escape-chars"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_16 ~msg

  let escape_strings =
    let names = ["escape-strings"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_16 ~msg

  let exp_grouping =
    let doc = "Style of expression grouping." in
    let names = ["exp-grouping"] in
    let all =
      [ Decl.Value.make ~name:"parens" `Parens
          "$(b,parens) groups expressions using parentheses."
      ; Decl.Value.make ~name:"preserve" `Preserve
          "$(b,preserve) preserves the original grouping syntax \
           (parentheses or $(i,begin)/$(i,end))." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind ~allow_inline:false
      (fun conf elt -> update conf ~f:(fun f -> {f with exp_grouping= elt}))
      (fun conf -> conf.fmt_opts.exp_grouping)

  let extension_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of items inside extension nodes ($(docv) columns)."
    in
    let names = ["extension-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with extension_indent= elt}) )
      (fun conf -> conf.fmt_opts.extension_indent)

  let extension_sugar =
    let names = ["extension-sugar"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_17 ~msg

  let field_space =
    let doc =
      "Whether or not to use a space between a field name and the \
       punctuation symbol (`:` or `=`) preceding the rhs. This option \
       affects records and objects."
    in
    let names = ["field-space"] in
    let all =
      [ Decl.Value.make ~name:"loose" `Loose "$(b,loose) uses a space."
      ; Decl.Value.make ~name:"tight" `Tight
          "$(b,tight) does not use a space."
      ; Decl.Value.make ~name:"tight-decl" `Tight_decl
          "$(b,tight-decl) is $(b,tight) for declarations and $(b,loose) \
           for instantiations." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with field_space= elt}))
      (fun conf -> conf.fmt_opts.field_space)

  let function_indent =
    let docv = "COLS" in
    let doc = "Indentation of function cases ($(docv) columns)." in
    let names = ["function-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with function_indent= elt}) )
      (fun conf -> conf.fmt_opts.function_indent)

  let function_indent_nested =
    let doc =
      "Whether the $(b,function-indent) parameter should be applied even \
       when in a sub-block."
    in
    let names = ["function-indent-nested"] in
    let all =
      [ Decl.Value.make ~name:"never" `Never
          "$(b,never) only applies $(b,function-indent) if the function \
           block starts a line."
      ; Decl.Value.make ~name:"always" `Always
          "$(b,always) always apply $(b,function-indent)."
      ; Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) applies $(b,function-indent) when seen fit." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with function_indent_nested= elt}) )
      (fun conf -> conf.fmt_opts.function_indent_nested)

  let if_then_else =
    let doc = "If-then-else formatting." in
    let names = ["if-then-else"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) tries to format an if-then-else expression on a \
           single line."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks branches if they do not \
           fit on a single line."
      ; Decl.Value.make ~name:"vertical" `Vertical
          "$(b,vertical) always vertically breaks branches."
      ; Decl.Value.make ~name:"keyword-first" `Keyword_first
          "$(b,keyword-first) formats if-then-else expressions such that \
           the if-then-else keywords are the first on the line."
      ; Decl.Value.make ~name:"k-r" `K_R
          "$(b,k-r) formats if-then-else expressions with parentheses that \
           match the K&R style." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with if_then_else= elt}))
      (fun conf -> conf.fmt_opts.if_then_else)

  let indent_after_in =
    let docv = "COLS" in
    let doc =
      "Indentation ($(docv) columns) after `let ... in`, unless followed by \
       another `let`."
    in
    let names = ["indent-after-in"] in
    Decl.int ~names ~default ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with indent_after_in= elt}) )
      (fun conf -> conf.fmt_opts.indent_after_in)

  let indicate_multiline_delimiters =
    let doc =
      "How to indicate that two matching delimiters live on different lines."
    in
    let names = ["indicate-multiline-delimiters"] in
    let all =
      [ Decl.Value.make ~name:"no" `No
          "$(b, no) doesn't do anything special to indicate the closing \
           delimiter."
      ; Decl.Value.make ~name:"space" `Space
          "$(b,space) prints a space inside the delimiter to indicate the \
           matching one is on a different line."
      ; Decl.Value.make ~name:"closing-on-separate-line"
          `Closing_on_separate_line
          "$(b, closing-on-separate-line) makes sure that the closing \
           delimiter is on its own line." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f ->
            {f with indicate_multiline_delimiters= elt} ) )
      (fun conf -> conf.fmt_opts.indicate_multiline_delimiters)

  let indicate_nested_or_patterns =
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let names = ["indicate-nested-or-patterns"] in
    let all =
      [ Decl.Value.make ~name:"unsafe-no" `Unsafe_no
          "$(b,unsafe-no) does not indicate nested or-patterns. Warning: \
           this can produce confusing code where a short body of a match \
           case is visually hidden by surrounding long patterns, leading to \
           misassociation between patterns and body expressions."
      ; Decl.Value.make ~name:"space" `Space
          "$(b,space) starts lines of nested or-patterns with \" |\" rather \
           than \"| \"." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with indicate_nested_or_patterns= elt}) )
      (fun conf -> conf.fmt_opts.indicate_nested_or_patterns)

  let infix_precedence =
    let doc =
      "Use indentation or also discretionary parentheses to explicitly \
       disambiguate precedences of infix operators."
    in
    let names = ["infix-precedence"] in
    let all =
      [ Decl.Value.make ~name:"indent" `Indent
          "$(b,indent) uses indentation to explicitly disambiguate \
           precedences of infix operators."
      ; Decl.Value.make ~name:"parens" `Parens
          "$(b,parens) uses parentheses to explicitly disambiguate \
           precedences of infix operators." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with infix_precedence= elt}) )
      (fun conf -> conf.fmt_opts.infix_precedence)

  let leading_nested_match_parens =
    let doc = "Nested match parens formatting." in
    let names = ["leading-nested-match-parens"] in
    Decl.flag ~names ~default ~doc ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with leading_nested_match_parens= elt}) )
      (fun conf -> conf.fmt_opts.leading_nested_match_parens)

  let let_and =
    let doc = "Style of let_and." in
    let names = ["let-and"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format `let p = e and p = e` in a \
           single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break between them." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with let_and= elt}))
      (fun conf -> conf.fmt_opts.let_and)

  let let_binding_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of let binding expressions ($(docv) columns) if they do \
       not fit on a single line."
    in
    let names = ["let-binding-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with let_binding_indent= elt}) )
      (fun conf -> conf.fmt_opts.let_binding_indent)

  let let_binding_deindent_fun =
    let doc = "Deindent a line beginning with `fun`." in
    let names = ["let-binding-deindent-fun"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with let_binding_deindent_fun= elt}) )
      (fun conf -> conf.fmt_opts.let_binding_deindent_fun)

  let let_binding_spacing =
    let doc = "Spacing between let binding." in
    let names = ["let-binding-spacing"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) spacing separates adjacent let bindings in a module \
           according to module-item-spacing."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) places two open lines between a multi-line \
           module-level let binding and the next."
      ; Decl.Value.make ~name:"double-semicolon" `Double_semicolon
          "$(b,double-semicolon) places double semicolons and an open line \
           between a multi-line module-level let binding and the next." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with let_binding_spacing= elt}) )
      (fun conf -> conf.fmt_opts.let_binding_spacing)

  let let_module =
    let doc = "Module binding formatting." in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) does not break a line after the $(i,let module ... \
           =) and before the $(i,in) if the module declaration does not fit \
           on a single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) breaks a line after $(i,let module ... =) and before \
           the $(i,in) if the module declaration does not fit on a single \
           line." ]
    in
    Decl.choice ~names:["let-module"] ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with let_module= elt}))
      (fun conf -> conf.fmt_opts.let_module)

  let let_open =
    let names = ["let-open"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_17 ~msg

  let line_endings =
    let doc = "Line endings used." in
    let all =
      [ Decl.Value.make ~name:"lf" `Lf "$(b,lf) uses Unix line endings."
      ; Decl.Value.make ~name:"crlf" `Crlf
          "$(b,crlf) uses Windows line endings." ]
    in
    Decl.choice ~names:["line-endings"] ~all ~default ~doc
      ~allow_inline:false ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with line_endings= elt}))
      (fun conf -> conf.fmt_opts.line_endings)

  let margin =
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    Decl.int ~names:["m"; "margin"] ~default ~doc ~docv ~kind
      ~allow_inline:false
      (fun conf elt -> update conf ~f:(fun f -> {f with margin= elt}))
      (fun conf -> conf.fmt_opts.margin)

  let match_indent =
    let docv = "COLS" in
    let doc = "Indentation of match/try cases ($(docv) columns)." in
    let names = ["match-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with match_indent= elt}))
      (fun conf -> conf.fmt_opts.match_indent)

  let match_indent_nested =
    let doc =
      "Whether the $(b,match-indent) parameter should be applied even when \
       in a sub-block."
    in
    let names = ["match-indent-nested"] in
    let all =
      [ Decl.Value.make ~name:"never" `Never
          "$(b,never) only applies $(b,match-indent) if the match block \
           starts a line."
      ; Decl.Value.make ~name:"always" `Always
          "$(b,always) always apply $(b,match-indent)."
      ; Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) applies $(b,match-indent) when seen fit." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with match_indent_nested= elt}) )
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
    Decl.any
      Arg.(some ~none:default_max_indent int)
      ~names:["max-indent"] ~doc ~docv ~kind ~default ~allow_inline:false
      ~values:Int
      (fun conf elt -> update conf ~f:(fun f -> {f with max_indent= elt}))
      (fun conf -> conf.fmt_opts.max_indent)

  let module_item_spacing =
    let doc = "Spacing between items of structures and signatures." in
    let names = ["module-item-spacing"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will not leave open lines between one-liners of \
           similar sorts."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break a line between two items."
      ; Decl.Value.make ~name:"preserve" `Preserve
          "$(b,preserve) will not leave open lines between one-liners of \
           similar sorts unless there is an open line in the input." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with module_item_spacing= elt}) )
      (fun conf -> conf.fmt_opts.module_item_spacing)

  let nested_match =
    let doc =
      "Style of a pattern-matching nested in the last case of another \
       pattern-matching."
    in
    let names = ["nested-match"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) wraps the nested pattern-matching with parentheses and \
           adds indentation."
      ; Decl.Value.make ~name:"align" `Align
          "$(b,align) vertically aligns the nested pattern-matching under \
           the encompassing pattern-matching." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with nested_match= elt}))
      (fun conf -> conf.fmt_opts.nested_match)

  let ocp_indent_compat =
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let names = ["ocp-indent-compat"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with ocp_indent_compat= elt}) )
      (fun conf -> conf.fmt_opts.ocp_indent_compat)

  let parens_ite =
    let doc =
      "Uses parentheses around if-then-else branches that spread across \
       multiple lines."
    in
    let names = ["parens-ite"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with parens_ite= elt}))
      (fun conf -> conf.fmt_opts.parens_ite)

  let parens_tuple =
    let doc = "Parens tuple expressions." in
    let names = ["parens-tuple"] in
    let all =
      [ Decl.Value.make ~name:"always" `Always
          "$(b,always) always uses parentheses around tuples."
      ; Decl.Value.make ~name:"multi-line-only" `Multi_line_only
          "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuples." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with parens_tuple= elt}))
      (fun conf -> conf.fmt_opts.parens_tuple)

  let parens_tuple_patterns =
    let doc = "Parens tuple patterns." in
    let names = ["parens-tuple-patterns"] in
    let all =
      [ Decl.Value.make ~name:"multi-line-only" `Multi_line_only
          "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuple patterns."
      ; Decl.Value.make ~name:"always" `Always
          "$(b,always) always uses parentheses around tuples patterns." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with parens_tuple_patterns= elt}) )
      (fun conf -> conf.fmt_opts.parens_tuple_patterns)

  let parse_docstrings =
    let doc = "Parse and format docstrings." in
    let names = ["parse-docstrings"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with parse_docstrings= elt}) )
      (fun conf -> conf.fmt_opts.parse_docstrings)

  let parse_toplevel_phrases =
    let doc = "Parse and format toplevel phrases and their output." in
    let names = ["parse-toplevel-phrases"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with parse_toplevel_phrases= elt}) )
      (fun conf -> conf.fmt_opts.parse_toplevel_phrases)

  let sequence_blank_line =
    let doc = "Blank line between expressions of a sequence." in
    let names = ["sequence-blank-line"] in
    let all =
      [ Decl.Value.make ~name:"preserve-one" `Preserve_one
          "$(b,preserve) will keep a blank line between two expressions of \
           a sequence if the input contains at least one."
      ; Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will not keep any blank line between expressions of \
           a sequence." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with sequence_blank_line= elt}) )
      (fun conf -> conf.fmt_opts.sequence_blank_line)

  let sequence_style =
    let doc = "Style of sequence." in
    let names = ["sequence-style"] in
    let all =
      [ Decl.Value.make ~name:"terminator" `Terminator
          "$(b,terminator) only puts spaces after semicolons."
      ; Decl.Value.make ~name:"separator" `Separator
          "$(b,separator) puts spaces before and after semicolons."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) breaks the sequence before semicolons." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with sequence_style= elt}))
      (fun conf -> conf.fmt_opts.sequence_style)

  let single_case =
    let doc =
      "Style of pattern matching expressions with only a single case."
    in
    let names = ["single-case"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format a single case on a single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break the line before a single case." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with single_case= elt}))
      (fun conf -> conf.fmt_opts.single_case)

  let space_around_arrays =
    let doc = "Add a space inside the delimiters of arrays." in
    let names = ["space-around-arrays"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_arrays= elt}) )
      (fun conf -> conf.fmt_opts.space_around_arrays)

  let space_around_lists =
    let doc = "Add a space inside the delimiters of lists." in
    let names = ["space-around-lists"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_lists= elt}) )
      (fun conf -> conf.fmt_opts.space_around_lists)

  let space_around_records =
    let doc = "Add a space inside the delimiters of records." in
    let names = ["space-around-records"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_records= elt}) )
      (fun conf -> conf.fmt_opts.space_around_records)

  let space_around_variants =
    let doc = "Add a space inside the delimiters of variants." in
    let names = ["space-around-variants"] in
    Decl.flag ~names ~default ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_variants= elt}) )
      (fun conf -> conf.fmt_opts.space_around_variants)

  let stritem_extension_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of structure items inside extension nodes ($(docv) \
       columns)."
    in
    let names = ["stritem-extension-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with stritem_extension_indent= elt}) )
      (fun conf -> conf.fmt_opts.stritem_extension_indent)

  let type_decl =
    let doc = "Style of type declaration." in
    let names = ["type-decl"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format constructors and records \
           definition in a single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break between constructors and record \
           fields." ]
    in
    Decl.choice ~names ~all ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with type_decl= elt}))
      (fun conf -> conf.fmt_opts.type_decl)

  let type_decl_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of type declarations ($(docv) columns) if they do not \
       fit on a single line."
    in
    let names = ["type-decl-indent"] in
    Decl.int ~names ~default ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with type_decl_indent= elt}) )
      (fun conf -> conf.fmt_opts.type_decl_indent)

  let wrap_comments =
    let doc =
      "Comments are divided into paragraphs by open lines (two or more \
       consecutive newlines), and each paragraph is wrapped at the margin. \
       Multi-line comments with vertically-aligned asterisks on the left \
       margin are not wrapped. Consecutive comments with both left and \
       right margin aligned are not wrapped either."
    in
    Decl.flag ~default ~names:["wrap-comments"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with wrap_comments= elt}))
      (fun conf -> conf.fmt_opts.wrap_comments)

  let wrap_fun_args =
    let doc = "Style for function call." in
    let names = ["wrap-fun-args"] in
    Decl.flag ~default ~names ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with wrap_fun_args= elt}))
      (fun conf -> conf.fmt_opts.wrap_fun_args)

  let options =
    Store.
      [ elt assignment_operator
      ; elt break_before_in
      ; elt break_cases
      ; elt break_collection_expressions
      ; elt break_colon
      ; elt break_fun_decl
      ; elt break_fun_sig
      ; elt break_infix
      ; elt break_infix_before_func
      ; elt break_separators
      ; elt break_sequences
      ; elt break_string_literals
      ; elt break_struct
      ; elt cases_exp_indent
      ; elt cases_matching_exp_indent
      ; elt disambiguate_non_breaking_match
      ; elt doc_comments
      ; elt doc_comments_padding
      ; elt doc_comments_tag_only
      ; elt dock_collection_brackets
      ; elt exp_grouping
      ; elt extension_indent
      ; elt field_space
      ; elt function_indent
      ; elt function_indent_nested
      ; elt if_then_else
      ; elt indent_after_in
      ; elt indicate_multiline_delimiters
      ; elt indicate_nested_or_patterns
      ; elt infix_precedence
      ; elt leading_nested_match_parens
      ; elt let_and
      ; elt let_binding_indent
      ; elt let_binding_deindent_fun
      ; elt let_binding_spacing
      ; elt let_module
      ; elt line_endings
      ; elt margin
      ; elt match_indent
      ; elt match_indent_nested
      ; elt max_indent
      ; elt module_item_spacing
      ; elt nested_match
      ; elt ocp_indent_compat
      ; elt parens_ite
      ; elt parens_tuple
      ; elt parens_tuple_patterns
      ; elt parse_docstrings
      ; elt parse_toplevel_phrases
      ; elt sequence_blank_line
      ; elt sequence_style
      ; elt single_case
      ; elt space_around_arrays
      ; elt space_around_lists
      ; elt space_around_records
      ; elt space_around_variants
      ; elt stritem_extension_indent
      ; elt type_decl
      ; elt type_decl_indent
      ; elt wrap_comments
      ; elt wrap_fun_args
      ; (* removed options *)
        elt align_cases
      ; elt align_constructors_decl
      ; elt align_variants_decl
      ; elt doc_comments_val
      ; elt escape_chars
      ; elt escape_strings
      ; elt extension_sugar
      ; elt let_open ]
end

(* Flags that can be modified in the config file that don't affect
   formatting *)

let kind = Decl.Operational

module Operational = struct
  let update ~f c = {c with opr_opts= f c.opr_opts}

  let comment_check =
    let doc =
      "Control whether to check comments and documentation comments. Unsafe \
       to turn off. May be set in $(b,.ocamlformat)."
    in
    Decl.flag ~default ~names:["comment-check"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with comment_check= elt}))
      (fun conf -> conf.opr_opts.comment_check)

  let debug =
    let doc = "Generate debugging output." in
    Decl.flag ~default ~names:["g"; "debug"] ~doc ~kind
      (fun conf elt ->
        if elt.v then Box_debug.enable_stacktraces := true ;
        update conf ~f:(fun f -> {f with debug= elt}) )
      (fun conf -> conf.opr_opts.debug)

  let disable =
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(b,[@@@ocamlformat \
       \"enable\"]) instead of $(b,[@@@ocamlformat \"disable=false\"])."
    in
    Decl.flag ~names:["disable"] ~default ~doc ~kind ~allow_inline:true
      (fun conf elt -> update conf ~f:(fun f -> {f with disable= elt}))
      (fun conf -> conf.opr_opts.disable)

  let margin_check =
    let doc = "Emit a warning if the formatted output exceeds the margin." in
    Decl.flag ~default ~names:["margin-check"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with margin_check= elt}))
      (fun conf -> conf.opr_opts.margin_check)

  let max_iters =
    let docv = "N" in
    let doc =
      "Fail if output of formatting does not stabilize within $(docv) \
       iterations. May be set in $(b,.ocamlformat)."
    in
    Decl.int ~names:["n"; "max-iters"] ~default ~doc ~docv ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with max_iters= elt}))
      (fun conf -> conf.opr_opts.max_iters)

  let ocaml_version =
    let doc = "Version of OCaml syntax of the output." in
    Decl.ocaml_version ~names:["ocaml-version"] ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with ocaml_version= elt}))
      (fun conf -> conf.opr_opts.ocaml_version)

  let quiet =
    let doc = "Quiet. May be set in $(b,.ocamlformat)." in
    Decl.flag ~default ~names:["q"; "quiet"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with quiet= elt}))
      (fun conf -> conf.opr_opts.quiet)

  let disable_conf_attrs =
    let doc = "Disable configuration in attributes." in
    Decl.flag ~default ~names:["disable-conf-attrs"] ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with disable_conf_attrs= elt}) )
      (fun conf -> conf.opr_opts.disable_conf_attrs)

  let version_check =
    let doc =
      "Check that the version matches the one specified in .ocamlformat."
    in
    Decl.flag ~default ~names:["version-check"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with version_check= elt}))
      (fun conf -> conf.opr_opts.version_check)

  let options : Store.t =
    Store.
      [ elt comment_check
      ; elt debug
      ; elt disable
      ; elt margin_check
      ; elt max_iters
      ; elt ocaml_version
      ; elt quiet
      ; elt disable_conf_attrs
      ; elt version_check ]
end

let options = Operational.options @ Formatting.options @ options

let parse_line config ?(version_check = config.opr_opts.version_check.v)
    ?(disable_conf_attrs = config.opr_opts.disable_conf_attrs.v) ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.current value || not version_check then
          Ok config
        else
          Error
            (Error.Version_mismatch {read= value; installed= Version.current})
    | name, `File x ->
        Decl.update options ~config
          ~from:(`Parsed (`File x))
          ~name ~value ~inline:false
    | name, `Attribute loc ->
        if disable_conf_attrs then (
          warn ~loc "Configuration in attribute %S ignored." s ;
          Ok config )
        else
          Decl.update options ~config
            ~from:(`Parsed (`Attribute loc))
            ~name ~value ~inline:true
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
  | _ -> Error (Error.Malformed s)

open Parsetree

let parse_attr {attr_name= {txt; loc= _}; attr_payload; _} =
  match txt with
  | "ocamlformat" -> (
    match attr_payload with
    | PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc=
                      Pexp_constant
                        {pconst_desc= Pconst_string (str, strloc, None); _}
                  ; pexp_attributes= []
                  ; _ }
                , [] )
          ; _ } ] ->
        Ok (str, strloc)
    | _ -> Error (`Msg "Invalid format: String expected") )
  | _ when String.is_prefix ~prefix:"ocamlformat." txt ->
      Error
        (`Msg
           (Format.sprintf "Invalid format: Unknown suffix %S"
              (String.chop_prefix_exn ~prefix:"ocamlformat." txt) ) )
  | _ -> Error `Ignore

let update ?(quiet = false) c ({attr_name= {txt; loc}; _} as attr) =
  let result =
    match parse_attr attr with
    | Ok (str, strloc) ->
        parse_line ~from:(`Attribute strloc) c str
        |> Result.map_error ~f:Error.to_string
    | Error (`Msg msg) -> Error msg
    | Error `Ignore -> Ok c
  in
  match result with
  | Ok conf -> conf
  | Error error ->
      let w = Warnings.Attribute_payload (txt, error) in
      if (not c.opr_opts.quiet.v) && not quiet then
        Warning.print_warning loc w ;
      c

let update_value config ~name ~value =
  Decl.update options ~config ~from:`Commandline ~name ~value ~inline:false

let update_state c state =
  let disable = match state with `Enable -> false | `Disable -> true in
  let opr_opts =
    {c.opr_opts with disable= {c.opr_opts.disable with v= disable}}
  in
  {c with opr_opts}

let parse_state_attr attr =
  match parse_attr attr with
  | Ok ("enable", _) -> Some `Enable
  | Ok ("disable", _) -> Some `Disable
  | _ -> None

let print_config = Decl.print_config options

let term = Decl.Store.to_term options

module UI = struct
  let profile = Decl.to_ui profile

  let opr_opts = Store.to_ui Operational.options

  let fmt_opts = Store.to_ui Formatting.options
end
