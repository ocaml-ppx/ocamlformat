
(rule
 (targets align_cases-break_all.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:align_cases-break_all.ml.opts} %{dep:align_cases.ml}"))))

(alias
 (name runtest)
 (action (diff align_cases-break_all.ml.ref align_cases-break_all.ml.output)))

(rule
 (targets align_cases.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:align_cases.ml.opts} %{dep:align_cases.ml}"))))

(alias
 (name runtest)
 (action (diff align_cases.ml align_cases.ml.output)))

(rule
 (targets align_infix.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:align_infix.ml.opts} %{dep:align_infix.ml}"))))

(alias
 (name runtest)
 (action (diff align_infix.ml align_infix.ml.output)))

(rule
 (targets apply.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:apply.ml}"))))

(alias
 (name runtest)
 (action (diff apply.ml apply.ml.output)))

(rule
 (targets array.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:array.ml}"))))

(alias
 (name runtest)
 (action (diff array.ml array.ml.output)))

(rule
 (targets assignment_operator-op_begin_line.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:assignment_operator-op_begin_line.ml.opts} %{dep:assignment_operator.ml}"))))

(alias
 (name runtest)
 (action (diff assignment_operator-op_begin_line.ml.ref assignment_operator-op_begin_line.ml.output)))

(rule
 (targets assignment_operator.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:assignment_operator.ml}"))))

(alias
 (name runtest)
 (action (diff assignment_operator.ml.ref assignment_operator.ml.output)))

(rule
 (targets attribute_and_expression.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:attribute_and_expression.ml}"))))

(alias
 (name runtest)
 (action (diff attribute_and_expression.ml attribute_and_expression.ml.output)))

(rule
 (targets attributes.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:attributes.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff attributes.ml attributes.ml.output)))

(rule
 (targets attributes.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:attributes.mli}"))))

(alias
 (name runtest)
 (action (diff attributes.mli.ref attributes.mli.output)))

(rule
 (targets break_before_in-auto.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_before_in-auto.ml.opts} %{dep:break_before_in.ml}"))))

(alias
 (name runtest)
 (action (diff break_before_in-auto.ml.ref break_before_in-auto.ml.output)))

(rule
 (targets break_before_in.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_before_in.ml.opts} %{dep:break_before_in.ml}"))))

(alias
 (name runtest)
 (action (diff break_before_in.ml break_before_in.ml.output)))

(rule
 (targets break_cases-align.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-align.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-align.ml.ref break_cases-align.ml.output)))

(rule
 (targets break_cases-all.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-all.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-all.ml.ref break_cases-all.ml.output)))

(rule
 (targets break_cases-closing_on_separate_line.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-closing_on_separate_line.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-closing_on_separate_line.ml.ref break_cases-closing_on_separate_line.ml.output)))

(rule
 (targets break_cases-closing_on_separate_line_leading_nested_match_parens.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-closing_on_separate_line_leading_nested_match_parens.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-closing_on_separate_line_leading_nested_match_parens.ml.ref break_cases-closing_on_separate_line_leading_nested_match_parens.ml.output)))

(rule
 (targets break_cases-cosl_lnmp_cmei.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-cosl_lnmp_cmei.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-cosl_lnmp_cmei.ml.ref break_cases-cosl_lnmp_cmei.ml.output)))

(rule
 (targets break_cases-fit_or_vertical.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-fit_or_vertical.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-fit_or_vertical.ml.ref break_cases-fit_or_vertical.ml.output)))

(rule
 (targets break_cases-nested.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-nested.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-nested.ml.ref break_cases-nested.ml.output)))

(rule
 (targets break_cases-normal_indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-normal_indent.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-normal_indent.ml.ref break_cases-normal_indent.ml.output)))

(rule
 (targets break_cases-toplevel.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases-toplevel.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases-toplevel.ml.ref break_cases-toplevel.ml.output)))

(rule
 (targets break_cases.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_cases.ml.opts} %{dep:break_cases.ml}"))))

(alias
 (name runtest)
 (action (diff break_cases.ml.ref break_cases.ml.output)))

(rule
 (targets break_fun_decl-fit_or_vertical.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_fun_decl-fit_or_vertical.ml.opts} %{dep:break_fun_decl.ml}"))))

(alias
 (name runtest)
 (action (diff break_fun_decl-fit_or_vertical.ml.ref break_fun_decl-fit_or_vertical.ml.output)))

(rule
 (targets break_fun_decl-smart.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_fun_decl-smart.ml.opts} %{dep:break_fun_decl.ml}"))))

(alias
 (name runtest)
 (action (diff break_fun_decl-smart.ml.ref break_fun_decl-smart.ml.output)))

(rule
 (targets break_fun_decl-wrap.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_fun_decl-wrap.ml.opts} %{dep:break_fun_decl.ml}"))))

(alias
 (name runtest)
 (action (diff break_fun_decl-wrap.ml.ref break_fun_decl-wrap.ml.output)))

(rule
 (targets break_fun_decl.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:break_fun_decl.ml}"))))

(alias
 (name runtest)
 (action (diff break_fun_decl.ml break_fun_decl.ml.output)))

(rule
 (targets break_record.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_record.ml.opts} %{dep:break_record.ml}"))))

(alias
 (name runtest)
 (action (diff break_record.ml break_record.ml.output)))

(rule
 (targets break_separators-after.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-after.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-after.ml.ref break_separators-after.ml.output)))

(rule
 (targets break_separators-after_docked.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-after_docked.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-after_docked.ml.ref break_separators-after_docked.ml.output)))

(rule
 (targets break_separators-after_docked_transition_message.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "! %{bin:ocamlformat} %{read-lines:break_separators-after_docked_transition_message.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-after_docked_transition_message.ml.ref break_separators-after_docked_transition_message.ml.output)))

(rule
 (targets break_separators-after_docked_wrap.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-after_docked_wrap.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-after_docked_wrap.ml.ref break_separators-after_docked_wrap.ml.output)))

(rule
 (targets break_separators-after_wrap.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-after_wrap.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-after_wrap.ml.ref break_separators-after_wrap.ml.output)))

(rule
 (targets break_separators-before_docked.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-before_docked.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-before_docked.ml.ref break_separators-before_docked.ml.output)))

(rule
 (targets break_separators-before_docked_wrap.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-before_docked_wrap.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-before_docked_wrap.ml.ref break_separators-before_docked_wrap.ml.output)))

(rule
 (targets break_separators-wrap.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators-wrap.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators-wrap.ml.ref break_separators-wrap.ml.output)))

(rule
 (targets break_separators.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_separators.ml.opts} %{dep:break_separators.ml}"))))

(alias
 (name runtest)
 (action (diff break_separators.ml break_separators.ml.output)))

(rule
 (targets break_sequence_before.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:break_sequence_before.ml}"))))

(alias
 (name runtest)
 (action (diff break_sequence_before.ml break_sequence_before.ml.output)))

(rule
 (targets break_string_literals-never.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_string_literals-never.ml.opts} %{dep:break_string_literals.ml}"))))

(alias
 (name runtest)
 (action (diff break_string_literals-never.ml.ref break_string_literals-never.ml.output)))

(rule
 (targets break_string_literals-wrap.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "! %{bin:ocamlformat} %{read-lines:break_string_literals-wrap.ml.opts} %{dep:break_string_literals.ml}"))))

(alias
 (name runtest)
 (action (diff break_string_literals-wrap.ml.ref break_string_literals-wrap.ml.output)))

(rule
 (targets break_string_literals.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:break_string_literals.ml.opts} %{dep:break_string_literals.ml}"))))

(alias
 (name runtest)
 (action (diff break_string_literals.ml.ref break_string_literals.ml.output)))

(rule
 (targets break_struct.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:break_struct.ml}"))))

(alias
 (name runtest)
 (action (diff break_struct.ml.ref break_struct.ml.output)))

(rule
 (targets cinaps.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:cinaps.ml}"))))

(alias
 (name runtest)
 (action (diff cinaps.ml cinaps.ml.output)))

(rule
 (targets cmdline_override.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:cmdline_override.ml.opts} %{dep:cmdline_override.ml}"))))

(alias
 (name runtest)
 (action (diff cmdline_override.ml cmdline_override.ml.output)))

(rule
 (targets cmdline_override2.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:cmdline_override2.ml.opts} %{dep:cmdline_override2.ml}"))))

(alias
 (name runtest)
 (action (diff cmdline_override2.ml cmdline_override2.ml.output)))

(rule
 (targets comment_breaking.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comment_breaking.ml}"))))

(alias
 (name runtest)
 (action (diff comment_breaking.ml comment_breaking.ml.output)))

(rule
 (targets comment_header.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comment_header.ml}"))))

(alias
 (name runtest)
 (action (diff comment_header.ml.ref comment_header.ml.output)))

(rule
 (targets comment_in_empty.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comment_in_empty.ml}"))))

(alias
 (name runtest)
 (action (diff comment_in_empty.ml comment_in_empty.ml.output)))

(rule
 (targets comment_in_modules.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comment_in_modules.ml}"))))

(alias
 (name runtest)
 (action (diff comment_in_modules.ml.ref comment_in_modules.ml.output)))

(rule
 (targets comment_last.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comment_last.ml}"))))

(alias
 (name runtest)
 (action (diff comment_last.ml comment_last.ml.output)))

(rule
 (targets comment_sparse.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comment_sparse.ml}"))))

(alias
 (name runtest)
 (action (diff comment_sparse.ml comment_sparse.ml.output)))

(rule
 (targets comments.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:comments.ml.opts} %{dep:comments.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff comments.ml.ref comments.ml.output)))

(rule
 (targets comments_args.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:comments_args.ml.opts} %{dep:comments_args.ml}"))))

(alias
 (name runtest)
 (action (diff comments_args.ml.ref comments_args.ml.output)))

(rule
 (targets comments_around_disabled.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comments_around_disabled.ml}"))))

(alias
 (name runtest)
 (action (diff comments_around_disabled.ml.ref comments_around_disabled.ml.output)))

(rule
 (targets comments_in_record.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:comments_in_record.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff comments_in_record.ml.ref comments_in_record.ml.output)))

(rule
 (targets compact_lists_arrays.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:compact_lists_arrays.ml}"))))

(alias
 (name runtest)
 (action (diff compact_lists_arrays.ml compact_lists_arrays.ml.output)))

(rule
 (targets custom_list.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:custom_list.ml}"))))

(alias
 (name runtest)
 (action (diff custom_list.ml custom_list.ml.output)))

(rule
 (targets directives.mlt.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:directives.mlt}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff directives.mlt.ref directives.mlt.output)))

(rule
 (targets disabled.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:disabled.ml.opts} %{dep:disabled.ml}"))))

(alias
 (name runtest)
 (action (diff disabled.ml disabled.ml.output)))

(rule
 (targets disambiguate.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:disambiguate.ml}"))))

(alias
 (name runtest)
 (action (diff disambiguate.ml disambiguate.ml.output)))

(rule
 (targets doc_comments-after.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:doc_comments-after.ml.opts} %{dep:doc_comments.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff doc_comments-after.ml.ref doc_comments-after.ml.output)))

(rule
 (targets doc_comments-before.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:doc_comments-before.ml.opts} %{dep:doc_comments.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff doc_comments-before.ml.ref doc_comments-before.ml.output)))

(rule
 (targets doc_comments-val-after.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:doc_comments-val-after.ml.opts} %{dep:doc_comments.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff doc_comments-val-after.ml.ref doc_comments-val-after.ml.output)))

(rule
 (targets doc_comments.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:doc_comments.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff doc_comments.ml.ref doc_comments.ml.output)))

(rule
 (targets doc_comments.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:doc_comments.mli}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff doc_comments.mli.ref doc_comments.mli.output)))

(rule
 (targets doc_comments_padding.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:doc_comments_padding.ml}"))))

(alias
 (name runtest)
 (action (diff doc_comments_padding.ml doc_comments_padding.ml.output)))

(rule
 (targets empty.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:empty.ml}"))))

(alias
 (name runtest)
 (action (diff empty.ml empty.ml.output)))

(rule
 (targets empty_ml.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:empty_ml.ml}"))))

(alias
 (name runtest)
 (action (diff empty_ml.ml empty_ml.ml.output)))

(rule
 (targets empty_mli.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:empty_mli.mli}"))))

(alias
 (name runtest)
 (action (diff empty_mli.mli empty_mli.mli.output)))

(rule
 (targets empty_mlt.mlt.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:empty_mlt.mlt}"))))

(alias
 (name runtest)
 (action (diff empty_mlt.mlt empty_mlt.mlt.output)))

(rule
 (targets error1.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "! %{bin:ocamlformat} %{dep:error1.ml}"))))

(alias
 (name runtest)
 (action (diff error1.ml.ref error1.ml.output)))

(rule
 (targets error2.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "! %{bin:ocamlformat} %{dep:error2.ml}"))))

(alias
 (name runtest)
 (action (diff error2.ml.ref error2.ml.output)))

(rule
 (targets error3.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "! %{bin:ocamlformat} %{dep:error3.ml}"))))

(alias
 (name runtest)
 (action (diff error3.ml.ref error3.ml.output)))

(rule
 (targets error4.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:error4.ml.opts} %{dep:error4.ml}"))))

(alias
 (name runtest)
 (action (diff error4.ml.ref error4.ml.output)))

(rule
 (targets exceptions.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:exceptions.ml}"))))

(alias
 (name runtest)
 (action (diff exceptions.ml exceptions.ml.output)))

(rule
 (targets exp_grouping-parens.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:exp_grouping-parens.ml.opts} %{dep:exp_grouping.ml}"))))

(alias
 (name runtest)
 (action (diff exp_grouping-parens.ml.ref exp_grouping-parens.ml.output)))

(rule
 (targets exp_grouping.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:exp_grouping.ml.opts} %{dep:exp_grouping.ml}"))))

(alias
 (name runtest)
 (action (diff exp_grouping.ml.ref exp_grouping.ml.output)))

(rule
 (targets exp_record.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:exp_record.ml}"))))

(alias
 (name runtest)
 (action (diff exp_record.ml exp_record.ml.output)))

(rule
 (targets expect_test.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:expect_test.ml}"))))

(alias
 (name runtest)
 (action (diff expect_test.ml expect_test.ml.output)))

(rule
 (targets extensions-indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:extensions-indent.ml.opts} %{dep:extensions.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff extensions-indent.ml.ref extensions-indent.ml.output)))

(rule
 (targets extensions-indent.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:extensions-indent.mli.opts} %{dep:extensions.mli}"))))

(alias
 (name runtest)
 (action (diff extensions-indent.mli.ref extensions-indent.mli.output)))

(rule
 (targets extensions-sugar_always.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:extensions-sugar_always.ml.opts} %{dep:extensions.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff extensions-sugar_always.ml.ref extensions-sugar_always.ml.output)))

(rule
 (targets extensions.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:extensions.ml.opts} %{dep:extensions.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff extensions.ml.ref extensions.ml.output)))

(rule
 (targets extensions.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:extensions.mli}"))))

(alias
 (name runtest)
 (action (diff extensions.mli extensions.mli.output)))

(rule
 (targets field-op_begin_line.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:field-op_begin_line.ml.opts} %{dep:field.ml}"))))

(alias
 (name runtest)
 (action (diff field-op_begin_line.ml.ref field-op_begin_line.ml.output)))

(rule
 (targets field.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:field.ml}"))))

(alias
 (name runtest)
 (action (diff field.ml field.ml.output)))

(rule
 (targets first_class_module.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:first_class_module.ml}"))))

(alias
 (name runtest)
 (action (diff first_class_module.ml first_class_module.ml.output)))

(rule
 (targets floating_doc.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:floating_doc.ml}"))))

(alias
 (name runtest)
 (action (diff floating_doc.ml floating_doc.ml.output)))

(rule
 (targets for_while.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:for_while.ml}"))))

(alias
 (name runtest)
 (action (diff for_while.ml for_while.ml.output)))

(rule
 (targets format_invalid_files.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:format_invalid_files.ml.opts} %{dep:format_invalid_files.ml}"))))

(alias
 (name runtest)
 (action (diff format_invalid_files.ml.ref format_invalid_files.ml.output)))

(rule
 (targets fun_decl.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:fun_decl.ml}"))))

(alias
 (name runtest)
 (action (diff fun_decl.ml fun_decl.ml.output)))

(rule
 (targets function_indent-never.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:function_indent-never.ml.opts} %{dep:function_indent.ml}"))))

(alias
 (name runtest)
 (action (diff function_indent-never.ml.ref function_indent-never.ml.output)))

(rule
 (targets function_indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:function_indent.ml.opts} %{dep:function_indent.ml}"))))

(alias
 (name runtest)
 (action (diff function_indent.ml.ref function_indent.ml.output)))

(rule
 (targets functor.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:functor.ml}"))))

(alias
 (name runtest)
 (action (diff functor.ml functor.ml.output)))

(rule
 (targets funsig.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:funsig.ml}"))))

(alias
 (name runtest)
 (action (diff funsig.ml funsig.ml.output)))

(rule
 (targets gadt.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:gadt.ml}"))))

(alias
 (name runtest)
 (action (diff gadt.ml gadt.ml.output)))

(rule
 (targets generative.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:generative.ml}"))))

(alias
 (name runtest)
 (action (diff generative.ml generative.ml.output)))

(rule
 (targets index_op.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:index_op.ml}"))))

(alias
 (name runtest)
 (action (diff index_op.ml index_op.ml.output)))

(rule
 (targets infix_arg_grouping.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:infix_arg_grouping.ml}"))))

(alias
 (name runtest)
 (action (diff infix_arg_grouping.ml infix_arg_grouping.ml.output)))

(rule
 (targets infix_bind-break.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:infix_bind-break.ml.opts} %{dep:infix_bind.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff infix_bind-break.ml.ref infix_bind-break.ml.output)))

(rule
 (targets infix_bind-fit_or_vertical-break.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:infix_bind-fit_or_vertical-break.ml.opts} %{dep:infix_bind.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff infix_bind-fit_or_vertical-break.ml.ref infix_bind-fit_or_vertical-break.ml.output)))

(rule
 (targets infix_bind-fit_or_vertical.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:infix_bind-fit_or_vertical.ml.opts} %{dep:infix_bind.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff infix_bind-fit_or_vertical.ml.ref infix_bind-fit_or_vertical.ml.output)))

(rule
 (targets infix_bind.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:infix_bind.ml.opts} %{dep:infix_bind.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff infix_bind.ml infix_bind.ml.output)))

(rule
 (targets infix_precedence.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:infix_precedence.ml.opts} %{dep:infix_precedence.ml}"))))

(alias
 (name runtest)
 (action (diff infix_precedence.ml infix_precedence.ml.output)))

(rule
 (targets invalid.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:invalid.ml}"))))

(alias
 (name runtest)
 (action (diff invalid.ml invalid.ml.output)))

(rule
 (targets issue114.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue114.ml}"))))

(alias
 (name runtest)
 (action (diff issue114.ml issue114.ml.output)))

(rule
 (targets issue289.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue289.ml}"))))

(alias
 (name runtest)
 (action (diff issue289.ml issue289.ml.output)))

(rule
 (targets issue48.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue48.ml}"))))

(alias
 (name runtest)
 (action (diff issue48.ml issue48.ml.output)))

(rule
 (targets issue51.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue51.ml}"))))

(alias
 (name runtest)
 (action (diff issue51.ml issue51.ml.output)))

(rule
 (targets issue57.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue57.ml}"))))

(alias
 (name runtest)
 (action (diff issue57.ml issue57.ml.output)))

(rule
 (targets issue60.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue60.ml}"))))

(alias
 (name runtest)
 (action (diff issue60.ml issue60.ml.output)))

(rule
 (targets issue77.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue77.ml}"))))

(alias
 (name runtest)
 (action (diff issue77.ml issue77.ml.output)))

(rule
 (targets issue85.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue85.ml}"))))

(alias
 (name runtest)
 (action (diff issue85.ml issue85.ml.output)))

(rule
 (targets issue89.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:issue89.ml}"))))

(alias
 (name runtest)
 (action (diff issue89.ml issue89.ml.output)))

(rule
 (targets ite-compact.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-compact.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-compact.ml.ref ite-compact.ml.output)))

(rule
 (targets ite-compact_closing.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-compact_closing.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-compact_closing.ml.ref ite-compact_closing.ml.output)))

(rule
 (targets ite-fit_or_vertical.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-fit_or_vertical.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-fit_or_vertical.ml.ref ite-fit_or_vertical.ml.output)))

(rule
 (targets ite-fit_or_vertical_closing.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-fit_or_vertical_closing.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-fit_or_vertical_closing.ml.ref ite-fit_or_vertical_closing.ml.output)))

(rule
 (targets ite-fit_or_vertical_no_indicate.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-fit_or_vertical_no_indicate.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-fit_or_vertical_no_indicate.ml.ref ite-fit_or_vertical_no_indicate.ml.output)))

(rule
 (targets ite-kr.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-kr.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-kr.ml.ref ite-kr.ml.output)))

(rule
 (targets ite-kr_closing.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-kr_closing.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-kr_closing.ml.ref ite-kr_closing.ml.output)))

(rule
 (targets ite-kw_first.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-kw_first.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-kw_first.ml.ref ite-kw_first.ml.output)))

(rule
 (targets ite-kw_first_closing.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-kw_first_closing.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-kw_first_closing.ml.ref ite-kw_first_closing.ml.output)))

(rule
 (targets ite-kw_first_no_indicate.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-kw_first_no_indicate.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-kw_first_no_indicate.ml.ref ite-kw_first_no_indicate.ml.output)))

(rule
 (targets ite-no_indicate.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite-no_indicate.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite-no_indicate.ml.ref ite-no_indicate.ml.output)))

(rule
 (targets ite.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ite.ml.opts} %{dep:ite.ml}"))))

(alias
 (name runtest)
 (action (diff ite.ml.ref ite.ml.output)))

(rule
 (targets js_sig.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:js_sig.mli.opts} %{dep:js_sig.mli}"))))

(alias
 (name runtest)
 (action (diff js_sig.mli.ref js_sig.mli.output)))

(rule
 (targets js_source.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:js_source.ml.opts} %{dep:js_source.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff js_source.ml.ref js_source.ml.output)))

(rule
 (targets js_source.ml.ocp.output)
 (deps .ocp-indent )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocp-indent} %{dep:js_source.ml.ref}"))))

(alias
 (name runtest)
 (action (diff js_source.ml.ocp js_source.ml.ocp.output)))

(rule
 (targets kw_extentions.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:kw_extentions.ml}"))))

(alias
 (name runtest)
 (action (diff kw_extentions.ml kw_extentions.ml.output)))

(rule
 (targets label_option_default_args.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:label_option_default_args.ml.opts} %{dep:label_option_default_args.ml}"))))

(alias
 (name runtest)
 (action (diff label_option_default_args.ml.ref label_option_default_args.ml.output)))

(rule
 (targets lazy.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:lazy.ml}"))))

(alias
 (name runtest)
 (action (diff lazy.ml lazy.ml.output)))

(rule
 (targets let_binding-in_indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:let_binding-in_indent.ml.opts} %{dep:let_binding.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff let_binding-in_indent.ml.ref let_binding-in_indent.ml.output)))

(rule
 (targets let_binding-indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:let_binding-indent.ml.opts} %{dep:let_binding.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff let_binding-indent.ml.ref let_binding-indent.ml.output)))

(rule
 (targets let_binding.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:let_binding.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff let_binding.ml.ref let_binding.ml.output)))

(rule
 (targets let_in_constr.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:let_in_constr.ml}"))))

(alias
 (name runtest)
 (action (diff let_in_constr.ml let_in_constr.ml.output)))

(rule
 (targets let_module-sparse.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:let_module-sparse.ml.opts} %{dep:let_module.ml}"))))

(alias
 (name runtest)
 (action (diff let_module-sparse.ml.ref let_module-sparse.ml.output)))

(rule
 (targets let_module.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:let_module.ml.opts} %{dep:let_module.ml}"))))

(alias
 (name runtest)
 (action (diff let_module.ml.ref let_module.ml.output)))

(rule
 (targets list-space_around.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:list-space_around.ml.opts} %{dep:list.ml}"))))

(alias
 (name runtest)
 (action (diff list-space_around.ml.ref list-space_around.ml.output)))

(rule
 (targets list.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:list.ml}"))))

(alias
 (name runtest)
 (action (diff list.ml list.ml.output)))

(rule
 (targets loc_stack.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:loc_stack.ml.opts} %{dep:loc_stack.ml}"))))

(alias
 (name runtest)
 (action (diff loc_stack.ml.ref loc_stack.ml.output)))

(rule
 (targets locally_abtract_types.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:locally_abtract_types.ml}"))))

(alias
 (name runtest)
 (action (diff locally_abtract_types.ml locally_abtract_types.ml.output)))

(rule
 (targets margin_80.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:margin_80.ml.opts} %{dep:margin_80.ml}"))))

(alias
 (name runtest)
 (action (diff margin_80.ml.ref margin_80.ml.output)))

(rule
 (targets match.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:match.ml}"))))

(alias
 (name runtest)
 (action (diff match.ml match.ml.output)))

(rule
 (targets match2.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:match2.ml.opts} %{dep:match2.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff match2.ml match2.ml.output)))

(rule
 (targets match_indent-never.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:match_indent-never.ml.opts} %{dep:match_indent.ml}"))))

(alias
 (name runtest)
 (action (diff match_indent-never.ml.ref match_indent-never.ml.output)))

(rule
 (targets match_indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:match_indent.ml.opts} %{dep:match_indent.ml}"))))

(alias
 (name runtest)
 (action (diff match_indent.ml.ref match_indent.ml.output)))

(rule
 (targets max_indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:max_indent.ml.opts} %{dep:max_indent.ml}"))))

(alias
 (name runtest)
 (action (diff max_indent.ml max_indent.ml.output)))

(rule
 (targets module.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:module.ml}"))))

(alias
 (name runtest)
 (action (diff module.ml module.ml.output)))

(rule
 (targets module_anonymous.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:module_anonymous.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.10.0))
 (action (diff module_anonymous.ml module_anonymous.ml.output)))

(rule
 (targets module_attributes.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:module_attributes.ml}"))))

(alias
 (name runtest)
 (action (diff module_attributes.ml.ref module_attributes.ml.output)))

(rule
 (targets module_item_spacing-preserve.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:module_item_spacing-preserve.ml.opts} %{dep:module_item_spacing.ml}"))))

(alias
 (name runtest)
 (action (diff module_item_spacing-preserve.ml.ref module_item_spacing-preserve.ml.output)))

(rule
 (targets module_item_spacing-sparse.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:module_item_spacing-sparse.ml.opts} %{dep:module_item_spacing.ml}"))))

(alias
 (name runtest)
 (action (diff module_item_spacing-sparse.ml.ref module_item_spacing-sparse.ml.output)))

(rule
 (targets module_item_spacing.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:module_item_spacing.ml.opts} %{dep:module_item_spacing.ml}"))))

(alias
 (name runtest)
 (action (diff module_item_spacing.ml.ref module_item_spacing.ml.output)))

(rule
 (targets module_item_spacing.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:module_item_spacing.mli.opts} %{dep:module_item_spacing.mli}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff module_item_spacing.mli.ref module_item_spacing.mli.output)))

(rule
 (targets module_type.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:module_type.ml}"))))

(alias
 (name runtest)
 (action (diff module_type.ml module_type.ml.output)))

(rule
 (targets monadic_binding.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:monadic_binding.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff monadic_binding.ml monadic_binding.ml.output)))

(rule
 (targets multi_index_op.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:multi_index_op.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.10.0))
 (action (diff multi_index_op.ml multi_index_op.ml.output)))

(rule
 (targets need_format.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "! %{bin:ocamlformat} %{read-lines:need_format.ml.opts} %{dep:need_format.ml}"))))

(alias
 (name runtest)
 (action (diff need_format.ml.ref need_format.ml.output)))

(rule
 (targets new.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:new.ml}"))))

(alias
 (name runtest)
 (action (diff new.ml new.ml.output)))

(rule
 (targets object.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:object.ml}"))))

(alias
 (name runtest)
 (action (diff object.ml object.ml.output)))

(rule
 (targets object_type.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:object_type.ml}"))))

(alias
 (name runtest)
 (action (diff object_type.ml.ref object_type.ml.output)))

(rule
 (targets ocp_indent_compat.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:ocp_indent_compat.ml}"))))

(alias
 (name runtest)
 (action (diff ocp_indent_compat.ml ocp_indent_compat.ml.output)))

(rule
 (targets ocp_indent_options.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:ocp_indent_options.ml.opts} %{dep:ocp_indent_options.ml}"))))

(alias
 (name runtest)
 (action (diff ocp_indent_options.ml.ref ocp_indent_options.ml.output)))

(rule
 (targets open-auto.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:open-auto.ml.opts} %{dep:open.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff open-auto.ml.ref open-auto.ml.output)))

(rule
 (targets open-long.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:open-long.ml.opts} %{dep:open.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff open-long.ml.ref open-long.ml.output)))

(rule
 (targets open-preserve.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:open-preserve.ml.opts} %{dep:open.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff open-preserve.ml.ref open-preserve.ml.output)))

(rule
 (targets open-short.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:open-short.ml.opts} %{dep:open.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff open-short.ml.ref open-short.ml.output)))

(rule
 (targets open.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:open.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff open.ml.ref open.ml.output)))

(rule
 (targets open_types.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:open_types.ml}"))))

(alias
 (name runtest)
 (action (diff open_types.ml open_types.ml.output)))

(rule
 (targets option.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:option.ml}"))))

(alias
 (name runtest)
 (action (diff option.ml.ref option.ml.output)))

(rule
 (targets parens_tuple_patterns.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:parens_tuple_patterns.ml}"))))

(alias
 (name runtest)
 (action (diff parens_tuple_patterns.ml parens_tuple_patterns.ml.output)))

(rule
 (targets precedence.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:precedence.ml}"))))

(alias
 (name runtest)
 (action (diff precedence.ml precedence.ml.output)))

(rule
 (targets prefix_infix.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:prefix_infix.ml}"))))

(alias
 (name runtest)
 (action (diff prefix_infix.ml prefix_infix.ml.output)))

(rule
 (targets print_config.ml.output)
 (deps .ocamlformat dir1/dir2/.ocamlformat dir1/dir2/print_config.ml)
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:print_config.ml.opts} %{dep:print_config.ml}"))))

(alias
 (name runtest)
 (action (diff print_config.ml.ref print_config.ml.output)))

(rule
 (targets profiles.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:profiles.ml.opts} %{dep:profiles.ml}"))))

(alias
 (name runtest)
 (action (diff profiles.ml profiles.ml.output)))

(rule
 (targets profiles2.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:profiles2.ml.opts} %{dep:profiles2.ml}"))))

(alias
 (name runtest)
 (action (diff profiles2.ml profiles2.ml.output)))

(rule
 (targets protected_object_types.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:protected_object_types.ml}"))))

(alias
 (name runtest)
 (action (diff protected_object_types.ml protected_object_types.ml.output)))

(rule
 (targets recmod.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:recmod.mli}"))))

(alias
 (name runtest)
 (action (diff recmod.mli recmod.mli.output)))

(rule
 (targets record-loose.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:record-loose.ml.opts} %{dep:record.ml}"))))

(alias
 (name runtest)
 (action (diff record-loose.ml.ref record-loose.ml.output)))

(rule
 (targets record-tight_decl.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:record-tight_decl.ml.opts} %{dep:record.ml}"))))

(alias
 (name runtest)
 (action (diff record-tight_decl.ml.ref record-tight_decl.ml.output)))

(rule
 (targets record.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:record.ml.opts} %{dep:record.ml}"))))

(alias
 (name runtest)
 (action (diff record.ml.ref record.ml.output)))

(rule
 (targets record_punning.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:record_punning.ml}"))))

(alias
 (name runtest)
 (action (diff record_punning.ml record_punning.ml.output)))

(rule
 (targets reformat_string.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:reformat_string.ml.opts} %{dep:reformat_string.ml}"))))

(alias
 (name runtest)
 (action (diff reformat_string.ml.ref reformat_string.ml.output)))

(rule
 (targets refs.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:refs.ml}"))))

(alias
 (name runtest)
 (action (diff refs.ml refs.ml.output)))

(rule
 (targets remove_extra_parens.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:remove_extra_parens.ml}"))))

(alias
 (name runtest)
 (action (diff remove_extra_parens.ml.ref remove_extra_parens.ml.output)))

(rule
 (targets revapply_ext.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:revapply_ext.ml}"))))

(alias
 (name runtest)
 (action (diff revapply_ext.ml revapply_ext.ml.output)))

(rule
 (targets send.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:send.ml}"))))

(alias
 (name runtest)
 (action (diff send.ml send.ml.output)))

(rule
 (targets sequence-preserve.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:sequence-preserve.ml.opts} %{dep:sequence.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff sequence-preserve.ml.ref sequence-preserve.ml.output)))

(rule
 (targets sequence.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:sequence.ml.opts} %{dep:sequence.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff sequence.ml.ref sequence.ml.output)))

(rule
 (targets shebang.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:shebang.ml}"))))

(alias
 (name runtest)
 (action (diff shebang.ml shebang.ml.output)))

(rule
 (targets shortcut_ext_attr.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:shortcut_ext_attr.ml}"))))

(alias
 (name runtest)
 (action (diff shortcut_ext_attr.ml shortcut_ext_attr.ml.output)))

(rule
 (targets skip.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:skip.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff skip.ml skip.ml.output)))

(rule
 (targets source.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:source.ml.opts} %{dep:source.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff source.ml.ref source.ml.output)))

(rule
 (targets str_value.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:str_value.ml}"))))

(alias
 (name runtest)
 (action (diff str_value.ml str_value.ml.output)))

(rule
 (targets string.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:string.ml}"))))

(alias
 (name runtest)
 (action (diff string.ml.ref string.ml.output)))

(rule
 (targets string_array.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:string_array.ml}"))))

(alias
 (name runtest)
 (action (diff string_array.ml string_array.ml.output)))

(rule
 (targets string_wrapping.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:string_wrapping.ml}"))))

(alias
 (name runtest)
 (action (diff string_wrapping.ml string_wrapping.ml.output)))

(rule
 (targets tag_only.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:tag_only.ml}"))))

(alias
 (name runtest)
 (action (diff tag_only.ml tag_only.ml.output)))

(rule
 (targets tag_only.mli.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:tag_only.mli}"))))

(alias
 (name runtest)
 (action (diff tag_only.mli tag_only.mli.output)))

(rule
 (targets try_with_or_pattern.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:try_with_or_pattern.ml}"))))

(alias
 (name runtest)
 (action (diff try_with_or_pattern.ml try_with_or_pattern.ml.output)))

(rule
 (targets tuple.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:tuple.ml.opts} %{dep:tuple.ml}"))))

(alias
 (name runtest)
 (action (diff tuple.ml tuple.ml.output)))

(rule
 (targets tuple_less_parens.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:tuple_less_parens.ml.opts} %{dep:tuple_less_parens.ml}"))))

(alias
 (name runtest)
 (action (diff tuple_less_parens.ml tuple_less_parens.ml.output)))

(rule
 (targets tuple_type_parens.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:tuple_type_parens.ml}"))))

(alias
 (name runtest)
 (action (diff tuple_type_parens.ml tuple_type_parens.ml.output)))

(rule
 (targets type_and_constraint.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:type_and_constraint.ml}"))))

(alias
 (name runtest)
 (action (diff type_and_constraint.ml type_and_constraint.ml.output)))

(rule
 (targets type_annotations.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:type_annotations.ml}"))))

(alias
 (name runtest)
 (action (diff type_annotations.ml type_annotations.ml.output)))

(rule
 (targets types-compact-space_around-docked.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:types-compact-space_around-docked.ml.opts} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types-compact-space_around-docked.ml.ref types-compact-space_around-docked.ml.output)))

(rule
 (targets types-compact-space_around.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:types-compact-space_around.ml.opts} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types-compact-space_around.ml.ref types-compact-space_around.ml.output)))

(rule
 (targets types-compact.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:types-compact.ml.opts} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types-compact.ml.ref types-compact.ml.output)))

(rule
 (targets types-indent.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:types-indent.ml.opts} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types-indent.ml.ref types-indent.ml.output)))

(rule
 (targets types-sparse-space_around.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:types-sparse-space_around.ml.opts} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types-sparse-space_around.ml.ref types-sparse-space_around.ml.output)))

(rule
 (targets types-sparse.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:types-sparse.ml.opts} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types-sparse.ml.ref types-sparse.ml.output)))

(rule
 (targets types.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:types.ml}"))))

(alias
 (name runtest)
 (enabled_if (>= %{ocaml_version} 4.08.0))
 (action (diff types.ml types.ml.output)))

(rule
 (targets unary.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:unary.ml}"))))

(alias
 (name runtest)
 (action (diff unary.ml.ref unary.ml.output)))

(rule
 (targets unicode.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:unicode.ml.opts} %{dep:unicode.ml}"))))

(alias
 (name runtest)
 (action (diff unicode.ml.ref unicode.ml.output)))

(rule
 (targets use_file.mlt.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:use_file.mlt}"))))

(alias
 (name runtest)
 (action (diff use_file.mlt use_file.mlt.output)))

(rule
 (targets verbose1.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:verbose1.ml.opts} %{dep:verbose1.ml}"))))

(alias
 (name runtest)
 (action (diff verbose1.ml.ref verbose1.ml.output)))

(rule
 (targets verbose2.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:verbose2.ml.opts} %{dep:verbose2.ml}"))))

(alias
 (name runtest)
 (action (diff verbose2.ml.ref verbose2.ml.output)))

(rule
 (targets wrap_comments.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:wrap_comments.ml.opts} %{dep:wrap_comments.ml}"))))

(alias
 (name runtest)
 (action (diff wrap_comments.ml.ref wrap_comments.ml.output)))

(rule
 (targets wrap_comments_break.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{read-lines:wrap_comments_break.ml.opts} %{dep:wrap_comments_break.ml}"))))

(alias
 (name runtest)
 (action (diff wrap_comments_break.ml wrap_comments_break.ml.output)))

(rule
 (targets wrapping_functor_args.ml.output)
 (deps .ocamlformat )
 (action
   (with-outputs-to %{targets}
     (system "%{bin:ocamlformat} %{dep:wrapping_functor_args.ml}"))))

(alias
 (name runtest)
 (action (diff wrapping_functor_args.ml wrapping_functor_args.ml.output)))
