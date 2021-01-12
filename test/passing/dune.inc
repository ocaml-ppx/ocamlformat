
(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to align_cases-break_all.ml.output
     (run %{bin:ocamlformat} --align-constructors-decl --align-variants-decl --align-cases --break-cases=all %{dep:align_cases.ml}))))

(rule
 (alias runtest)
 (action (diff align_cases-break_all.ml.ref align_cases-break_all.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to align_cases.ml.output
     (run %{bin:ocamlformat} --align-constructors-decl --align-variants-decl --align-cases %{dep:align_cases.ml}))))

(rule
 (alias runtest)
 (action (diff align_cases.ml align_cases.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to align_infix.ml.output
     (run %{bin:ocamlformat} --break-infix=fit-or-vertical %{dep:align_infix.ml}))))

(rule
 (alias runtest)
 (action (diff align_infix.ml align_infix.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to alignment.ml.output
     (run %{bin:ocamlformat} %{dep:alignment.ml}))))

(rule
 (alias runtest)
 (action (diff alignment.ml.ref alignment.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to apply.ml.output
     (run %{bin:ocamlformat} %{dep:apply.ml}))))

(rule
 (alias runtest)
 (action (diff apply.ml apply.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to args_grouped.ml.output
     (run %{bin:ocamlformat} --profile=ocamlformat --margin=100 %{dep:args_grouped.ml}))))

(rule
 (alias runtest)
 (action (diff args_grouped.ml args_grouped.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to array.ml.output
     (run %{bin:ocamlformat} %{dep:array.ml}))))

(rule
 (alias runtest)
 (action (diff array.ml array.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to assignment_operator-op_begin_line.ml.output
     (run %{bin:ocamlformat} --assignment-operator=begin-line %{dep:assignment_operator.ml}))))

(rule
 (alias runtest)
 (action (diff assignment_operator-op_begin_line.ml.ref assignment_operator-op_begin_line.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to assignment_operator.ml.output
     (run %{bin:ocamlformat} %{dep:assignment_operator.ml}))))

(rule
 (alias runtest)
 (action (diff assignment_operator.ml.ref assignment_operator.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to attribute_and_expression.ml.output
     (run %{bin:ocamlformat} %{dep:attribute_and_expression.ml}))))

(rule
 (alias runtest)
 (action (diff attribute_and_expression.ml attribute_and_expression.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to attributes.ml.output
     (run %{bin:ocamlformat} %{dep:attributes.ml}))))

(rule
 (alias runtest)
 (action (diff attributes.ml attributes.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to attributes.mli.output
     (run %{bin:ocamlformat} %{dep:attributes.mli}))))

(rule
 (alias runtest)
 (action (diff attributes.mli.ref attributes.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_before_in-auto.ml.output
     (run %{bin:ocamlformat} --break-before-in=auto %{dep:break_before_in.ml}))))

(rule
 (alias runtest)
 (action (diff break_before_in-auto.ml.ref break_before_in-auto.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_before_in.ml.output
     (run %{bin:ocamlformat} --break-before-in=fit-or-vertical %{dep:break_before_in.ml}))))

(rule
 (alias runtest)
 (action (diff break_before_in.ml break_before_in.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-align.ml.output
     (run %{bin:ocamlformat} --nested-match=align --break-cases=all %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-align.ml.ref break_cases-align.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-all.ml.output
     (run %{bin:ocamlformat} --break-cases=all %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-all.ml.ref break_cases-all.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-closing_on_separate_line.ml.output
     (run %{bin:ocamlformat} --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-closing_on_separate_line.ml.ref break_cases-closing_on_separate_line.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.output
     (run %{bin:ocamlformat} --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line --leading-nested-match-parens %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-closing_on_separate_line_leading_nested_match_parens.ml.ref break_cases-closing_on_separate_line_leading_nested_match_parens.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-cosl_lnmp_cmei.ml.output
     (run %{bin:ocamlformat} --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line --leading-nested-match-parens --cases-matching-exp-indent=normal %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-cosl_lnmp_cmei.ml.ref break_cases-cosl_lnmp_cmei.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-fit_or_vertical.ml.output
     (run %{bin:ocamlformat} --break-cases=fit-or-vertical %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-fit_or_vertical.ml.ref break_cases-fit_or_vertical.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-nested.ml.output
     (run %{bin:ocamlformat} --break-cases=nested %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-nested.ml.ref break_cases-nested.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-normal_indent.ml.output
     (run %{bin:ocamlformat} --cases-matching-exp-indent=normal --break-cases=all %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-normal_indent.ml.ref break_cases-normal_indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases-toplevel.ml.output
     (run %{bin:ocamlformat} --break-cases=toplevel --max-iter=4 %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases-toplevel.ml.ref break_cases-toplevel.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_cases.ml.output
     (run %{bin:ocamlformat} --break-cases=fit --max-iter=4 %{dep:break_cases.ml}))))

(rule
 (alias runtest)
 (action (diff break_cases.ml.ref break_cases.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_fun_decl-fit_or_vertical.ml.output
     (run %{bin:ocamlformat} --break-fun-decl=fit-or-vertical --break-fun-sig=fit-or-vertical %{dep:break_fun_decl.ml}))))

(rule
 (alias runtest)
 (action (diff break_fun_decl-fit_or_vertical.ml.ref break_fun_decl-fit_or_vertical.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_fun_decl-smart.ml.output
     (run %{bin:ocamlformat} --break-fun-decl=smart --break-fun-sig=smart %{dep:break_fun_decl.ml}))))

(rule
 (alias runtest)
 (action (diff break_fun_decl-smart.ml.ref break_fun_decl-smart.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_fun_decl-wrap.ml.output
     (run %{bin:ocamlformat} --break-fun-decl=wrap --break-fun-sig=wrap %{dep:break_fun_decl.ml}))))

(rule
 (alias runtest)
 (action (diff break_fun_decl-wrap.ml.ref break_fun_decl-wrap.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_fun_decl.ml.output
     (run %{bin:ocamlformat} %{dep:break_fun_decl.ml}))))

(rule
 (alias runtest)
 (action (diff break_fun_decl.ml break_fun_decl.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_record.ml.output
     (run %{bin:ocamlformat} --profile=janestreet --margin=58 %{dep:break_record.ml}))))

(rule
 (alias runtest)
 (action (diff break_record.ml break_record.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-after.ml.output
     (run %{bin:ocamlformat} --break-separators=after --max-iter=3 %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-after.ml.ref break_separators-after.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-after_docked.ml.output
     (run %{bin:ocamlformat} --break-separators=after --dock-collection-brackets --max-iter=3 %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-after_docked.ml.ref break_separators-after_docked.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-after_docked_transition_message.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --break-separators=after-and-docked %{dep:break_separators.ml})))))

(rule
 (alias runtest)
 (action (diff break_separators-after_docked_transition_message.ml.ref break_separators-after_docked_transition_message.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-after_docked_wrap.ml.output
     (run %{bin:ocamlformat} --break-separators=after --break-collection-expressions=wrap --dock-collection-brackets %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-after_docked_wrap.ml.ref break_separators-after_docked_wrap.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-after_wrap.ml.output
     (run %{bin:ocamlformat} --break-separators=after --break-collection-expressions=wrap %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-after_wrap.ml.ref break_separators-after_wrap.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-before_docked.ml.output
     (run %{bin:ocamlformat} --break-separators=before --dock-collection-brackets --max-iter=3 %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-before_docked.ml.ref break_separators-before_docked.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-before_docked_wrap.ml.output
     (run %{bin:ocamlformat} --break-separators=before --break-collection-expressions=wrap --dock-collection-brackets %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-before_docked_wrap.ml.ref break_separators-before_docked_wrap.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators-wrap.ml.output
     (run %{bin:ocamlformat} --break-separators=before --break-collection-expressions=wrap %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators-wrap.ml.ref break_separators-wrap.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_separators.ml.output
     (run %{bin:ocamlformat} --break-separators=before --max-iter=3 %{dep:break_separators.ml}))))

(rule
 (alias runtest)
 (action (diff break_separators.ml break_separators.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_sequence_before.ml.output
     (run %{bin:ocamlformat} %{dep:break_sequence_before.ml}))))

(rule
 (alias runtest)
 (action (diff break_sequence_before.ml break_sequence_before.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_string_literals-never.ml.output
     (run %{bin:ocamlformat} --break-string-literals=never %{dep:break_string_literals.ml}))))

(rule
 (alias runtest)
 (action (diff break_string_literals-never.ml.ref break_string_literals-never.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_string_literals-wrap.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --break-string-literals=wrap %{dep:break_string_literals.ml})))))

(rule
 (alias runtest)
 (action (diff break_string_literals-wrap.ml.ref break_string_literals-wrap.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_string_literals.ml.output
     (run %{bin:ocamlformat} --break-string-literals=auto %{dep:break_string_literals.ml}))))

(rule
 (alias runtest)
 (action (diff break_string_literals.ml.ref break_string_literals.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to break_struct.ml.output
     (run %{bin:ocamlformat} %{dep:break_struct.ml}))))

(rule
 (alias runtest)
 (action (diff break_struct.ml.ref break_struct.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to cinaps.ml.output
     (run %{bin:ocamlformat} %{dep:cinaps.ml}))))

(rule
 (alias runtest)
 (action (diff cinaps.ml cinaps.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to cmdline_override.ml.output
     (run %{bin:ocamlformat} --config=module-item-spacing=compact --module-item-spacing=sparse %{dep:cmdline_override.ml}))))

(rule
 (alias runtest)
 (action (diff cmdline_override.ml cmdline_override.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to cmdline_override2.ml.output
     (run %{bin:ocamlformat} --module-item-spacing=sparse --config=module-item-spacing=compact %{dep:cmdline_override2.ml}))))

(rule
 (alias runtest)
 (action (diff cmdline_override2.ml cmdline_override2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comment_breaking.ml.output
     (run %{bin:ocamlformat} %{dep:comment_breaking.ml}))))

(rule
 (alias runtest)
 (action (diff comment_breaking.ml comment_breaking.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comment_header.ml.output
     (run %{bin:ocamlformat} %{dep:comment_header.ml}))))

(rule
 (alias runtest)
 (action (diff comment_header.ml.ref comment_header.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comment_in_empty.ml.output
     (run %{bin:ocamlformat} %{dep:comment_in_empty.ml}))))

(rule
 (alias runtest)
 (action (diff comment_in_empty.ml comment_in_empty.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comment_in_modules.ml.output
     (run %{bin:ocamlformat} %{dep:comment_in_modules.ml}))))

(rule
 (alias runtest)
 (action (diff comment_in_modules.ml.ref comment_in_modules.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comment_last.ml.output
     (run %{bin:ocamlformat} %{dep:comment_last.ml}))))

(rule
 (alias runtest)
 (action (diff comment_last.ml comment_last.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comment_sparse.ml.output
     (run %{bin:ocamlformat} %{dep:comment_sparse.ml}))))

(rule
 (alias runtest)
 (action (diff comment_sparse.ml comment_sparse.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comments.ml.output
     (run %{bin:ocamlformat} --max-iter=4 %{dep:comments.ml}))))

(rule
 (alias runtest)
 (action (diff comments.ml.ref comments.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comments_args.ml.output
     (run %{bin:ocamlformat} --max-iter=4 %{dep:comments_args.ml}))))

(rule
 (alias runtest)
 (action (diff comments_args.ml.ref comments_args.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comments_around_disabled.ml.output
     (run %{bin:ocamlformat} %{dep:comments_around_disabled.ml}))))

(rule
 (alias runtest)
 (action (diff comments_around_disabled.ml.ref comments_around_disabled.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comments_in_record.ml.output
     (run %{bin:ocamlformat} %{dep:comments_in_record.ml}))))

(rule
 (alias runtest)
 (action (diff comments_in_record.ml.ref comments_in_record.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to compact_lists_arrays.ml.output
     (run %{bin:ocamlformat} %{dep:compact_lists_arrays.ml}))))

(rule
 (alias runtest)
 (action (diff compact_lists_arrays.ml compact_lists_arrays.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to custom_list.ml.output
     (run %{bin:ocamlformat} %{dep:custom_list.ml}))))

(rule
 (alias runtest)
 (action (diff custom_list.ml custom_list.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to directives.mlt.output
     (run %{bin:ocamlformat} %{dep:directives.mlt}))))

(rule
 (alias runtest)
 (action (diff directives.mlt.ref directives.mlt.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to disabled.ml.output
     (run %{bin:ocamlformat} --disable %{dep:disabled.ml}))))

(rule
 (alias runtest)
 (action (diff disabled.ml disabled.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to disambiguate.ml.output
     (run %{bin:ocamlformat} %{dep:disambiguate.ml}))))

(rule
 (alias runtest)
 (action (diff disambiguate.ml disambiguate.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to doc_comments-after.ml.output
     (run %{bin:ocamlformat} --doc-comments=after-when-possible %{dep:doc_comments.ml}))))

(rule
 (alias runtest)
 (action (diff doc_comments-after.ml.ref doc_comments-after.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to doc_comments-before-except-val.ml.output
     (run %{bin:ocamlformat} --doc-comments=before-except-val %{dep:doc_comments.ml}))))

(rule
 (alias runtest)
 (action (diff doc_comments-before-except-val.ml.ref doc_comments-before-except-val.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to doc_comments-before.ml.output
     (run %{bin:ocamlformat} --doc-comments=before %{dep:doc_comments.ml}))))

(rule
 (alias runtest)
 (action (diff doc_comments-before.ml.ref doc_comments-before.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to doc_comments.ml.output
     (run %{bin:ocamlformat} %{dep:doc_comments.ml}))))

(rule
 (alias runtest)
 (action (diff doc_comments.ml.ref doc_comments.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to doc_comments.mli.output
     (run %{bin:ocamlformat} %{dep:doc_comments.mli}))))

(rule
 (alias runtest)
 (action (diff doc_comments.mli.ref doc_comments.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to doc_comments_padding.ml.output
     (run %{bin:ocamlformat} %{dep:doc_comments_padding.ml}))))

(rule
 (alias runtest)
 (action (diff doc_comments_padding.ml doc_comments_padding.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to eliom_ext.eliom.output
     (run %{bin:ocamlformat} %{dep:eliom_ext.eliom}))))

(rule
 (alias runtest)
 (action (diff eliom_ext.eliom eliom_ext.eliom.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to empty.ml.output
     (run %{bin:ocamlformat} %{dep:empty.ml}))))

(rule
 (alias runtest)
 (action (diff empty.ml empty.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to empty_ml.ml.output
     (run %{bin:ocamlformat} %{dep:empty_ml.ml}))))

(rule
 (alias runtest)
 (action (diff empty_ml.ml empty_ml.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to empty_mli.mli.output
     (run %{bin:ocamlformat} %{dep:empty_mli.mli}))))

(rule
 (alias runtest)
 (action (diff empty_mli.mli empty_mli.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to empty_mlt.mlt.output
     (run %{bin:ocamlformat} %{dep:empty_mlt.mlt}))))

(rule
 (alias runtest)
 (action (diff empty_mlt.mlt empty_mlt.mlt.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to error1.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:error1.ml})))))

(rule
 (alias runtest)
 (action (diff error1.ml.ref error1.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to error2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:error2.ml})))))

(rule
 (alias runtest)
 (action (diff error2.ml.ref error2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to error3.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:error3.ml})))))

(rule
 (alias runtest)
 (action (diff error3.ml.ref error3.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to error4.ml.output
     (run %{bin:ocamlformat} --no-comment-check %{dep:error4.ml}))))

(rule
 (alias runtest)
 (action (diff error4.ml.ref error4.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to escaped_nl.ml.output
     (run %{bin:ocamlformat} %{dep:escaped_nl.ml}))))

(rule
 (alias runtest)
 (action (diff escaped_nl.ml.ref escaped_nl.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to exceptions.ml.output
     (run %{bin:ocamlformat} %{dep:exceptions.ml}))))

(rule
 (alias runtest)
 (action (diff exceptions.ml exceptions.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to exp_grouping-parens.ml.output
     (run %{bin:ocamlformat} --exp-grouping=parens %{dep:exp_grouping.ml}))))

(rule
 (alias runtest)
 (action (diff exp_grouping-parens.ml.ref exp_grouping-parens.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to exp_grouping.ml.output
     (run %{bin:ocamlformat} --exp-grouping=preserve %{dep:exp_grouping.ml}))))

(rule
 (alias runtest)
 (action (diff exp_grouping.ml.ref exp_grouping.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to exp_record.ml.output
     (run %{bin:ocamlformat} %{dep:exp_record.ml}))))

(rule
 (alias runtest)
 (action (diff exp_record.ml exp_record.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to expect_test.ml.output
     (run %{bin:ocamlformat} %{dep:expect_test.ml}))))

(rule
 (alias runtest)
 (action (diff expect_test.ml expect_test.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to extensions-indent.ml.output
     (run %{bin:ocamlformat} --max-iters=3 --extension-indent=5 --stritem-extension-indent=3 %{dep:extensions.ml}))))

(rule
 (alias runtest)
 (action (diff extensions-indent.ml.ref extensions-indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to extensions-indent.mli.output
     (run %{bin:ocamlformat} --extension-indent=5 --stritem-extension-indent=3 %{dep:extensions.mli}))))

(rule
 (alias runtest)
 (action (diff extensions-indent.mli.ref extensions-indent.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to extensions.ml.output
     (run %{bin:ocamlformat} --max-iters=3 %{dep:extensions.ml}))))

(rule
 (alias runtest)
 (action (diff extensions.ml.ref extensions.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to extensions.mli.output
     (run %{bin:ocamlformat} %{dep:extensions.mli}))))

(rule
 (alias runtest)
 (action (diff extensions.mli extensions.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to field-op_begin_line.ml.output
     (run %{bin:ocamlformat} --assignment-operator=begin-line %{dep:field.ml}))))

(rule
 (alias runtest)
 (action (diff field-op_begin_line.ml.ref field-op_begin_line.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to field.ml.output
     (run %{bin:ocamlformat} %{dep:field.ml}))))

(rule
 (alias runtest)
 (action (diff field.ml field.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to first_class_module.ml.output
     (run %{bin:ocamlformat} %{dep:first_class_module.ml}))))

(rule
 (alias runtest)
 (action (diff first_class_module.ml first_class_module.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to floating_doc.ml.output
     (run %{bin:ocamlformat} %{dep:floating_doc.ml}))))

(rule
 (alias runtest)
 (action (diff floating_doc.ml floating_doc.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to for_while.ml.output
     (run %{bin:ocamlformat} %{dep:for_while.ml}))))

(rule
 (alias runtest)
 (action (diff for_while.ml for_while.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to format_invalid_files.ml.output
     (run %{bin:ocamlformat} --format-invalid-files=auto %{dep:format_invalid_files.ml}))))

(rule
 (alias runtest)
 (action (diff format_invalid_files.ml.ref format_invalid_files.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to format_invalid_files_with_locations.ml.output
     (run %{bin:ocamlformat} --format-invalid-files=auto --max-iter 3 %{dep:format_invalid_files_with_locations.ml}))))

(rule
 (alias runtest)
 (action (diff format_invalid_files_with_locations.ml.ref format_invalid_files_with_locations.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to fun_decl.ml.output
     (run %{bin:ocamlformat} %{dep:fun_decl.ml}))))

(rule
 (alias runtest)
 (action (diff fun_decl.ml fun_decl.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to fun_function.ml.output
     (run %{bin:ocamlformat} %{dep:fun_function.ml}))))

(rule
 (alias runtest)
 (action (diff fun_function.ml fun_function.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to function_indent-never.ml.output
     (run %{bin:ocamlformat} --function-indent=4 --function-indent-nested=never %{dep:function_indent.ml}))))

(rule
 (alias runtest)
 (action (diff function_indent-never.ml.ref function_indent-never.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to function_indent.ml.output
     (run %{bin:ocamlformat} --function-indent=4 --function-indent-nested=always %{dep:function_indent.ml}))))

(rule
 (alias runtest)
 (action (diff function_indent.ml.ref function_indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to functor.ml.output
     (run %{bin:ocamlformat} %{dep:functor.ml}))))

(rule
 (alias runtest)
 (action (diff functor.ml functor.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to funsig.ml.output
     (run %{bin:ocamlformat} %{dep:funsig.ml}))))

(rule
 (alias runtest)
 (action (diff funsig.ml funsig.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to gadt.ml.output
     (run %{bin:ocamlformat} %{dep:gadt.ml}))))

(rule
 (alias runtest)
 (action (diff gadt.ml gadt.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to generative.ml.output
     (run %{bin:ocamlformat} %{dep:generative.ml}))))

(rule
 (alias runtest)
 (action (diff generative.ml generative.ml.output)))

(rule
 (deps .ocamlformat )
 (enabled_if (>= %{ocaml_version} 4.11.0))
 (action
   (with-outputs-to hash_types.ml.output
     (run %{bin:ocamlformat} %{dep:hash_types.ml}))))

(rule
 (alias runtest)
 (enabled_if (>= %{ocaml_version} 4.11.0))
 (action (diff hash_types.ml hash_types.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to index_op.ml.output
     (run %{bin:ocamlformat} %{dep:index_op.ml}))))

(rule
 (alias runtest)
 (action (diff index_op.ml index_op.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to indicate_multiline_delimiters-cosl.ml.output
     (run %{bin:ocamlformat} --indicate-multiline-delimiters=closing-on-separate-line %{dep:indicate_multiline_delimiters.ml}))))

(rule
 (alias runtest)
 (action (diff indicate_multiline_delimiters-cosl.ml.ref indicate_multiline_delimiters-cosl.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to indicate_multiline_delimiters-space.ml.output
     (run %{bin:ocamlformat} --indicate-multiline-delimiters=space %{dep:indicate_multiline_delimiters.ml}))))

(rule
 (alias runtest)
 (action (diff indicate_multiline_delimiters-space.ml.ref indicate_multiline_delimiters-space.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to indicate_multiline_delimiters.ml.output
     (run %{bin:ocamlformat} --indicate-multiline-delimiters=no %{dep:indicate_multiline_delimiters.ml}))))

(rule
 (alias runtest)
 (action (diff indicate_multiline_delimiters.ml indicate_multiline_delimiters.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to infix_arg_grouping.ml.output
     (run %{bin:ocamlformat} %{dep:infix_arg_grouping.ml}))))

(rule
 (alias runtest)
 (action (diff infix_arg_grouping.ml infix_arg_grouping.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to infix_bind-break.ml.output
     (run %{bin:ocamlformat} --break-infix=wrap --break-infix-before-func %{dep:infix_bind.ml}))))

(rule
 (alias runtest)
 (action (diff infix_bind-break.ml.ref infix_bind-break.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to infix_bind-fit_or_vertical-break.ml.output
     (run %{bin:ocamlformat} --break-infix=fit-or-vertical --break-infix-before-func %{dep:infix_bind.ml}))))

(rule
 (alias runtest)
 (action (diff infix_bind-fit_or_vertical-break.ml.ref infix_bind-fit_or_vertical-break.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to infix_bind-fit_or_vertical.ml.output
     (run %{bin:ocamlformat} --break-infix=fit-or-vertical --no-break-infix-before-func %{dep:infix_bind.ml}))))

(rule
 (alias runtest)
 (action (diff infix_bind-fit_or_vertical.ml.ref infix_bind-fit_or_vertical.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to infix_bind.ml.output
     (run %{bin:ocamlformat} --break-infix=wrap --no-break-infix-before-func %{dep:infix_bind.ml}))))

(rule
 (alias runtest)
 (action (diff infix_bind.ml infix_bind.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to infix_precedence.ml.output
     (run %{bin:ocamlformat} --infix-precedence=parens %{dep:infix_precedence.ml}))))

(rule
 (alias runtest)
 (action (diff infix_precedence.ml infix_precedence.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to invalid.ml.output
     (run %{bin:ocamlformat} %{dep:invalid.ml}))))

(rule
 (alias runtest)
 (action (diff invalid.ml invalid.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to invalid_docstring.ml.output
     (run %{bin:ocamlformat} %{dep:invalid_docstring.ml}))))

(rule
 (alias runtest)
 (action (diff invalid_docstring.ml.ref invalid_docstring.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue114.ml.output
     (run %{bin:ocamlformat} %{dep:issue114.ml}))))

(rule
 (alias runtest)
 (action (diff issue114.ml issue114.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue289.ml.output
     (run %{bin:ocamlformat} %{dep:issue289.ml}))))

(rule
 (alias runtest)
 (action (diff issue289.ml issue289.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue48.ml.output
     (run %{bin:ocamlformat} %{dep:issue48.ml}))))

(rule
 (alias runtest)
 (action (diff issue48.ml issue48.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue51.ml.output
     (run %{bin:ocamlformat} %{dep:issue51.ml}))))

(rule
 (alias runtest)
 (action (diff issue51.ml issue51.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue57.ml.output
     (run %{bin:ocamlformat} %{dep:issue57.ml}))))

(rule
 (alias runtest)
 (action (diff issue57.ml issue57.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue60.ml.output
     (run %{bin:ocamlformat} %{dep:issue60.ml}))))

(rule
 (alias runtest)
 (action (diff issue60.ml issue60.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue77.ml.output
     (run %{bin:ocamlformat} %{dep:issue77.ml}))))

(rule
 (alias runtest)
 (action (diff issue77.ml issue77.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue85.ml.output
     (run %{bin:ocamlformat} %{dep:issue85.ml}))))

(rule
 (alias runtest)
 (action (diff issue85.ml issue85.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to issue89.ml.output
     (run %{bin:ocamlformat} %{dep:issue89.ml}))))

(rule
 (alias runtest)
 (action (diff issue89.ml issue89.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-compact.ml.output
     (run %{bin:ocamlformat} --if-then-else=compact %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-compact.ml.ref ite-compact.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-compact_closing.ml.output
     (run %{bin:ocamlformat} --if-then-else=compact --indicate-multiline-delimiters=closing-on-separate-line %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-compact_closing.ml.ref ite-compact_closing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-fit_or_vertical.ml.output
     (run %{bin:ocamlformat} --if-then-else=fit-or-vertical %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-fit_or_vertical.ml.ref ite-fit_or_vertical.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-fit_or_vertical_closing.ml.output
     (run %{bin:ocamlformat} --if-then-else fit-or-vertical --indicate-multiline-delimiters=closing-on-separate-line %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-fit_or_vertical_closing.ml.ref ite-fit_or_vertical_closing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-fit_or_vertical_no_indicate.ml.output
     (run %{bin:ocamlformat} --if-then-else=fit-or-vertical --indicate-multiline-delimiters=no %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-fit_or_vertical_no_indicate.ml.ref ite-fit_or_vertical_no_indicate.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-kr.ml.output
     (run %{bin:ocamlformat} --if-then-else=k-r --max-iters=3 %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-kr.ml.ref ite-kr.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-kr_closing.ml.output
     (run %{bin:ocamlformat} --if-then-else=k-r --max-iters=3 --indicate-multiline-delimiters=closing-on-separate-line %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-kr_closing.ml.ref ite-kr_closing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-kw_first.ml.output
     (run %{bin:ocamlformat} --if-then-else=keyword-first %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-kw_first.ml.ref ite-kw_first.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-kw_first_closing.ml.output
     (run %{bin:ocamlformat} --if-then-else keyword-first --indicate-multiline-delimiters=closing-on-separate-line %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-kw_first_closing.ml.ref ite-kw_first_closing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-kw_first_no_indicate.ml.output
     (run %{bin:ocamlformat} --if-then-else=keyword-first --indicate-multiline-delimiters=no %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-kw_first_no_indicate.ml.ref ite-kw_first_no_indicate.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite-no_indicate.ml.output
     (run %{bin:ocamlformat} --if-then-else=compact --indicate-multiline-delimiters=no %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite-no_indicate.ml.ref ite-no_indicate.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ite.ml.output
     (run %{bin:ocamlformat} --if-then-else=compact %{dep:ite.ml}))))

(rule
 (alias runtest)
 (action (diff ite.ml.ref ite.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_args.ml.output
     (run %{bin:ocamlformat} --max-iter=3 %{dep:js_args.ml}))))

(rule
 (alias runtest)
 (action (diff js_args.ml.ref js_args.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_begin.ml.output
     (run %{bin:ocamlformat} %{dep:js_begin.ml}))))

(rule
 (alias runtest)
 (action (diff js_begin.ml.ref js_begin.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_fun.ml.output
     (run %{bin:ocamlformat} --max-iter=3 %{dep:js_fun.ml}))))

(rule
 (alias runtest)
 (action (diff js_fun.ml.ref js_fun.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_pattern.ml.output
     (run %{bin:ocamlformat} %{dep:js_pattern.ml}))))

(rule
 (alias runtest)
 (action (diff js_pattern.ml.ref js_pattern.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_record.ml.output
     (run %{bin:ocamlformat} %{dep:js_record.ml}))))

(rule
 (alias runtest)
 (action (diff js_record.ml.ref js_record.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_sig.mli.output
     (run %{bin:ocamlformat} --profile=janestreet %{dep:js_sig.mli}))))

(rule
 (alias runtest)
 (action (diff js_sig.mli.ref js_sig.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_source.ml.output
     (run %{bin:ocamlformat} --max-iters=3 --profile=janestreet %{dep:js_source.ml}))))

(rule
 (alias runtest)
 (action (diff js_source.ml.ref js_source.ml.output)))

(rule
 (deps .ocp-indent )
 (action
   (with-outputs-to js_source.ml.ocp.output
     (run %{bin:ocp-indent} %{dep:js_source.ml.ref}))))

(rule
 (alias runtest)
 (action (diff js_source.ml.ocp js_source.ml.ocp.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_syntax.ml.output
     (run %{bin:ocamlformat} %{dep:js_syntax.ml}))))

(rule
 (alias runtest)
 (action (diff js_syntax.ml.ref js_syntax.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_to_do.ml.output
     (run %{bin:ocamlformat} %{dep:js_to_do.ml}))))

(rule
 (alias runtest)
 (action (diff js_to_do.ml.ref js_to_do.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_upon.ml.output
     (run %{bin:ocamlformat} %{dep:js_upon.ml}))))

(rule
 (alias runtest)
 (action (diff js_upon.ml.ref js_upon.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to kw_extentions.ml.output
     (run %{bin:ocamlformat} %{dep:kw_extentions.ml}))))

(rule
 (alias runtest)
 (action (diff kw_extentions.ml kw_extentions.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to label_option_default_args.ml.output
     (run %{bin:ocamlformat} --max-iters=4 %{dep:label_option_default_args.ml}))))

(rule
 (alias runtest)
 (action (diff label_option_default_args.ml.ref label_option_default_args.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to lazy.ml.output
     (run %{bin:ocamlformat} %{dep:lazy.ml}))))

(rule
 (alias runtest)
 (action (diff lazy.ml lazy.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_binding-in_indent.ml.output
     (run %{bin:ocamlformat} --indent-after-in=4 %{dep:let_binding.ml}))))

(rule
 (alias runtest)
 (action (diff let_binding-in_indent.ml.ref let_binding-in_indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_binding-indent.ml.output
     (run %{bin:ocamlformat} --let-binding-indent=6 %{dep:let_binding.ml}))))

(rule
 (alias runtest)
 (action (diff let_binding-indent.ml.ref let_binding-indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_binding.ml.output
     (run %{bin:ocamlformat} %{dep:let_binding.ml}))))

(rule
 (alias runtest)
 (action (diff let_binding.ml.ref let_binding.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_binding_spacing-double-semicolon.ml.output
     (run %{bin:ocamlformat} --let-binding-spacing=double-semicolon %{dep:let_binding_spacing.ml}))))

(rule
 (alias runtest)
 (action (diff let_binding_spacing-double-semicolon.ml.ref let_binding_spacing-double-semicolon.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_binding_spacing-sparse.ml.output
     (run %{bin:ocamlformat} --let-binding-spacing=sparse %{dep:let_binding_spacing.ml}))))

(rule
 (alias runtest)
 (action (diff let_binding_spacing-sparse.ml.ref let_binding_spacing-sparse.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_binding_spacing.ml.output
     (run %{bin:ocamlformat} --let-binding-spacing=compact %{dep:let_binding_spacing.ml}))))

(rule
 (alias runtest)
 (action (diff let_binding_spacing.ml let_binding_spacing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_in_constr.ml.output
     (run %{bin:ocamlformat} %{dep:let_in_constr.ml}))))

(rule
 (alias runtest)
 (action (diff let_in_constr.ml let_in_constr.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_module-sparse.ml.output
     (run %{bin:ocamlformat} --let-module=sparse %{dep:let_module.ml}))))

(rule
 (alias runtest)
 (action (diff let_module-sparse.ml.ref let_module-sparse.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to let_module.ml.output
     (run %{bin:ocamlformat} --let-module=compact %{dep:let_module.ml}))))

(rule
 (alias runtest)
 (action (diff let_module.ml.ref let_module.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to list-space_around.ml.output
     (run %{bin:ocamlformat} --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:list.ml}))))

(rule
 (alias runtest)
 (action (diff list-space_around.ml.ref list-space_around.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to list.ml.output
     (run %{bin:ocamlformat} %{dep:list.ml}))))

(rule
 (alias runtest)
 (action (diff list.ml list.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to list_and_comments.ml.output
     (run %{bin:ocamlformat} %{dep:list_and_comments.ml}))))

(rule
 (alias runtest)
 (action (diff list_and_comments.ml.ref list_and_comments.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to loc_stack.ml.output
     (run %{bin:ocamlformat} -n 3 %{dep:loc_stack.ml}))))

(rule
 (alias runtest)
 (action (diff loc_stack.ml.ref loc_stack.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to locally_abtract_types.ml.output
     (run %{bin:ocamlformat} %{dep:locally_abtract_types.ml}))))

(rule
 (alias runtest)
 (action (diff locally_abtract_types.ml locally_abtract_types.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to margin_80.ml.output
     (run %{bin:ocamlformat} --margin=80 %{dep:margin_80.ml}))))

(rule
 (alias runtest)
 (action (diff margin_80.ml.ref margin_80.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to match.ml.output
     (run %{bin:ocamlformat} %{dep:match.ml}))))

(rule
 (alias runtest)
 (action (diff match.ml match.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to match2.ml.output
     (run %{bin:ocamlformat} --leading-nested-match-parens %{dep:match2.ml}))))

(rule
 (alias runtest)
 (action (diff match2.ml match2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to match_indent-never.ml.output
     (run %{bin:ocamlformat} --match-indent=4 --match-indent-nested=never %{dep:match_indent.ml}))))

(rule
 (alias runtest)
 (action (diff match_indent-never.ml.ref match_indent-never.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to match_indent.ml.output
     (run %{bin:ocamlformat} --match-indent=4 --match-indent-nested=always %{dep:match_indent.ml}))))

(rule
 (alias runtest)
 (action (diff match_indent.ml.ref match_indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to max_indent.ml.output
     (run %{bin:ocamlformat} --max-indent=2 %{dep:max_indent.ml}))))

(rule
 (alias runtest)
 (action (diff max_indent.ml max_indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module.ml.output
     (run %{bin:ocamlformat} %{dep:module.ml}))))

(rule
 (alias runtest)
 (action (diff module.ml module.ml.output)))

(rule
 (deps .ocamlformat )
 (enabled_if (>= %{ocaml_version} 4.10.0))
 (action
   (with-outputs-to module_anonymous.ml.output
     (run %{bin:ocamlformat} %{dep:module_anonymous.ml}))))

(rule
 (alias runtest)
 (enabled_if (>= %{ocaml_version} 4.10.0))
 (action (diff module_anonymous.ml module_anonymous.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module_attributes.ml.output
     (run %{bin:ocamlformat} %{dep:module_attributes.ml}))))

(rule
 (alias runtest)
 (action (diff module_attributes.ml.ref module_attributes.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module_item_spacing-preserve.ml.output
     (run %{bin:ocamlformat} --max-iter=3 --module-item-spacing=preserve %{dep:module_item_spacing.ml}))))

(rule
 (alias runtest)
 (action (diff module_item_spacing-preserve.ml.ref module_item_spacing-preserve.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module_item_spacing-sparse.ml.output
     (run %{bin:ocamlformat} --max-iter=3 --module-item-spacing=sparse %{dep:module_item_spacing.ml}))))

(rule
 (alias runtest)
 (action (diff module_item_spacing-sparse.ml.ref module_item_spacing-sparse.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module_item_spacing.ml.output
     (run %{bin:ocamlformat} --max-iter=3 --module-item-spacing=compact %{dep:module_item_spacing.ml}))))

(rule
 (alias runtest)
 (action (diff module_item_spacing.ml.ref module_item_spacing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module_item_spacing.mli.output
     (run %{bin:ocamlformat} --max-iter=3 %{dep:module_item_spacing.mli}))))

(rule
 (alias runtest)
 (action (diff module_item_spacing.mli.ref module_item_spacing.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module_type.ml.output
     (run %{bin:ocamlformat} %{dep:module_type.ml}))))

(rule
 (alias runtest)
 (action (diff module_type.ml module_type.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to monadic_binding.ml.output
     (run %{bin:ocamlformat} %{dep:monadic_binding.ml}))))

(rule
 (alias runtest)
 (action (diff monadic_binding.ml monadic_binding.ml.output)))

(rule
 (deps .ocamlformat )
 (enabled_if (>= %{ocaml_version} 4.10.0))
 (action
   (with-outputs-to multi_index_op.ml.output
     (run %{bin:ocamlformat} %{dep:multi_index_op.ml}))))

(rule
 (alias runtest)
 (enabled_if (>= %{ocaml_version} 4.10.0))
 (action (diff multi_index_op.ml multi_index_op.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to need_format.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --max-iters=1 %{dep:need_format.ml})))))

(rule
 (alias runtest)
 (action (diff need_format.ml.ref need_format.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to new.ml.output
     (run %{bin:ocamlformat} %{dep:new.ml}))))

(rule
 (alias runtest)
 (action (diff new.ml new.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to object.ml.output
     (run %{bin:ocamlformat} %{dep:object.ml}))))

(rule
 (alias runtest)
 (action (diff object.ml object.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to object_type.ml.output
     (run %{bin:ocamlformat} %{dep:object_type.ml}))))

(rule
 (alias runtest)
 (action (diff object_type.ml.ref object_type.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ocp_indent_compat.ml.output
     (run %{bin:ocamlformat} %{dep:ocp_indent_compat.ml}))))

(rule
 (alias runtest)
 (action (diff ocp_indent_compat.ml ocp_indent_compat.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ocp_indent_options.ml.output
     (run %{bin:ocamlformat} --ocp-indent-config %{dep:ocp_indent_options.ml}))))

(rule
 (alias runtest)
 (action (diff ocp_indent_options.ml.ref ocp_indent_options.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to open.ml.output
     (run %{bin:ocamlformat} %{dep:open.ml}))))

(rule
 (alias runtest)
 (action (diff open.ml.ref open.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to open_types.ml.output
     (run %{bin:ocamlformat} %{dep:open_types.ml}))))

(rule
 (alias runtest)
 (action (diff open_types.ml open_types.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to option.ml.output
     (run %{bin:ocamlformat} %{dep:option.ml}))))

(rule
 (alias runtest)
 (action (diff option.ml.ref option.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to override.ml.output
     (run %{bin:ocamlformat} %{dep:override.ml}))))

(rule
 (alias runtest)
 (action (diff override.ml.ref override.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to parens_tuple_patterns.ml.output
     (run %{bin:ocamlformat} %{dep:parens_tuple_patterns.ml}))))

(rule
 (alias runtest)
 (action (diff parens_tuple_patterns.ml parens_tuple_patterns.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to partial.ml.output
     (run %{bin:ocamlformat} --format-invalid-files=auto %{dep:partial.ml}))))

(rule
 (alias runtest)
 (action (diff partial.ml.ref partial.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to partial_double_quotes.ml.output
     (run %{bin:ocamlformat} --format-invalid-files=auto %{dep:partial_double_quotes.ml}))))

(rule
 (alias runtest)
 (action (diff partial_double_quotes.ml.ref partial_double_quotes.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to pre_post_extensions.ml.output
     (run %{bin:ocamlformat} %{dep:pre_post_extensions.ml}))))

(rule
 (alias runtest)
 (action (diff pre_post_extensions.ml pre_post_extensions.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to precedence.ml.output
     (run %{bin:ocamlformat} %{dep:precedence.ml}))))

(rule
 (alias runtest)
 (action (diff precedence.ml precedence.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to prefix_infix.ml.output
     (run %{bin:ocamlformat} %{dep:prefix_infix.ml}))))

(rule
 (alias runtest)
 (action (diff prefix_infix.ml prefix_infix.ml.output)))

(rule
 (deps .ocamlformat dir1/dir2/.ocamlformat dir1/dir2/print_config.ml)
 (action
   (with-outputs-to print_config.ml.output
     (run %{bin:ocamlformat} dir1/dir2/print_config.ml --print-config --config=max-iters=2 %{dep:print_config.ml}))))

(rule
 (alias runtest)
 (action (diff print_config.ml.ref print_config.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to profiles.ml.output
     (run %{bin:ocamlformat} --config=margin=20 --profile=janestreet --module-item-spacing=sparse %{dep:profiles.ml}))))

(rule
 (alias runtest)
 (action (diff profiles.ml profiles.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to profiles2.ml.output
     (run %{bin:ocamlformat} --profile=janestreet %{dep:profiles2.ml}))))

(rule
 (alias runtest)
 (action (diff profiles2.ml profiles2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to protected_object_types.ml.output
     (run %{bin:ocamlformat} %{dep:protected_object_types.ml}))))

(rule
 (alias runtest)
 (action (diff protected_object_types.ml protected_object_types.ml.output)))

(rule
 (deps .ocamlformat )
 (enabled_if (>= %{ocaml_version} 4.11.0))
 (action
   (with-outputs-to quoted_strings.ml.output
     (run %{bin:ocamlformat} %{dep:quoted_strings.ml}))))

(rule
 (alias runtest)
 (enabled_if (>= %{ocaml_version} 4.11.0))
 (action (diff quoted_strings.ml quoted_strings.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to recmod.mli.output
     (run %{bin:ocamlformat} %{dep:recmod.mli}))))

(rule
 (alias runtest)
 (action (diff recmod.mli recmod.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to record-loose.ml.output
     (run %{bin:ocamlformat} --field-space=loose %{dep:record.ml}))))

(rule
 (alias runtest)
 (action (diff record-loose.ml.ref record-loose.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to record-tight_decl.ml.output
     (run %{bin:ocamlformat} --field-space=tight-decl %{dep:record.ml}))))

(rule
 (alias runtest)
 (action (diff record-tight_decl.ml.ref record-tight_decl.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to record.ml.output
     (run %{bin:ocamlformat} --field-space=tight %{dep:record.ml}))))

(rule
 (alias runtest)
 (action (diff record.ml.ref record.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to record_punning.ml.output
     (run %{bin:ocamlformat} %{dep:record_punning.ml}))))

(rule
 (alias runtest)
 (action (diff record_punning.ml record_punning.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to reformat_string.ml.output
     (run %{bin:ocamlformat} --max-iter=2 %{dep:reformat_string.ml}))))

(rule
 (alias runtest)
 (action (diff reformat_string.ml.ref reformat_string.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to refs.ml.output
     (run %{bin:ocamlformat} %{dep:refs.ml}))))

(rule
 (alias runtest)
 (action (diff refs.ml refs.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to remove_extra_parens.ml.output
     (run %{bin:ocamlformat} %{dep:remove_extra_parens.ml}))))

(rule
 (alias runtest)
 (action (diff remove_extra_parens.ml.ref remove_extra_parens.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to revapply_ext.ml.output
     (run %{bin:ocamlformat} %{dep:revapply_ext.ml}))))

(rule
 (alias runtest)
 (action (diff revapply_ext.ml revapply_ext.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to send.ml.output
     (run %{bin:ocamlformat} %{dep:send.ml}))))

(rule
 (alias runtest)
 (action (diff send.ml send.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to sequence-preserve.ml.output
     (run %{bin:ocamlformat} --sequence-blank-line=preserve-one --max-iter=3 %{dep:sequence.ml}))))

(rule
 (alias runtest)
 (action (diff sequence-preserve.ml.ref sequence-preserve.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to sequence.ml.output
     (run %{bin:ocamlformat} --sequence-blank-line=compact --max-iter=3 %{dep:sequence.ml}))))

(rule
 (alias runtest)
 (action (diff sequence.ml.ref sequence.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to shebang.ml.output
     (run %{bin:ocamlformat} %{dep:shebang.ml}))))

(rule
 (alias runtest)
 (action (diff shebang.ml shebang.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to shortcut_ext_attr.ml.output
     (run %{bin:ocamlformat} %{dep:shortcut_ext_attr.ml}))))

(rule
 (alias runtest)
 (action (diff shortcut_ext_attr.ml shortcut_ext_attr.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to sig_value.mli.output
     (run %{bin:ocamlformat} %{dep:sig_value.mli}))))

(rule
 (alias runtest)
 (action (diff sig_value.mli.ref sig_value.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to single_line.mli.output
     (run %{bin:ocamlformat} %{dep:single_line.mli}))))

(rule
 (alias runtest)
 (action (diff single_line.mli single_line.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to skip.ml.output
     (run %{bin:ocamlformat} %{dep:skip.ml}))))

(rule
 (alias runtest)
 (action (diff skip.ml skip.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to source.ml.output
     (run %{bin:ocamlformat} --max-iters=3 %{dep:source.ml}))))

(rule
 (alias runtest)
 (action (diff source.ml.ref source.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to str_value.ml.output
     (run %{bin:ocamlformat} %{dep:str_value.ml}))))

(rule
 (alias runtest)
 (action (diff str_value.ml str_value.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to string.ml.output
     (run %{bin:ocamlformat} %{dep:string.ml}))))

(rule
 (alias runtest)
 (action (diff string.ml.ref string.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to string_array.ml.output
     (run %{bin:ocamlformat} %{dep:string_array.ml}))))

(rule
 (alias runtest)
 (action (diff string_array.ml string_array.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to string_wrapping.ml.output
     (run %{bin:ocamlformat} %{dep:string_wrapping.ml}))))

(rule
 (alias runtest)
 (action (diff string_wrapping.ml string_wrapping.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to symbol.ml.output
     (run %{bin:ocamlformat} %{dep:symbol.ml}))))

(rule
 (alias runtest)
 (action (diff symbol.ml symbol.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to tag_only.ml.output
     (run %{bin:ocamlformat} %{dep:tag_only.ml}))))

(rule
 (alias runtest)
 (action (diff tag_only.ml tag_only.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to tag_only.mli.output
     (run %{bin:ocamlformat} %{dep:tag_only.mli}))))

(rule
 (alias runtest)
 (action (diff tag_only.mli tag_only.mli.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to try_with_or_pattern.ml.output
     (run %{bin:ocamlformat} %{dep:try_with_or_pattern.ml}))))

(rule
 (alias runtest)
 (action (diff try_with_or_pattern.ml try_with_or_pattern.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to tuple.ml.output
     (run %{bin:ocamlformat} --parens-tuple=always %{dep:tuple.ml}))))

(rule
 (alias runtest)
 (action (diff tuple.ml tuple.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to tuple_less_parens.ml.output
     (run %{bin:ocamlformat} --parens-tuple=multi-line-only %{dep:tuple_less_parens.ml}))))

(rule
 (alias runtest)
 (action (diff tuple_less_parens.ml tuple_less_parens.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to tuple_type_parens.ml.output
     (run %{bin:ocamlformat} %{dep:tuple_type_parens.ml}))))

(rule
 (alias runtest)
 (action (diff tuple_type_parens.ml tuple_type_parens.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to type_and_constraint.ml.output
     (run %{bin:ocamlformat} %{dep:type_and_constraint.ml}))))

(rule
 (alias runtest)
 (action (diff type_and_constraint.ml type_and_constraint.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to type_annotations.ml.output
     (run %{bin:ocamlformat} %{dep:type_annotations.ml}))))

(rule
 (alias runtest)
 (action (diff type_annotations.ml type_annotations.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types-compact-space_around-docked.ml.output
     (run %{bin:ocamlformat} --type-decl=compact --space-around-arrays --space-around-lists --space-around-records --space-around-variants --break-separators=after --dock-collection-brackets %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types-compact-space_around-docked.ml.ref types-compact-space_around-docked.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types-compact-space_around.ml.output
     (run %{bin:ocamlformat} --type-decl=compact --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types-compact-space_around.ml.ref types-compact-space_around.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types-compact.ml.output
     (run %{bin:ocamlformat} --type-decl=compact %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types-compact.ml.ref types-compact.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types-indent.ml.output
     (run %{bin:ocamlformat} --type-decl-indent=6 %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types-indent.ml.ref types-indent.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types-sparse-space_around.ml.output
     (run %{bin:ocamlformat} --type-decl=sparse --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types-sparse-space_around.ml.ref types-sparse-space_around.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types-sparse.ml.output
     (run %{bin:ocamlformat} --type-decl=sparse %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types-sparse.ml.ref types-sparse.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to types.ml.output
     (run %{bin:ocamlformat} %{dep:types.ml}))))

(rule
 (alias runtest)
 (action (diff types.ml types.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unary.ml.output
     (run %{bin:ocamlformat} %{dep:unary.ml}))))

(rule
 (alias runtest)
 (action (diff unary.ml.ref unary.ml.output)))

(rule
 (deps .ocamlformat )
 (enabled_if (>= %{ocaml_version} 4.12.0))
 (action
   (with-outputs-to unary_hash.ml.output
     (run %{bin:ocamlformat} %{dep:unary_hash.ml}))))

(rule
 (alias runtest)
 (enabled_if (>= %{ocaml_version} 4.12.0))
 (action (diff unary_hash.ml unary_hash.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unicode.ml.output
     (run %{bin:ocamlformat} --margin=80 --wrap-comments %{dep:unicode.ml}))))

(rule
 (alias runtest)
 (action (diff unicode.ml.ref unicode.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to use_file.mlt.output
     (run %{bin:ocamlformat} %{dep:use_file.mlt}))))

(rule
 (alias runtest)
 (action (diff use_file.mlt use_file.mlt.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to verbose1.ml.output
     (run %{bin:ocamlformat} --print-config --doc-comments=before --config=doc-comments=before %{dep:verbose1.ml}))))

(rule
 (alias runtest)
 (action (diff verbose1.ml.ref verbose1.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to verbose2.ml.output
     (run %{bin:ocamlformat} --print-config --doc-comments=before --config=doc-comments=before %{dep:verbose2.ml}))))

(rule
 (alias runtest)
 (action (diff verbose2.ml.ref verbose2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to wrap_comments.ml.output
     (run %{bin:ocamlformat} --profile=ocamlformat %{dep:wrap_comments.ml}))))

(rule
 (alias runtest)
 (action (diff wrap_comments.ml.ref wrap_comments.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to wrap_comments_break.ml.output
     (run %{bin:ocamlformat} --no-wrap-fun-args --margin=67 %{dep:wrap_comments_break.ml}))))

(rule
 (alias runtest)
 (action (diff wrap_comments_break.ml wrap_comments_break.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to wrapping_functor_args.ml.output
     (run %{bin:ocamlformat} %{dep:wrapping_functor_args.ml}))))

(rule
 (alias runtest)
 (action (diff wrapping_functor_args.ml wrapping_functor_args.ml.output)))
