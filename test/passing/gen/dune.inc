
(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to align_infix.ml.stdout
   (with-stderr-to align_infix.ml.stderr
     (run %{bin:ocamlformat} --name align_infix.ml --margin-check --break-infix=fit-or-vertical %{dep:../tests/align_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff align_infix.ml.ref align_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff align_infix.ml.err align_infix.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to alignment.ml.stdout
   (with-stderr-to alignment.ml.stderr
     (run %{bin:ocamlformat} --name alignment.ml --margin-check %{dep:../tests/alignment.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff alignment.ml.ref alignment.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff alignment.ml.err alignment.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to apply.ml.stdout
   (with-stderr-to apply.ml.stderr
     (run %{bin:ocamlformat} --name apply.ml --margin-check %{dep:../tests/apply.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff apply.ml.ref apply.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff apply.ml.err apply.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to apply_functor.ml.stdout
   (with-stderr-to apply_functor.ml.stderr
     (run %{bin:ocamlformat} --name apply_functor.ml --margin-check %{dep:../tests/apply_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff apply_functor.ml.ref apply_functor.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff apply_functor.ml.err apply_functor.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to args_grouped.ml.stdout
   (with-stderr-to args_grouped.ml.stderr
     (run %{bin:ocamlformat} --name args_grouped.ml --margin-check --margin=100 %{dep:../tests/args_grouped.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff args_grouped.ml.ref args_grouped.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff args_grouped.ml.err args_grouped.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to array.ml.stdout
   (with-stderr-to array.ml.stderr
     (run %{bin:ocamlformat} --name array.ml --margin-check %{dep:../tests/array.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff array.ml.ref array.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff array.ml.err array.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to assignment_operator-op_begin_line.ml.stdout
   (with-stderr-to assignment_operator-op_begin_line.ml.stderr
     (run %{bin:ocamlformat} --name assignment_operator-op_begin_line.ml --margin-check --assignment-operator=begin-line %{dep:../tests/assignment_operator.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff assignment_operator-op_begin_line.ml.ref assignment_operator-op_begin_line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff assignment_operator-op_begin_line.ml.err assignment_operator-op_begin_line.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to assignment_operator.ml.stdout
   (with-stderr-to assignment_operator.ml.stderr
     (run %{bin:ocamlformat} --name assignment_operator.ml --margin-check %{dep:../tests/assignment_operator.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff assignment_operator.ml.ref assignment_operator.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff assignment_operator.ml.err assignment_operator.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to attribute_and_expression.ml.stdout
   (with-stderr-to attribute_and_expression.ml.stderr
     (run %{bin:ocamlformat} --name attribute_and_expression.ml --margin-check %{dep:../tests/attribute_and_expression.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff attribute_and_expression.ml.ref attribute_and_expression.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff attribute_and_expression.ml.err attribute_and_expression.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to attributes.ml.stdout
   (with-stderr-to attributes.ml.stderr
     (run %{bin:ocamlformat} --name attributes.ml --margin-check %{dep:../tests/attributes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff attributes.ml.ref attributes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff attributes.ml.err attributes.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to attributes.mli.stdout
   (with-stderr-to attributes.mli.stderr
     (run %{bin:ocamlformat} --name attributes.mli --margin-check %{dep:../tests/attributes.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff attributes.mli.ref attributes.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff attributes.mli.err attributes.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to binders.ml.stdout
   (with-stderr-to binders.ml.stderr
     (run %{bin:ocamlformat} --name binders.ml --margin-check %{dep:../tests/binders.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff binders.ml.ref binders.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff binders.ml.err binders.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_before_in-auto.ml.stdout
   (with-stderr-to break_before_in-auto.ml.stderr
     (run %{bin:ocamlformat} --name break_before_in-auto.ml --margin-check --break-before-in=auto %{dep:../tests/break_before_in.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_before_in-auto.ml.ref break_before_in-auto.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_before_in-auto.ml.err break_before_in-auto.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_before_in.ml.stdout
   (with-stderr-to break_before_in.ml.stderr
     (run %{bin:ocamlformat} --name break_before_in.ml --margin-check --break-before-in=fit-or-vertical %{dep:../tests/break_before_in.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_before_in.ml.ref break_before_in.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_before_in.ml.err break_before_in.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-align.ml.stdout
   (with-stderr-to break_cases-align.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-align.ml --margin-check --nested-match=align --break-cases=all %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-align.ml.ref break_cases-align.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-align.ml.err break_cases-align.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-all.ml.stdout
   (with-stderr-to break_cases-all.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-all.ml --margin-check --break-cases=all %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-all.ml.ref break_cases-all.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-all.ml.err break_cases-all.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line.ml.stdout
   (with-stderr-to break_cases-closing_on_separate_line.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-closing_on_separate_line.ml --margin-check --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-closing_on_separate_line.ml.ref break_cases-closing_on_separate_line.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-closing_on_separate_line.ml.err break_cases-closing_on_separate_line.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line_fit_or_vertical.ml.stdout
   (with-stderr-to break_cases-closing_on_separate_line_fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-closing_on_separate_line_fit_or_vertical.ml --margin-check --break-cases=fit-or-vertical --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_cases-closing_on_separate_line_fit_or_vertical.ml.ref break_cases-closing_on_separate_line_fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_cases-closing_on_separate_line_fit_or_vertical.ml.err break_cases-closing_on_separate_line_fit_or_vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stdout
   (with-stderr-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-closing_on_separate_line_leading_nested_match_parens.ml --margin-check --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line --leading-nested-match-parens %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-closing_on_separate_line_leading_nested_match_parens.ml.ref break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-closing_on_separate_line_leading_nested_match_parens.ml.err break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-cosl_lnmp_cmei.ml.stdout
   (with-stderr-to break_cases-cosl_lnmp_cmei.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-cosl_lnmp_cmei.ml --margin-check --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line --leading-nested-match-parens --cases-matching-exp-indent=normal %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-cosl_lnmp_cmei.ml.ref break_cases-cosl_lnmp_cmei.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-cosl_lnmp_cmei.ml.err break_cases-cosl_lnmp_cmei.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-fit_or_vertical.ml.stdout
   (with-stderr-to break_cases-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-fit_or_vertical.ml --margin-check --break-cases=fit-or-vertical %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-fit_or_vertical.ml.ref break_cases-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-fit_or_vertical.ml.err break_cases-fit_or_vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-nested.ml.stdout
   (with-stderr-to break_cases-nested.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-nested.ml --margin-check --break-cases=nested %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-nested.ml.ref break_cases-nested.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-nested.ml.err break_cases-nested.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-normal_indent.ml.stdout
   (with-stderr-to break_cases-normal_indent.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-normal_indent.ml --margin-check --cases-matching-exp-indent=normal --break-cases=all %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-normal_indent.ml.ref break_cases-normal_indent.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-normal_indent.ml.err break_cases-normal_indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-toplevel.ml.stdout
   (with-stderr-to break_cases-toplevel.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-toplevel.ml --margin-check --break-cases=toplevel --max-iter=4 %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_cases-toplevel.ml.ref break_cases-toplevel.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_cases-toplevel.ml.err break_cases-toplevel.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-vertical.ml.stdout
   (with-stderr-to break_cases-vertical.ml.stderr
     (run %{bin:ocamlformat} --name break_cases-vertical.ml --margin-check --break-cases=vertical %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-vertical.ml.ref break_cases-vertical.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff break_cases-vertical.ml.err break_cases-vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_cases.ml.stdout
   (with-stderr-to break_cases.ml.stderr
     (run %{bin:ocamlformat} --name break_cases.ml --margin-check --break-cases=fit --max-iter=4 %{dep:../tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_cases.ml.ref break_cases.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_cases.ml.err break_cases.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_collection_expressions-wrap.ml.stdout
   (with-stderr-to break_collection_expressions-wrap.ml.stderr
     (run %{bin:ocamlformat} --name break_collection_expressions-wrap.ml --margin-check --break-collection-expressions=wrap --max-iters=3 %{dep:../tests/break_collection_expressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_collection_expressions-wrap.ml.ref break_collection_expressions-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_collection_expressions-wrap.ml.err break_collection_expressions-wrap.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_collection_expressions.ml.stdout
   (with-stderr-to break_collection_expressions.ml.stderr
     (run %{bin:ocamlformat} --name break_collection_expressions.ml --margin-check --break-collection-expressions=fit-or-vertical --max-iters=3 %{dep:../tests/break_collection_expressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_collection_expressions.ml.ref break_collection_expressions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_collection_expressions.ml.err break_collection_expressions.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_colon-before.ml.stdout
   (with-stderr-to break_colon-before.ml.stderr
     (run %{bin:ocamlformat} --name break_colon-before.ml --margin-check --break-colon=before %{dep:../tests/break_colon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_colon-before.ml.ref break_colon-before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_colon-before.ml.err break_colon-before.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_colon.ml.stdout
   (with-stderr-to break_colon.ml.stderr
     (run %{bin:ocamlformat} --name break_colon.ml --margin-check --break-colon=after %{dep:../tests/break_colon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_colon.ml.ref break_colon.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_colon.ml.err break_colon.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-fit_or_vertical.ml.stdout
   (with-stderr-to break_fun_decl-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --name break_fun_decl-fit_or_vertical.ml --margin-check --break-fun-decl=fit-or-vertical --break-fun-sig=fit-or-vertical %{dep:../tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl-fit_or_vertical.ml.ref break_fun_decl-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl-fit_or_vertical.ml.err break_fun_decl-fit_or_vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-smart.ml.stdout
   (with-stderr-to break_fun_decl-smart.ml.stderr
     (run %{bin:ocamlformat} --name break_fun_decl-smart.ml --margin-check --break-fun-decl=smart --break-fun-sig=smart %{dep:../tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl-smart.ml.ref break_fun_decl-smart.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl-smart.ml.err break_fun_decl-smart.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-wrap.ml.stdout
   (with-stderr-to break_fun_decl-wrap.ml.stderr
     (run %{bin:ocamlformat} --name break_fun_decl-wrap.ml --margin-check --break-fun-decl=wrap --break-fun-sig=wrap %{dep:../tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl-wrap.ml.ref break_fun_decl-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl-wrap.ml.err break_fun_decl-wrap.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl.ml.stdout
   (with-stderr-to break_fun_decl.ml.stderr
     (run %{bin:ocamlformat} --name break_fun_decl.ml --margin-check %{dep:../tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl.ml.ref break_fun_decl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_fun_decl.ml.err break_fun_decl.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_infix-fit-or-vertical.ml.stdout
   (with-stderr-to break_infix-fit-or-vertical.ml.stderr
     (run %{bin:ocamlformat} --name break_infix-fit-or-vertical.ml --margin-check --break-infix=fit-or-vertical %{dep:../tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_infix-fit-or-vertical.ml.ref break_infix-fit-or-vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_infix-fit-or-vertical.ml.err break_infix-fit-or-vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_infix-wrap.ml.stdout
   (with-stderr-to break_infix-wrap.ml.stderr
     (run %{bin:ocamlformat} --name break_infix-wrap.ml --margin-check --break-infix=wrap %{dep:../tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_infix-wrap.ml.ref break_infix-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_infix-wrap.ml.err break_infix-wrap.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_infix.ml.stdout
   (with-stderr-to break_infix.ml.stderr
     (run %{bin:ocamlformat} --name break_infix.ml --margin-check --break-infix=wrap-or-vertical %{dep:../tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_infix.ml.ref break_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_infix.ml.err break_infix.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_record.ml.stdout
   (with-stderr-to break_record.ml.stderr
     (run %{bin:ocamlformat} --name break_record.ml --margin-check --margin=58 %{dep:../tests/break_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_record.ml.ref break_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_record.ml.err break_record.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-after.ml.stdout
   (with-stderr-to break_separators-after.ml.stderr
     (run %{bin:ocamlformat} --name break_separators-after.ml --margin-check --break-separators=after --max-iter=3 %{dep:../tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators-after.ml.ref break_separators-after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators-after.ml.err break_separators-after.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-after_docked.ml.stdout
   (with-stderr-to break_separators-after_docked.ml.stderr
     (run %{bin:ocamlformat} --name break_separators-after_docked.ml --margin-check --break-separators=after --dock-collection-brackets --max-iter=3 %{dep:../tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators-after_docked.ml.ref break_separators-after_docked.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators-after_docked.ml.err break_separators-after_docked.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-before_docked.ml.stdout
   (with-stderr-to break_separators-before_docked.ml.stderr
     (run %{bin:ocamlformat} --name break_separators-before_docked.ml --margin-check --break-separators=before --dock-collection-brackets --max-iter=3 %{dep:../tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators-before_docked.ml.ref break_separators-before_docked.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators-before_docked.ml.err break_separators-before_docked.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_separators.ml.stdout
   (with-stderr-to break_separators.ml.stderr
     (run %{bin:ocamlformat} --name break_separators.ml --margin-check --break-separators=before --max-iter=3 %{dep:../tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators.ml.ref break_separators.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_separators.ml.err break_separators.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_sequence_before.ml.stdout
   (with-stderr-to break_sequence_before.ml.stderr
     (run %{bin:ocamlformat} --name break_sequence_before.ml --margin-check %{dep:../tests/break_sequence_before.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_sequence_before.ml.ref break_sequence_before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_sequence_before.ml.err break_sequence_before.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_string_literals-never.ml.stdout
   (with-stderr-to break_string_literals-never.ml.stderr
     (run %{bin:ocamlformat} --name break_string_literals-never.ml --margin-check --break-string-literals=never %{dep:../tests/break_string_literals.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_string_literals-never.ml.ref break_string_literals-never.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_string_literals-never.ml.err break_string_literals-never.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_string_literals.ml.stdout
   (with-stderr-to break_string_literals.ml.stderr
     (run %{bin:ocamlformat} --name break_string_literals.ml --margin-check --break-string-literals=auto %{dep:../tests/break_string_literals.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_string_literals.ml.ref break_string_literals.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_string_literals.ml.err break_string_literals.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to break_struct.ml.stdout
   (with-stderr-to break_struct.ml.stderr
     (run %{bin:ocamlformat} --name break_struct.ml --margin-check %{dep:../tests/break_struct.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_struct.ml.ref break_struct.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff break_struct.ml.err break_struct.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to cases_exp_grouping.ml.stdout
   (with-stderr-to cases_exp_grouping.ml.stderr
     (run %{bin:ocamlformat} --name cases_exp_grouping.ml --margin-check --exp-grouping=preserve %{dep:../tests/cases_exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff cases_exp_grouping.ml.ref cases_exp_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff cases_exp_grouping.ml.err cases_exp_grouping.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to cinaps.ml.stdout
   (with-stderr-to cinaps.ml.stderr
     (run %{bin:ocamlformat} --name cinaps.ml --margin-check %{dep:../tests/cinaps.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff cinaps.ml.ref cinaps.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff cinaps.ml.err cinaps.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to class_expr.ml.stdout
   (with-stderr-to class_expr.ml.stderr
     (run %{bin:ocamlformat} --name class_expr.ml --margin-check %{dep:../tests/class_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_expr.ml.ref class_expr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_expr.ml.err class_expr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to class_sig-after.mli.stdout
   (with-stderr-to class_sig-after.mli.stderr
     (run %{bin:ocamlformat} --name class_sig-after.mli --margin-check --break-separators=after %{dep:../tests/class_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_sig-after.mli.ref class_sig-after.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_sig-after.mli.err class_sig-after.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to class_sig.mli.stdout
   (with-stderr-to class_sig.mli.stderr
     (run %{bin:ocamlformat} --name class_sig.mli --margin-check %{dep:../tests/class_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_sig.mli.ref class_sig.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_sig.mli.err class_sig.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to class_type.ml.stdout
   (with-stderr-to class_type.ml.stderr
     (run %{bin:ocamlformat} --name class_type.ml --margin-check --max-iters=3 %{dep:../tests/class_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_type.ml.ref class_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff class_type.ml.err class_type.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to cmdline_override.ml.stdout
   (with-stderr-to cmdline_override.ml.stderr
     (run %{bin:ocamlformat} --name cmdline_override.ml --margin-check --config=module-item-spacing=compact --module-item-spacing=sparse %{dep:../tests/cmdline_override.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff cmdline_override.ml.ref cmdline_override.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff cmdline_override.ml.err cmdline_override.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to cmdline_override2.ml.stdout
   (with-stderr-to cmdline_override2.ml.stderr
     (run %{bin:ocamlformat} --name cmdline_override2.ml --margin-check --module-item-spacing=sparse --config=module-item-spacing=compact %{dep:../tests/cmdline_override2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff cmdline_override2.ml.ref cmdline_override2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff cmdline_override2.ml.err cmdline_override2.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to coerce.ml.stdout
   (with-stderr-to coerce.ml.stderr
     (run %{bin:ocamlformat} --name coerce.ml --margin-check %{dep:../tests/coerce.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff coerce.ml.ref coerce.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff coerce.ml.err coerce.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comment_breaking.ml.stdout
   (with-stderr-to comment_breaking.ml.stderr
     (run %{bin:ocamlformat} --name comment_breaking.ml --margin-check %{dep:../tests/comment_breaking.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_breaking.ml.ref comment_breaking.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_breaking.ml.err comment_breaking.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to comment_header.ml.stdout
   (with-stderr-to comment_header.ml.stderr
     (run %{bin:ocamlformat} --name comment_header.ml --margin-check %{dep:../tests/comment_header.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff comment_header.ml.ref comment_header.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff comment_header.ml.err comment_header.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comment_in_empty.ml.stdout
   (with-stderr-to comment_in_empty.ml.stderr
     (run %{bin:ocamlformat} --name comment_in_empty.ml --margin-check %{dep:../tests/comment_in_empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_in_empty.ml.ref comment_in_empty.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_in_empty.ml.err comment_in_empty.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comment_in_modules.ml.stdout
   (with-stderr-to comment_in_modules.ml.stderr
     (run %{bin:ocamlformat} --name comment_in_modules.ml --margin-check %{dep:../tests/comment_in_modules.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_in_modules.ml.ref comment_in_modules.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_in_modules.ml.err comment_in_modules.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comment_last.ml.stdout
   (with-stderr-to comment_last.ml.stderr
     (run %{bin:ocamlformat} --name comment_last.ml --margin-check %{dep:../tests/comment_last.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_last.ml.ref comment_last.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_last.ml.err comment_last.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comment_sparse.ml.stdout
   (with-stderr-to comment_sparse.ml.stderr
     (run %{bin:ocamlformat} --name comment_sparse.ml --margin-check %{dep:../tests/comment_sparse.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_sparse.ml.ref comment_sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comment_sparse.ml.err comment_sparse.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments-no-wrap.ml.stdout
   (with-stderr-to comments-no-wrap.ml.stderr
     (run %{bin:ocamlformat} --name comments-no-wrap.ml --margin-check --no-wrap-comments --max-iter=4 %{dep:../tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments-no-wrap.ml.ref comments-no-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments-no-wrap.ml.err comments-no-wrap.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments.ml.stdout
   (with-stderr-to comments.ml.stderr
     (run %{bin:ocamlformat} --name comments.ml --margin-check --max-iter=4 %{dep:../tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments.ml.ref comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments.ml.err comments.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments.mli.stdout
   (with-stderr-to comments.mli.stderr
     (run %{bin:ocamlformat} --name comments.mli --margin-check %{dep:../tests/comments.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments.mli.ref comments.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments.mli.err comments.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments_args.ml.stdout
   (with-stderr-to comments_args.ml.stderr
     (run %{bin:ocamlformat} --name comments_args.ml --margin-check --max-iter=4 %{dep:../tests/comments_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_args.ml.ref comments_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_args.ml.err comments_args.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments_around_disabled.ml.stdout
   (with-stderr-to comments_around_disabled.ml.stderr
     (run %{bin:ocamlformat} --name comments_around_disabled.ml --margin-check %{dep:../tests/comments_around_disabled.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_around_disabled.ml.ref comments_around_disabled.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_around_disabled.ml.err comments_around_disabled.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments_in_local_let.ml.stdout
   (with-stderr-to comments_in_local_let.ml.stderr
     (run %{bin:ocamlformat} --name comments_in_local_let.ml --margin-check %{dep:../tests/comments_in_local_let.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_local_let.ml.ref comments_in_local_let.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_local_let.ml.err comments_in_local_let.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments_in_record-break_separator-after.ml.stdout
   (with-stderr-to comments_in_record-break_separator-after.ml.stderr
     (run %{bin:ocamlformat} --name comments_in_record-break_separator-after.ml --margin-check --break-separator=after %{dep:../tests/comments_in_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_record-break_separator-after.ml.ref comments_in_record-break_separator-after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_record-break_separator-after.ml.err comments_in_record-break_separator-after.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments_in_record-break_separator-before.ml.stdout
   (with-stderr-to comments_in_record-break_separator-before.ml.stderr
     (run %{bin:ocamlformat} --name comments_in_record-break_separator-before.ml --margin-check --break-separator=before %{dep:../tests/comments_in_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_record-break_separator-before.ml.ref comments_in_record-break_separator-before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_record-break_separator-before.ml.err comments_in_record-break_separator-before.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to comments_in_record.ml.stdout
   (with-stderr-to comments_in_record.ml.stderr
     (run %{bin:ocamlformat} --name comments_in_record.ml --margin-check %{dep:../tests/comments_in_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_record.ml.ref comments_in_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff comments_in_record.ml.err comments_in_record.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to crlf_to_crlf.ml.stdout
   (with-stderr-to crlf_to_crlf.ml.stderr
     (run %{bin:ocamlformat} --name crlf_to_crlf.ml --margin-check --line-endings=crlf %{dep:../tests/crlf_to_crlf.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff crlf_to_crlf.ml.ref crlf_to_crlf.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff crlf_to_crlf.ml.err crlf_to_crlf.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to crlf_to_lf.ml.stdout
   (with-stderr-to crlf_to_lf.ml.stderr
     (run %{bin:ocamlformat} --name crlf_to_lf.ml --margin-check --line-endings=lf %{dep:../tests/crlf_to_lf.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff crlf_to_lf.ml.ref crlf_to_lf.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff crlf_to_lf.ml.err crlf_to_lf.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to custom_list.ml.stdout
   (with-stderr-to custom_list.ml.stderr
     (run %{bin:ocamlformat} --name custom_list.ml --margin-check %{dep:../tests/custom_list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff custom_list.ml.ref custom_list.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff custom_list.ml.err custom_list.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to directives.mlt.stdout
   (with-stderr-to directives.mlt.stderr
     (run %{bin:ocamlformat} --name directives.mlt --margin-check %{dep:../tests/directives.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff directives.mlt.ref directives.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff directives.mlt.err directives.mlt.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disable_attr.ml.stdout
   (with-stderr-to disable_attr.ml.stderr
     (run %{bin:ocamlformat} --name disable_attr.ml --margin-check %{dep:../tests/disable_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_attr.ml.ref disable_attr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_attr.ml.err disable_attr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disable_class_type.ml.stdout
   (with-stderr-to disable_class_type.ml.stderr
     (run %{bin:ocamlformat} --name disable_class_type.ml --margin-check %{dep:../tests/disable_class_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_class_type.ml.ref disable_class_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_class_type.ml.err disable_class_type.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disable_conf_attrs.ml.stdout
   (with-stderr-to disable_conf_attrs.ml.stderr
     (run %{bin:ocamlformat} --name disable_conf_attrs.ml --margin-check --disable-conf-attrs %{dep:../tests/disable_conf_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_conf_attrs.ml.ref disable_conf_attrs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_conf_attrs.ml.err disable_conf_attrs.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disable_local_let.ml.stdout
   (with-stderr-to disable_local_let.ml.stderr
     (run %{bin:ocamlformat} --name disable_local_let.ml --margin-check %{dep:../tests/disable_local_let.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_local_let.ml.ref disable_local_let.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disable_local_let.ml.err disable_local_let.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disabled.ml.stdout
   (with-stderr-to disabled.ml.stderr
     (run %{bin:ocamlformat} --name disabled.ml --margin-check --disable %{dep:../tests/disabled.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disabled.ml.ref disabled.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disabled.ml.err disabled.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disabled_attr.ml.stdout
   (with-stderr-to disabled_attr.ml.stderr
     (run %{bin:ocamlformat} --name disabled_attr.ml --margin-check %{dep:../tests/disabled_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disabled_attr.ml.ref disabled_attr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disabled_attr.ml.err disabled_attr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disambiguate.ml.stdout
   (with-stderr-to disambiguate.ml.stderr
     (run %{bin:ocamlformat} --name disambiguate.ml --margin-check %{dep:../tests/disambiguate.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disambiguate.ml.ref disambiguate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disambiguate.ml.err disambiguate.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to disambiguated_types.ml.stdout
   (with-stderr-to disambiguated_types.ml.stderr
     (run %{bin:ocamlformat} --name disambiguated_types.ml --margin-check %{dep:../tests/disambiguated_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disambiguated_types.ml.ref disambiguated_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff disambiguated_types.ml.err disambiguated_types.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc.mld.stdout
   (with-stderr-to doc.mld.stderr
     (run %{bin:ocamlformat} --name doc.mld --margin-check %{dep:../tests/doc.mld})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc.mld.ref doc.mld.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc.mld.err doc.mld.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-after.ml.stdout
   (with-stderr-to doc_comments-after.ml.stderr
     (run %{bin:ocamlformat} --name doc_comments-after.ml --margin-check --doc-comments=after-when-possible %{dep:../tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-after.ml.ref doc_comments-after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-after.ml.err doc_comments-after.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-before-except-val.ml.stdout
   (with-stderr-to doc_comments-before-except-val.ml.stderr
     (run %{bin:ocamlformat} --name doc_comments-before-except-val.ml --margin-check --doc-comments=before-except-val %{dep:../tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-before-except-val.ml.ref doc_comments-before-except-val.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-before-except-val.ml.err doc_comments-before-except-val.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-before.ml.stdout
   (with-stderr-to doc_comments-before.ml.stderr
     (run %{bin:ocamlformat} --name doc_comments-before.ml --margin-check --doc-comments=before %{dep:../tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-before.ml.ref doc_comments-before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-before.ml.err doc_comments-before.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-no-parse-docstrings.mli.stdout
   (with-stderr-to doc_comments-no-parse-docstrings.mli.stderr
     (run %{bin:ocamlformat} --name doc_comments-no-parse-docstrings.mli --margin-check --no-parse-docstrings --max-iters=3 %{dep:../tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-no-parse-docstrings.mli.ref doc_comments-no-parse-docstrings.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments-no-parse-docstrings.mli.err doc_comments-no-parse-docstrings.mli.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-no-wrap.mli.stdout
   (with-stderr-to doc_comments-no-wrap.mli.stderr
     (run %{bin:ocamlformat} --name doc_comments-no-wrap.mli --margin-check --no-wrap-comments %{dep:../tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff doc_comments-no-wrap.mli.ref doc_comments-no-wrap.mli.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff doc_comments-no-wrap.mli.err doc_comments-no-wrap.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments.ml.stdout
   (with-stderr-to doc_comments.ml.stderr
     (run %{bin:ocamlformat} --name doc_comments.ml --margin-check %{dep:../tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments.ml.ref doc_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments.ml.err doc_comments.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments.mli.stdout
   (with-stderr-to doc_comments.mli.stderr
     (run %{bin:ocamlformat} --name doc_comments.mli --margin-check %{dep:../tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff doc_comments.mli.ref doc_comments.mli.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff doc_comments.mli.err doc_comments.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments_padding.ml.stdout
   (with-stderr-to doc_comments_padding.ml.stderr
     (run %{bin:ocamlformat} --name doc_comments_padding.ml --margin-check %{dep:../tests/doc_comments_padding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments_padding.ml.ref doc_comments_padding.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_comments_padding.ml.err doc_comments_padding.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to doc_repl.mld.stdout
   (with-stderr-to doc_repl.mld.stderr
     (run %{bin:ocamlformat} --name doc_repl.mld --margin-check --parse-toplevel-phrases %{dep:../tests/doc_repl.mld})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_repl.mld.ref doc_repl.mld.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff doc_repl.mld.err doc_repl.mld.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to docstrings_toplevel_directives.mlt.stdout
   (with-stderr-to docstrings_toplevel_directives.mlt.stderr
     (run %{bin:ocamlformat} --name docstrings_toplevel_directives.mlt --margin-check %{dep:../tests/docstrings_toplevel_directives.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff docstrings_toplevel_directives.mlt.ref docstrings_toplevel_directives.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff docstrings_toplevel_directives.mlt.err docstrings_toplevel_directives.mlt.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to effects.ml.stdout
   (with-stderr-to effects.ml.stderr
     (run %{bin:ocamlformat} --name effects.ml --margin-check --ocaml-version=5.3 %{dep:../tests/effects.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff effects.ml.ref effects.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff effects.ml.err effects.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to eliom_ext.eliom.stdout
   (with-stderr-to eliom_ext.eliom.stderr
     (run %{bin:ocamlformat} --name eliom_ext.eliom --margin-check %{dep:../tests/eliom_ext.eliom})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff eliom_ext.eliom.ref eliom_ext.eliom.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff eliom_ext.eliom.err eliom_ext.eliom.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to empty.ml.stdout
   (with-stderr-to empty.ml.stderr
     (run %{bin:ocamlformat} --name empty.ml --margin-check %{dep:../tests/empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty.ml.ref empty.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty.ml.err empty.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to empty_ml.ml.stdout
   (with-stderr-to empty_ml.ml.stderr
     (run %{bin:ocamlformat} --name empty_ml.ml --margin-check %{dep:../tests/empty_ml.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty_ml.ml.ref empty_ml.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty_ml.ml.err empty_ml.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to empty_mli.mli.stdout
   (with-stderr-to empty_mli.mli.stderr
     (run %{bin:ocamlformat} --name empty_mli.mli --margin-check %{dep:../tests/empty_mli.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty_mli.mli.ref empty_mli.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty_mli.mli.err empty_mli.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to empty_mlt.mlt.stdout
   (with-stderr-to empty_mlt.mlt.stderr
     (run %{bin:ocamlformat} --name empty_mlt.mlt --margin-check %{dep:../tests/empty_mlt.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty_mlt.mlt.ref empty_mlt.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff empty_mlt.mlt.err empty_mlt.mlt.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to error1.ml.stdout
   (with-stderr-to error1.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --name error1.ml --margin-check %{dep:../tests/error1.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error1.ml.ref error1.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error1.ml.err error1.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to error2.ml.stdout
   (with-stderr-to error2.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --name error2.ml --margin-check %{dep:../tests/error2.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error2.ml.ref error2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error2.ml.err error2.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to error3.ml.stdout
   (with-stderr-to error3.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --name error3.ml --margin-check %{dep:../tests/error3.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error3.ml.ref error3.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error3.ml.err error3.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to error4.ml.stdout
   (with-stderr-to error4.ml.stderr
     (run %{bin:ocamlformat} --name error4.ml --margin-check --no-comment-check %{dep:../tests/error4.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error4.ml.ref error4.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff error4.ml.err error4.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to escaped_nl.ml.stdout
   (with-stderr-to escaped_nl.ml.stderr
     (run %{bin:ocamlformat} --name escaped_nl.ml --margin-check %{dep:../tests/escaped_nl.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff escaped_nl.ml.ref escaped_nl.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff escaped_nl.ml.err escaped_nl.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to exceptions.ml.stdout
   (with-stderr-to exceptions.ml.stderr
     (run %{bin:ocamlformat} --name exceptions.ml --margin-check %{dep:../tests/exceptions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exceptions.ml.ref exceptions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exceptions.ml.err exceptions.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to exceptions.mli.stdout
   (with-stderr-to exceptions.mli.stderr
     (run %{bin:ocamlformat} --name exceptions.mli --margin-check %{dep:../tests/exceptions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exceptions.mli.ref exceptions.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exceptions.mli.err exceptions.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to exp_grouping-parens.ml.stdout
   (with-stderr-to exp_grouping-parens.ml.stderr
     (run %{bin:ocamlformat} --name exp_grouping-parens.ml --margin-check --exp-grouping=parens %{dep:../tests/exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exp_grouping-parens.ml.ref exp_grouping-parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exp_grouping-parens.ml.err exp_grouping-parens.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to exp_grouping.ml.stdout
   (with-stderr-to exp_grouping.ml.stderr
     (run %{bin:ocamlformat} --name exp_grouping.ml --margin-check --exp-grouping=preserve %{dep:../tests/exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exp_grouping.ml.ref exp_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exp_grouping.ml.err exp_grouping.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to exp_record.ml.stdout
   (with-stderr-to exp_record.ml.stderr
     (run %{bin:ocamlformat} --name exp_record.ml --margin-check %{dep:../tests/exp_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exp_record.ml.ref exp_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff exp_record.ml.err exp_record.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to expect_test.ml.stdout
   (with-stderr-to expect_test.ml.stderr
     (run %{bin:ocamlformat} --name expect_test.ml --margin-check %{dep:../tests/expect_test.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff expect_test.ml.ref expect_test.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff expect_test.ml.err expect_test.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to extensions-indent.ml.stdout
   (with-stderr-to extensions-indent.ml.stderr
     (run %{bin:ocamlformat} --name extensions-indent.ml --margin-check --max-iters=3 --extension-indent=5 --stritem-extension-indent=3 %{dep:../tests/extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions-indent.ml.ref extensions-indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions-indent.ml.err extensions-indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to extensions-indent.mli.stdout
   (with-stderr-to extensions-indent.mli.stderr
     (run %{bin:ocamlformat} --name extensions-indent.mli --margin-check --extension-indent=5 --stritem-extension-indent=3 %{dep:../tests/extensions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions-indent.mli.ref extensions-indent.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions-indent.mli.err extensions-indent.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to extensions.ml.stdout
   (with-stderr-to extensions.ml.stderr
     (run %{bin:ocamlformat} --name extensions.ml --margin-check --max-iters=3 %{dep:../tests/extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions.ml.ref extensions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions.ml.err extensions.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to extensions.mli.stdout
   (with-stderr-to extensions.mli.stderr
     (run %{bin:ocamlformat} --name extensions.mli --margin-check %{dep:../tests/extensions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions.mli.ref extensions.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions.mli.err extensions.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to extensions_exp_grouping.ml.stdout
   (with-stderr-to extensions_exp_grouping.ml.stderr
     (run %{bin:ocamlformat} --name extensions_exp_grouping.ml --margin-check --exp-grouping=preserve %{dep:../tests/extensions_exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions_exp_grouping.ml.ref extensions_exp_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff extensions_exp_grouping.ml.err extensions_exp_grouping.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to field-op_begin_line.ml.stdout
   (with-stderr-to field-op_begin_line.ml.stderr
     (run %{bin:ocamlformat} --name field-op_begin_line.ml --margin-check --assignment-operator=begin-line %{dep:../tests/field.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff field-op_begin_line.ml.ref field-op_begin_line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff field-op_begin_line.ml.err field-op_begin_line.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to field.ml.stdout
   (with-stderr-to field.ml.stderr
     (run %{bin:ocamlformat} --name field.ml --margin-check %{dep:../tests/field.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff field.ml.ref field.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff field.ml.err field.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to first_class_module.ml.stdout
   (with-stderr-to first_class_module.ml.stderr
     (run %{bin:ocamlformat} --name first_class_module.ml --margin-check %{dep:../tests/first_class_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff first_class_module.ml.ref first_class_module.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff first_class_module.ml.err first_class_module.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to floating_doc.ml.stdout
   (with-stderr-to floating_doc.ml.stderr
     (run %{bin:ocamlformat} --name floating_doc.ml --margin-check %{dep:../tests/floating_doc.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff floating_doc.ml.ref floating_doc.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff floating_doc.ml.err floating_doc.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to for_while.ml.stdout
   (with-stderr-to for_while.ml.stderr
     (run %{bin:ocamlformat} --name for_while.ml --margin-check %{dep:../tests/for_while.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff for_while.ml.ref for_while.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff for_while.ml.err for_while.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to fun_decl-no-wrap-fun-args.ml.stdout
   (with-stderr-to fun_decl-no-wrap-fun-args.ml.stderr
     (run %{bin:ocamlformat} --name fun_decl-no-wrap-fun-args.ml --margin-check --no-wrap-fun-args %{dep:../tests/fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff fun_decl-no-wrap-fun-args.ml.ref fun_decl-no-wrap-fun-args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff fun_decl-no-wrap-fun-args.ml.err fun_decl-no-wrap-fun-args.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to fun_decl.ml.stdout
   (with-stderr-to fun_decl.ml.stderr
     (run %{bin:ocamlformat} --name fun_decl.ml --margin-check %{dep:../tests/fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff fun_decl.ml.ref fun_decl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff fun_decl.ml.err fun_decl.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to fun_function.ml.stdout
   (with-stderr-to fun_function.ml.stderr
     (run %{bin:ocamlformat} --name fun_function.ml --margin-check --max-iter=3 %{dep:../tests/fun_function.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff fun_function.ml.ref fun_function.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff fun_function.ml.err fun_function.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to function_indent-never.ml.stdout
   (with-stderr-to function_indent-never.ml.stderr
     (run %{bin:ocamlformat} --name function_indent-never.ml --margin-check --function-indent=4 --function-indent-nested=never %{dep:../tests/function_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff function_indent-never.ml.ref function_indent-never.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff function_indent-never.ml.err function_indent-never.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to function_indent.ml.stdout
   (with-stderr-to function_indent.ml.stderr
     (run %{bin:ocamlformat} --name function_indent.ml --margin-check --function-indent=4 --function-indent-nested=always %{dep:../tests/function_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff function_indent.ml.ref function_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff function_indent.ml.err function_indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to functor.ml.stdout
   (with-stderr-to functor.ml.stderr
     (run %{bin:ocamlformat} --name functor.ml --margin-check %{dep:../tests/functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff functor.ml.ref functor.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff functor.ml.err functor.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to functor.mli.stdout
   (with-stderr-to functor.mli.stderr
     (run %{bin:ocamlformat} --name functor.mli --margin-check %{dep:../tests/functor.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff functor.mli.ref functor.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff functor.mli.err functor.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to funsig.ml.stdout
   (with-stderr-to funsig.ml.stderr
     (run %{bin:ocamlformat} --name funsig.ml --margin-check %{dep:../tests/funsig.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff funsig.ml.ref funsig.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff funsig.ml.err funsig.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to gadt.ml.stdout
   (with-stderr-to gadt.ml.stderr
     (run %{bin:ocamlformat} --name gadt.ml --margin-check %{dep:../tests/gadt.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff gadt.ml.ref gadt.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff gadt.ml.err gadt.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to generative.ml.stdout
   (with-stderr-to generative.ml.stderr
     (run %{bin:ocamlformat} --name generative.ml --margin-check --max-iters=3 %{dep:../tests/generative.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff generative.ml.ref generative.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff generative.ml.err generative.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to hash_bang.ml.stdout
   (with-stderr-to hash_bang.ml.stderr
     (run %{bin:ocamlformat} --name hash_bang.ml --margin-check %{dep:../tests/hash_bang.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff hash_bang.ml.ref hash_bang.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff hash_bang.ml.err hash_bang.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to hash_types.ml.stdout
   (with-stderr-to hash_types.ml.stderr
     (run %{bin:ocamlformat} --name hash_types.ml --margin-check %{dep:../tests/hash_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff hash_types.ml.ref hash_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff hash_types.ml.err hash_types.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to holes.ml.stdout
   (with-stderr-to holes.ml.stderr
     (run %{bin:ocamlformat} --name holes.ml --margin-check %{dep:../tests/holes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff holes.ml.ref holes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff holes.ml.err holes.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ifand.ml.stdout
   (with-stderr-to ifand.ml.stderr
     (run %{bin:ocamlformat} --name ifand.ml --margin-check %{dep:../tests/ifand.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ifand.ml.ref ifand.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ifand.ml.err ifand.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to index_op.ml.stdout
   (with-stderr-to index_op.ml.stderr
     (run %{bin:ocamlformat} --name index_op.ml --margin-check %{dep:../tests/index_op.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff index_op.ml.ref index_op.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff index_op.ml.err index_op.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters-cosl.ml.stdout
   (with-stderr-to indicate_multiline_delimiters-cosl.ml.stderr
     (run %{bin:ocamlformat} --name indicate_multiline_delimiters-cosl.ml --margin-check --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff indicate_multiline_delimiters-cosl.ml.ref indicate_multiline_delimiters-cosl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff indicate_multiline_delimiters-cosl.ml.err indicate_multiline_delimiters-cosl.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters-space.ml.stdout
   (with-stderr-to indicate_multiline_delimiters-space.ml.stderr
     (run %{bin:ocamlformat} --name indicate_multiline_delimiters-space.ml --margin-check --indicate-multiline-delimiters=space %{dep:../tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff indicate_multiline_delimiters-space.ml.ref indicate_multiline_delimiters-space.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff indicate_multiline_delimiters-space.ml.err indicate_multiline_delimiters-space.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters.ml.stdout
   (with-stderr-to indicate_multiline_delimiters.ml.stderr
     (run %{bin:ocamlformat} --name indicate_multiline_delimiters.ml --margin-check --indicate-multiline-delimiters=no %{dep:../tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff indicate_multiline_delimiters.ml.ref indicate_multiline_delimiters.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff indicate_multiline_delimiters.ml.err indicate_multiline_delimiters.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to infix_arg_grouping.ml.stdout
   (with-stderr-to infix_arg_grouping.ml.stderr
     (run %{bin:ocamlformat} --name infix_arg_grouping.ml --margin-check %{dep:../tests/infix_arg_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_arg_grouping.ml.ref infix_arg_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_arg_grouping.ml.err infix_arg_grouping.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-break.ml.stdout
   (with-stderr-to infix_bind-break.ml.stderr
     (run %{bin:ocamlformat} --name infix_bind-break.ml --margin-check --break-infix=wrap --break-infix-before-func --max-iters=3 %{dep:../tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind-break.ml.ref infix_bind-break.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind-break.ml.err infix_bind-break.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-fit_or_vertical-break.ml.stdout
   (with-stderr-to infix_bind-fit_or_vertical-break.ml.stderr
     (run %{bin:ocamlformat} --name infix_bind-fit_or_vertical-break.ml --margin-check --break-infix=fit-or-vertical --break-infix-before-func --max-iters=3 %{dep:../tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind-fit_or_vertical-break.ml.ref infix_bind-fit_or_vertical-break.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind-fit_or_vertical-break.ml.err infix_bind-fit_or_vertical-break.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-fit_or_vertical.ml.stdout
   (with-stderr-to infix_bind-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --name infix_bind-fit_or_vertical.ml --margin-check --break-infix=fit-or-vertical --no-break-infix-before-func %{dep:../tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind-fit_or_vertical.ml.ref infix_bind-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind-fit_or_vertical.ml.err infix_bind-fit_or_vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind.ml.stdout
   (with-stderr-to infix_bind.ml.stderr
     (run %{bin:ocamlformat} --name infix_bind.ml --margin-check --break-infix=wrap --no-break-infix-before-func %{dep:../tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind.ml.ref infix_bind.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_bind.ml.err infix_bind.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to infix_precedence.ml.stdout
   (with-stderr-to infix_precedence.ml.stderr
     (run %{bin:ocamlformat} --name infix_precedence.ml --margin-check --infix-precedence=parens %{dep:../tests/infix_precedence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_precedence.ml.ref infix_precedence.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff infix_precedence.ml.err infix_precedence.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to injectivity.ml.stdout
   (with-stderr-to injectivity.ml.stderr
     (run %{bin:ocamlformat} --name injectivity.ml --margin-check %{dep:../tests/injectivity.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff injectivity.ml.ref injectivity.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff injectivity.ml.err injectivity.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to into_infix.ml.stdout
   (with-stderr-to into_infix.ml.stderr
     (run %{bin:ocamlformat} --name into_infix.ml --margin-check %{dep:../tests/into_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff into_infix.ml.ref into_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff into_infix.ml.err into_infix.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to invalid.ml.stdout
   (with-stderr-to invalid.ml.stderr
     (run %{bin:ocamlformat} --name invalid.ml --margin-check %{dep:../tests/invalid.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff invalid.ml.ref invalid.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff invalid.ml.err invalid.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to invalid_docstring.ml.stdout
   (with-stderr-to invalid_docstring.ml.stderr
     (run %{bin:ocamlformat} --name invalid_docstring.ml --margin-check %{dep:../tests/invalid_docstring.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff invalid_docstring.ml.ref invalid_docstring.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff invalid_docstring.ml.err invalid_docstring.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to invalid_docstrings.mli.stdout
   (with-stderr-to invalid_docstrings.mli.stderr
     (run %{bin:ocamlformat} --name invalid_docstrings.mli --margin-check %{dep:../tests/invalid_docstrings.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff invalid_docstrings.mli.ref invalid_docstrings.mli.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff invalid_docstrings.mli.err invalid_docstrings.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue114.ml.stdout
   (with-stderr-to issue114.ml.stderr
     (run %{bin:ocamlformat} --name issue114.ml --margin-check %{dep:../tests/issue114.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue114.ml.ref issue114.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue114.ml.err issue114.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue1750.ml.stdout
   (with-stderr-to issue1750.ml.stderr
     (run %{bin:ocamlformat} --name issue1750.ml --margin-check %{dep:../tests/issue1750.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue1750.ml.ref issue1750.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue1750.ml.err issue1750.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue2660.ml.stdout
   (with-stderr-to issue2660.ml.stderr
     (run %{bin:ocamlformat} --name issue2660.ml --margin-check --profile=default --max-indent=2 %{dep:../tests/issue2660.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue2660.ml.ref issue2660.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue2660.ml.err issue2660.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue289.ml.stdout
   (with-stderr-to issue289.ml.stderr
     (run %{bin:ocamlformat} --name issue289.ml --margin-check %{dep:../tests/issue289.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue289.ml.ref issue289.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue289.ml.err issue289.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue48.ml.stdout
   (with-stderr-to issue48.ml.stderr
     (run %{bin:ocamlformat} --name issue48.ml --margin-check %{dep:../tests/issue48.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue48.ml.ref issue48.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue48.ml.err issue48.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue51.ml.stdout
   (with-stderr-to issue51.ml.stderr
     (run %{bin:ocamlformat} --name issue51.ml --margin-check %{dep:../tests/issue51.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue51.ml.ref issue51.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue51.ml.err issue51.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue57.ml.stdout
   (with-stderr-to issue57.ml.stderr
     (run %{bin:ocamlformat} --name issue57.ml --margin-check %{dep:../tests/issue57.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue57.ml.ref issue57.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue57.ml.err issue57.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue60.ml.stdout
   (with-stderr-to issue60.ml.stderr
     (run %{bin:ocamlformat} --name issue60.ml --margin-check %{dep:../tests/issue60.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue60.ml.ref issue60.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue60.ml.err issue60.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue77.ml.stdout
   (with-stderr-to issue77.ml.stderr
     (run %{bin:ocamlformat} --name issue77.ml --margin-check %{dep:../tests/issue77.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue77.ml.ref issue77.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue77.ml.err issue77.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue85.ml.stdout
   (with-stderr-to issue85.ml.stderr
     (run %{bin:ocamlformat} --name issue85.ml --margin-check %{dep:../tests/issue85.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue85.ml.ref issue85.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue85.ml.err issue85.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to issue89.ml.stdout
   (with-stderr-to issue89.ml.stderr
     (run %{bin:ocamlformat} --name issue89.ml --margin-check %{dep:../tests/issue89.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue89.ml.ref issue89.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff issue89.ml.err issue89.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-compact.ml.stdout
   (with-stderr-to ite-compact.ml.stderr
     (run %{bin:ocamlformat} --name ite-compact.ml --margin-check --if-then-else=compact %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-compact.ml.ref ite-compact.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-compact.ml.err ite-compact.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-compact_closing.ml.stdout
   (with-stderr-to ite-compact_closing.ml.stderr
     (run %{bin:ocamlformat} --name ite-compact_closing.ml --margin-check --if-then-else=compact --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-compact_closing.ml.ref ite-compact_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-compact_closing.ml.err ite-compact_closing.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical.ml.stdout
   (with-stderr-to ite-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --name ite-fit_or_vertical.ml --margin-check --if-then-else=fit-or-vertical %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-fit_or_vertical.ml.ref ite-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-fit_or_vertical.ml.err ite-fit_or_vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical_closing.ml.stdout
   (with-stderr-to ite-fit_or_vertical_closing.ml.stderr
     (run %{bin:ocamlformat} --name ite-fit_or_vertical_closing.ml --margin-check --if-then-else fit-or-vertical --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-fit_or_vertical_closing.ml.ref ite-fit_or_vertical_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-fit_or_vertical_closing.ml.err ite-fit_or_vertical_closing.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical_no_indicate.ml.stdout
   (with-stderr-to ite-fit_or_vertical_no_indicate.ml.stderr
     (run %{bin:ocamlformat} --name ite-fit_or_vertical_no_indicate.ml --margin-check --if-then-else=fit-or-vertical --indicate-multiline-delimiters=no %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-fit_or_vertical_no_indicate.ml.ref ite-fit_or_vertical_no_indicate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-fit_or_vertical_no_indicate.ml.err ite-fit_or_vertical_no_indicate.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-kr.ml.stdout
   (with-stderr-to ite-kr.ml.stderr
     (run %{bin:ocamlformat} --name ite-kr.ml --margin-check --if-then-else=k-r --max-iters=3 %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kr.ml.ref ite-kr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kr.ml.err ite-kr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-kr_closing.ml.stdout
   (with-stderr-to ite-kr_closing.ml.stderr
     (run %{bin:ocamlformat} --name ite-kr_closing.ml --margin-check --if-then-else=k-r --max-iters=3 --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kr_closing.ml.ref ite-kr_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kr_closing.ml.err ite-kr_closing.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first.ml.stdout
   (with-stderr-to ite-kw_first.ml.stderr
     (run %{bin:ocamlformat} --name ite-kw_first.ml --margin-check --if-then-else=keyword-first %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kw_first.ml.ref ite-kw_first.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kw_first.ml.err ite-kw_first.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first_closing.ml.stdout
   (with-stderr-to ite-kw_first_closing.ml.stderr
     (run %{bin:ocamlformat} --name ite-kw_first_closing.ml --margin-check --if-then-else keyword-first --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kw_first_closing.ml.ref ite-kw_first_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kw_first_closing.ml.err ite-kw_first_closing.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first_no_indicate.ml.stdout
   (with-stderr-to ite-kw_first_no_indicate.ml.stderr
     (run %{bin:ocamlformat} --name ite-kw_first_no_indicate.ml --margin-check --if-then-else=keyword-first --indicate-multiline-delimiters=no %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kw_first_no_indicate.ml.ref ite-kw_first_no_indicate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-kw_first_no_indicate.ml.err ite-kw_first_no_indicate.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-no_indicate.ml.stdout
   (with-stderr-to ite-no_indicate.ml.stderr
     (run %{bin:ocamlformat} --name ite-no_indicate.ml --margin-check --if-then-else=compact --indicate-multiline-delimiters=no %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-no_indicate.ml.ref ite-no_indicate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-no_indicate.ml.err ite-no_indicate.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite-vertical.ml.stdout
   (with-stderr-to ite-vertical.ml.stderr
     (run %{bin:ocamlformat} --name ite-vertical.ml --margin-check --if-then-else=vertical %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-vertical.ml.ref ite-vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite-vertical.ml.err ite-vertical.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ite.ml.stdout
   (with-stderr-to ite.ml.stderr
     (run %{bin:ocamlformat} --name ite.ml --margin-check --if-then-else=compact %{dep:../tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite.ml.ref ite.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ite.ml.err ite.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_args.ml.stdout
   (with-stderr-to js_args.ml.stderr
     (run %{bin:ocamlformat} --name js_args.ml --margin-check --max-iter=3 %{dep:../tests/js_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_args.ml.ref js_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_args.ml.err js_args.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_begin.ml.stdout
   (with-stderr-to js_begin.ml.stderr
     (run %{bin:ocamlformat} --name js_begin.ml --margin-check %{dep:../tests/js_begin.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_begin.ml.ref js_begin.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_begin.ml.err js_begin.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_bind.ml.stdout
   (with-stderr-to js_bind.ml.stderr
     (run %{bin:ocamlformat} --name js_bind.ml --margin-check %{dep:../tests/js_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_bind.ml.ref js_bind.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_bind.ml.err js_bind.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_fun.ml.stdout
   (with-stderr-to js_fun.ml.stderr
     (run %{bin:ocamlformat} --name js_fun.ml --margin-check --max-iter=3 %{dep:../tests/js_fun.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_fun.ml.ref js_fun.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_fun.ml.err js_fun.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_map.ml.stdout
   (with-stderr-to js_map.ml.stderr
     (run %{bin:ocamlformat} --name js_map.ml --margin-check --max-iter=3 %{dep:../tests/js_map.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_map.ml.ref js_map.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_map.ml.err js_map.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_pattern.ml.stdout
   (with-stderr-to js_pattern.ml.stderr
     (run %{bin:ocamlformat} --name js_pattern.ml --margin-check %{dep:../tests/js_pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_pattern.ml.ref js_pattern.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_pattern.ml.err js_pattern.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_poly.ml.stdout
   (with-stderr-to js_poly.ml.stderr
     (run %{bin:ocamlformat} --name js_poly.ml --margin-check --max-iter=3 %{dep:../tests/js_poly.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_poly.ml.ref js_poly.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_poly.ml.err js_poly.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_record.ml.stdout
   (with-stderr-to js_record.ml.stderr
     (run %{bin:ocamlformat} --name js_record.ml --margin-check --max-iter=3 %{dep:../tests/js_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_record.ml.ref js_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_record.ml.err js_record.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_sig.mli.stdout
   (with-stderr-to js_sig.mli.stderr
     (run %{bin:ocamlformat} --name js_sig.mli --margin-check %{dep:../tests/js_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_sig.mli.ref js_sig.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_sig.mli.err js_sig.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_source.ml.stdout
   (with-stderr-to js_source.ml.stderr
     (run %{bin:ocamlformat} --name js_source.ml --margin-check --max-iters=3 %{dep:../tests/js_source.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_source.ml.ref js_source.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_source.ml.err js_source.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_syntax.ml.stdout
   (with-stderr-to js_syntax.ml.stderr
     (run %{bin:ocamlformat} --name js_syntax.ml --margin-check %{dep:../tests/js_syntax.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_syntax.ml.ref js_syntax.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_syntax.ml.err js_syntax.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to js_to_do.ml.stdout
   (with-stderr-to js_to_do.ml.stderr
     (run %{bin:ocamlformat} --name js_to_do.ml --margin-check %{dep:../tests/js_to_do.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff js_to_do.ml.ref js_to_do.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff js_to_do.ml.err js_to_do.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to js_upon.ml.stdout
   (with-stderr-to js_upon.ml.stderr
     (run %{bin:ocamlformat} --name js_upon.ml --margin-check %{dep:../tests/js_upon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_upon.ml.ref js_upon.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff js_upon.ml.err js_upon.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to kw_extentions.ml.stdout
   (with-stderr-to kw_extentions.ml.stderr
     (run %{bin:ocamlformat} --name kw_extentions.ml --margin-check %{dep:../tests/kw_extentions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff kw_extentions.ml.ref kw_extentions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff kw_extentions.ml.err kw_extentions.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to label_option_default_args.ml.stdout
   (with-stderr-to label_option_default_args.ml.stderr
     (run %{bin:ocamlformat} --name label_option_default_args.ml --margin-check --max-iters=4 %{dep:../tests/label_option_default_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff label_option_default_args.ml.ref label_option_default_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff label_option_default_args.ml.err label_option_default_args.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to labelled_args-414.ml.stdout
   (with-stderr-to labelled_args-414.ml.stderr
     (run %{bin:ocamlformat} --name labelled_args-414.ml --margin-check --ocaml-version=4.14.0 %{dep:../tests/labelled_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff labelled_args-414.ml.ref labelled_args-414.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff labelled_args-414.ml.err labelled_args-414.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to labelled_args.ml.stdout
   (with-stderr-to labelled_args.ml.stderr
     (run %{bin:ocamlformat} --name labelled_args.ml --margin-check %{dep:../tests/labelled_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff labelled_args.ml.ref labelled_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff labelled_args.ml.err labelled_args.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to lazy.ml.stdout
   (with-stderr-to lazy.ml.stderr
     (run %{bin:ocamlformat} --name lazy.ml --margin-check %{dep:../tests/lazy.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff lazy.ml.ref lazy.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff lazy.ml.err lazy.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-deindent-fun.ml.stdout
   (with-stderr-to let_binding-deindent-fun.ml.stderr
     (run %{bin:ocamlformat} --name let_binding-deindent-fun.ml --margin-check --no-let-binding-deindent-fun %{dep:../tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding-deindent-fun.ml.ref let_binding-deindent-fun.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding-deindent-fun.ml.err let_binding-deindent-fun.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-in_indent.ml.stdout
   (with-stderr-to let_binding-in_indent.ml.stderr
     (run %{bin:ocamlformat} --name let_binding-in_indent.ml --margin-check --indent-after-in=4 %{dep:../tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding-in_indent.ml.ref let_binding-in_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding-in_indent.ml.err let_binding-in_indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-indent.ml.stdout
   (with-stderr-to let_binding-indent.ml.stderr
     (run %{bin:ocamlformat} --name let_binding-indent.ml --margin-check --let-binding-indent=6 %{dep:../tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding-indent.ml.ref let_binding-indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding-indent.ml.err let_binding-indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding.ml.stdout
   (with-stderr-to let_binding.ml.stderr
     (run %{bin:ocamlformat} --name let_binding.ml --margin-check %{dep:../tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding.ml.ref let_binding.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding.ml.err let_binding.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing-double-semicolon.ml.stdout
   (with-stderr-to let_binding_spacing-double-semicolon.ml.stderr
     (run %{bin:ocamlformat} --name let_binding_spacing-double-semicolon.ml --margin-check --let-binding-spacing=double-semicolon %{dep:../tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding_spacing-double-semicolon.ml.ref let_binding_spacing-double-semicolon.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding_spacing-double-semicolon.ml.err let_binding_spacing-double-semicolon.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing-sparse.ml.stdout
   (with-stderr-to let_binding_spacing-sparse.ml.stderr
     (run %{bin:ocamlformat} --name let_binding_spacing-sparse.ml --margin-check --let-binding-spacing=sparse %{dep:../tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding_spacing-sparse.ml.ref let_binding_spacing-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding_spacing-sparse.ml.err let_binding_spacing-sparse.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing.ml.stdout
   (with-stderr-to let_binding_spacing.ml.stderr
     (run %{bin:ocamlformat} --name let_binding_spacing.ml --margin-check --let-binding-spacing=compact %{dep:../tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding_spacing.ml.ref let_binding_spacing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_binding_spacing.ml.err let_binding_spacing.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_in_constr.ml.stdout
   (with-stderr-to let_in_constr.ml.stderr
     (run %{bin:ocamlformat} --name let_in_constr.ml --margin-check %{dep:../tests/let_in_constr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_in_constr.ml.ref let_in_constr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_in_constr.ml.err let_in_constr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_module-sparse.ml.stdout
   (with-stderr-to let_module-sparse.ml.stderr
     (run %{bin:ocamlformat} --name let_module-sparse.ml --margin-check --let-module=sparse %{dep:../tests/let_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_module-sparse.ml.ref let_module-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_module-sparse.ml.err let_module-sparse.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_module.ml.stdout
   (with-stderr-to let_module.ml.stderr
     (run %{bin:ocamlformat} --name let_module.ml --margin-check --let-module=compact %{dep:../tests/let_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_module.ml.ref let_module.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_module.ml.err let_module.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to let_punning.ml.stdout
   (with-stderr-to let_punning.ml.stderr
     (run %{bin:ocamlformat} --name let_punning.ml --margin-check %{dep:../tests/let_punning.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_punning.ml.ref let_punning.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff let_punning.ml.err let_punning.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to line_directives.ml.stdout
   (with-stderr-to line_directives.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --name line_directives.ml --margin-check %{dep:../tests/line_directives.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff line_directives.ml.ref line_directives.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff line_directives.ml.err line_directives.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to list-space_around.ml.stdout
   (with-stderr-to list-space_around.ml.stderr
     (run %{bin:ocamlformat} --name list-space_around.ml --margin-check --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:../tests/list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list-space_around.ml.ref list-space_around.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list-space_around.ml.err list-space_around.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to list.ml.stdout
   (with-stderr-to list.ml.stderr
     (run %{bin:ocamlformat} --name list.ml --margin-check %{dep:../tests/list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list.ml.ref list.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list.ml.err list.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to list_and_comments.ml.stdout
   (with-stderr-to list_and_comments.ml.stderr
     (run %{bin:ocamlformat} --name list_and_comments.ml --margin-check %{dep:../tests/list_and_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list_and_comments.ml.ref list_and_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list_and_comments.ml.err list_and_comments.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to list_normalized.ml.stdout
   (with-stderr-to list_normalized.ml.stderr
     (run %{bin:ocamlformat} --name list_normalized.ml --margin-check --max-iters=4 %{dep:../tests/list_normalized.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list_normalized.ml.ref list_normalized.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff list_normalized.ml.err list_normalized.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to loc_stack.ml.stdout
   (with-stderr-to loc_stack.ml.stderr
     (run %{bin:ocamlformat} --name loc_stack.ml --margin-check -n 3 %{dep:../tests/loc_stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff loc_stack.ml.ref loc_stack.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff loc_stack.ml.err loc_stack.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to locally_abtract_types.ml.stdout
   (with-stderr-to locally_abtract_types.ml.stderr
     (run %{bin:ocamlformat} --name locally_abtract_types.ml --margin-check %{dep:../tests/locally_abtract_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff locally_abtract_types.ml.ref locally_abtract_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff locally_abtract_types.ml.err locally_abtract_types.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to margin_80.ml.stdout
   (with-stderr-to margin_80.ml.stderr
     (run %{bin:ocamlformat} --name margin_80.ml --margin-check --margin=80 %{dep:../tests/margin_80.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff margin_80.ml.ref margin_80.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff margin_80.ml.err margin_80.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to match.ml.stdout
   (with-stderr-to match.ml.stderr
     (run %{bin:ocamlformat} --name match.ml --margin-check %{dep:../tests/match.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match.ml.ref match.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match.ml.err match.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to match2.ml.stdout
   (with-stderr-to match2.ml.stderr
     (run %{bin:ocamlformat} --name match2.ml --margin-check --leading-nested-match-parens %{dep:../tests/match2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match2.ml.ref match2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match2.ml.err match2.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to match_indent-never.ml.stdout
   (with-stderr-to match_indent-never.ml.stderr
     (run %{bin:ocamlformat} --name match_indent-never.ml --margin-check --match-indent=4 --match-indent-nested=never %{dep:../tests/match_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match_indent-never.ml.ref match_indent-never.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match_indent-never.ml.err match_indent-never.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to match_indent.ml.stdout
   (with-stderr-to match_indent.ml.stderr
     (run %{bin:ocamlformat} --name match_indent.ml --margin-check --match-indent=4 --match-indent-nested=always %{dep:../tests/match_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match_indent.ml.ref match_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff match_indent.ml.err match_indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to max_indent.ml.stdout
   (with-stderr-to max_indent.ml.stderr
     (run %{bin:ocamlformat} --name max_indent.ml --margin-check --max-indent=2 %{dep:../tests/max_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff max_indent.ml.ref max_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff max_indent.ml.err max_indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to mod_type_subst.ml.stdout
   (with-stderr-to mod_type_subst.ml.stderr
     (run %{bin:ocamlformat} --name mod_type_subst.ml --margin-check %{dep:../tests/mod_type_subst.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff mod_type_subst.ml.ref mod_type_subst.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff mod_type_subst.ml.err mod_type_subst.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module.ml.stdout
   (with-stderr-to module.ml.stderr
     (run %{bin:ocamlformat} --name module.ml --margin-check %{dep:../tests/module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module.ml.ref module.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module.ml.err module.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_anonymous.ml.stdout
   (with-stderr-to module_anonymous.ml.stderr
     (run %{bin:ocamlformat} --name module_anonymous.ml --margin-check %{dep:../tests/module_anonymous.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_anonymous.ml.ref module_anonymous.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_anonymous.ml.err module_anonymous.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_attributes.ml.stdout
   (with-stderr-to module_attributes.ml.stderr
     (run %{bin:ocamlformat} --name module_attributes.ml --margin-check %{dep:../tests/module_attributes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_attributes.ml.ref module_attributes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_attributes.ml.err module_attributes.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing-preserve.ml.stdout
   (with-stderr-to module_item_spacing-preserve.ml.stderr
     (run %{bin:ocamlformat} --name module_item_spacing-preserve.ml --margin-check --max-iter=3 --module-item-spacing=preserve %{dep:../tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing-preserve.ml.ref module_item_spacing-preserve.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing-preserve.ml.err module_item_spacing-preserve.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing-sparse.ml.stdout
   (with-stderr-to module_item_spacing-sparse.ml.stderr
     (run %{bin:ocamlformat} --name module_item_spacing-sparse.ml --margin-check --max-iter=3 --module-item-spacing=sparse %{dep:../tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing-sparse.ml.ref module_item_spacing-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing-sparse.ml.err module_item_spacing-sparse.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing.ml.stdout
   (with-stderr-to module_item_spacing.ml.stderr
     (run %{bin:ocamlformat} --name module_item_spacing.ml --margin-check --max-iter=3 --module-item-spacing=compact %{dep:../tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing.ml.ref module_item_spacing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing.ml.err module_item_spacing.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing.mli.stdout
   (with-stderr-to module_item_spacing.mli.stderr
     (run %{bin:ocamlformat} --name module_item_spacing.mli --margin-check --max-iter=3 %{dep:../tests/module_item_spacing.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing.mli.ref module_item_spacing.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_item_spacing.mli.err module_item_spacing.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_type.ml.stdout
   (with-stderr-to module_type.ml.stderr
     (run %{bin:ocamlformat} --name module_type.ml --margin-check %{dep:../tests/module_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_type.ml.ref module_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_type.ml.err module_type.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to module_type.mli.stdout
   (with-stderr-to module_type.mli.stderr
     (run %{bin:ocamlformat} --name module_type.mli --margin-check %{dep:../tests/module_type.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_type.mli.ref module_type.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff module_type.mli.err module_type.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to monadic_binding.ml.stdout
   (with-stderr-to monadic_binding.ml.stderr
     (run %{bin:ocamlformat} --name monadic_binding.ml --margin-check %{dep:../tests/monadic_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff monadic_binding.ml.ref monadic_binding.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff monadic_binding.ml.err monadic_binding.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to multi_index_op.ml.stdout
   (with-stderr-to multi_index_op.ml.stderr
     (run %{bin:ocamlformat} --name multi_index_op.ml --margin-check %{dep:../tests/multi_index_op.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff multi_index_op.ml.ref multi_index_op.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff multi_index_op.ml.err multi_index_op.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to named_existentials.ml.stdout
   (with-stderr-to named_existentials.ml.stderr
     (run %{bin:ocamlformat} --name named_existentials.ml --margin-check %{dep:../tests/named_existentials.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff named_existentials.ml.ref named_existentials.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff named_existentials.ml.err named_existentials.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to need_format.ml.stdout
   (with-stderr-to need_format.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --name need_format.ml --margin-check --max-iters=1 %{dep:../tests/need_format.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff need_format.ml.ref need_format.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff need_format.ml.err need_format.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to new.ml.stdout
   (with-stderr-to new.ml.stderr
     (run %{bin:ocamlformat} --name new.ml --margin-check %{dep:../tests/new.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff new.ml.ref new.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff new.ml.err new.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to newlines.ml.stdout
   (with-stderr-to newlines.ml.stderr
     (run %{bin:ocamlformat} --name newlines.ml --margin-check %{dep:../tests/newlines.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff newlines.ml.ref newlines.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff newlines.ml.err newlines.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to object.ml.stdout
   (with-stderr-to object.ml.stderr
     (run %{bin:ocamlformat} --name object.ml --margin-check %{dep:../tests/object.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object.ml.ref object.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object.ml.err object.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to object2.ml.stdout
   (with-stderr-to object2.ml.stderr
     (run %{bin:ocamlformat} --name object2.ml --margin-check %{dep:../tests/object2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object2.ml.ref object2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object2.ml.err object2.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to object_expr-414.ml.stdout
   (with-stderr-to object_expr-414.ml.stderr
     (run %{bin:ocamlformat} --name object_expr-414.ml --margin-check --ocaml-version=4.14.0 %{dep:../tests/object_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object_expr-414.ml.ref object_expr-414.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object_expr-414.ml.err object_expr-414.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to object_expr.ml.stdout
   (with-stderr-to object_expr.ml.stderr
     (run %{bin:ocamlformat} --name object_expr.ml --margin-check %{dep:../tests/object_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object_expr.ml.ref object_expr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object_expr.ml.err object_expr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to object_type.ml.stdout
   (with-stderr-to object_type.ml.stderr
     (run %{bin:ocamlformat} --name object_type.ml --margin-check %{dep:../tests/object_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object_type.ml.ref object_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff object_type.ml.err object_type.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to obuild.ml.stdout
   (with-stderr-to obuild.ml.stderr
     (run %{bin:ocamlformat} --name obuild.ml --margin-check %{dep:../tests/obuild.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff obuild.ml.ref obuild.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff obuild.ml.err obuild.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_compat-break_colon_after.ml.stdout
   (with-stderr-to ocp_indent_compat-break_colon_after.ml.stderr
     (run %{bin:ocamlformat} --name ocp_indent_compat-break_colon_after.ml --margin-check --ocp-indent-compat --break-colon=after %{dep:../tests/ocp_indent_compat.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ocp_indent_compat-break_colon_after.ml.ref ocp_indent_compat-break_colon_after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ocp_indent_compat-break_colon_after.ml.err ocp_indent_compat-break_colon_after.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_compat.ml.stdout
   (with-stderr-to ocp_indent_compat.ml.stderr
     (run %{bin:ocamlformat} --name ocp_indent_compat.ml --margin-check --ocp-indent-compat --break-colon=before %{dep:../tests/ocp_indent_compat.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ocp_indent_compat.ml.ref ocp_indent_compat.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ocp_indent_compat.ml.err ocp_indent_compat.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_options.ml.stdout
   (with-stderr-to ocp_indent_options.ml.stderr
     (run %{bin:ocamlformat} --name ocp_indent_options.ml --margin-check --ocp-indent-config %{dep:../tests/ocp_indent_options.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ocp_indent_options.ml.ref ocp_indent_options.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff ocp_indent_options.ml.err ocp_indent_options.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to odoc.mli.stdout
   (with-stderr-to odoc.mli.stderr
     (run %{bin:ocamlformat} --name odoc.mli --margin-check --parse-docstrings %{dep:../tests/odoc.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff odoc.mli.ref odoc.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff odoc.mli.err odoc.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to open-closing-on-separate-line.ml.stdout
   (with-stderr-to open-closing-on-separate-line.ml.stderr
     (run %{bin:ocamlformat} --name open-closing-on-separate-line.ml --margin-check --indicate-multiline-delimiters=closing-on-separate-line %{dep:../tests/open.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff open-closing-on-separate-line.ml.ref open-closing-on-separate-line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff open-closing-on-separate-line.ml.err open-closing-on-separate-line.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to open.ml.stdout
   (with-stderr-to open.ml.stderr
     (run %{bin:ocamlformat} --name open.ml --margin-check %{dep:../tests/open.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff open.ml.ref open.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff open.ml.err open.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to open_types.ml.stdout
   (with-stderr-to open_types.ml.stderr
     (run %{bin:ocamlformat} --name open_types.ml --margin-check %{dep:../tests/open_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff open_types.ml.ref open_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff open_types.ml.err open_types.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to option.ml.stdout
   (with-stderr-to option.ml.stderr
     (run %{bin:ocamlformat} --name option.ml --margin-check %{dep:../tests/option.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff option.ml.ref option.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff option.ml.err option.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to override.ml.stdout
   (with-stderr-to override.ml.stderr
     (run %{bin:ocamlformat} --name override.ml --margin-check %{dep:../tests/override.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff override.ml.ref override.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff override.ml.err override.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to parens_tuple_patterns.ml.stdout
   (with-stderr-to parens_tuple_patterns.ml.stderr
     (run %{bin:ocamlformat} --name parens_tuple_patterns.ml --margin-check %{dep:../tests/parens_tuple_patterns.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff parens_tuple_patterns.ml.ref parens_tuple_patterns.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff parens_tuple_patterns.ml.err parens_tuple_patterns.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to polytypes.ml.stdout
   (with-stderr-to polytypes.ml.stderr
     (run %{bin:ocamlformat} --name polytypes.ml --margin-check %{dep:../tests/polytypes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff polytypes.ml.ref polytypes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff polytypes.ml.err polytypes.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to pre42_syntax.ml.stdout
   (with-stderr-to pre42_syntax.ml.stderr
     (run %{bin:ocamlformat} --name pre42_syntax.ml --margin-check --ocaml-version=4.1 %{dep:../tests/pre42_syntax.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff pre42_syntax.ml.ref pre42_syntax.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff pre42_syntax.ml.err pre42_syntax.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to pre53_syntax.ml.stdout
   (with-stderr-to pre53_syntax.ml.stderr
     (run %{bin:ocamlformat} --name pre53_syntax.ml --margin-check --ocaml-version=5.2 %{dep:../tests/pre53_syntax.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff pre53_syntax.ml.ref pre53_syntax.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff pre53_syntax.ml.err pre53_syntax.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to pre_post_extensions.ml.stdout
   (with-stderr-to pre_post_extensions.ml.stderr
     (run %{bin:ocamlformat} --name pre_post_extensions.ml --margin-check %{dep:../tests/pre_post_extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff pre_post_extensions.ml.ref pre_post_extensions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff pre_post_extensions.ml.err pre_post_extensions.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to precedence.ml.stdout
   (with-stderr-to precedence.ml.stderr
     (run %{bin:ocamlformat} --name precedence.ml --margin-check %{dep:../tests/precedence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff precedence.ml.ref precedence.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff precedence.ml.err precedence.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to prefix_infix.ml.stdout
   (with-stderr-to prefix_infix.ml.stderr
     (run %{bin:ocamlformat} --name prefix_infix.ml --margin-check %{dep:../tests/prefix_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff prefix_infix.ml.ref prefix_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff prefix_infix.ml.err prefix_infix.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to profiles.ml.stdout
   (with-stderr-to profiles.ml.stderr
     (run %{bin:ocamlformat} --name profiles.ml --margin-check --config=margin=20 --module-item-spacing=sparse %{dep:../tests/profiles.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff profiles.ml.ref profiles.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff profiles.ml.err profiles.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to profiles2.ml.stdout
   (with-stderr-to profiles2.ml.stderr
     (run %{bin:ocamlformat} --name profiles2.ml --margin-check %{dep:../tests/profiles2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff profiles2.ml.ref profiles2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff profiles2.ml.err profiles2.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to protected_object_types.ml.stdout
   (with-stderr-to protected_object_types.ml.stderr
     (run %{bin:ocamlformat} --name protected_object_types.ml --margin-check %{dep:../tests/protected_object_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff protected_object_types.ml.ref protected_object_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff protected_object_types.ml.err protected_object_types.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to qtest.ml.stdout
   (with-stderr-to qtest.ml.stderr
     (run %{bin:ocamlformat} --name qtest.ml --margin-check %{dep:../tests/qtest.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff qtest.ml.ref qtest.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff qtest.ml.err qtest.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to quoted_strings.ml.stdout
   (with-stderr-to quoted_strings.ml.stderr
     (run %{bin:ocamlformat} --name quoted_strings.ml --margin-check %{dep:../tests/quoted_strings.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff quoted_strings.ml.ref quoted_strings.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff quoted_strings.ml.err quoted_strings.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to raw_identifiers.ml.stdout
   (with-stderr-to raw_identifiers.ml.stderr
     (run %{bin:ocamlformat} --name raw_identifiers.ml --margin-check %{dep:../tests/raw_identifiers.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff raw_identifiers.ml.ref raw_identifiers.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff raw_identifiers.ml.err raw_identifiers.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to recmod.mli.stdout
   (with-stderr-to recmod.mli.stderr
     (run %{bin:ocamlformat} --name recmod.mli --margin-check %{dep:../tests/recmod.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff recmod.mli.ref recmod.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff recmod.mli.err recmod.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to record-402.ml.stdout
   (with-stderr-to record-402.ml.stderr
     (run %{bin:ocamlformat} --name record-402.ml --margin-check --ocaml-version=4.02 %{dep:../tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record-402.ml.ref record-402.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record-402.ml.err record-402.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to record-loose.ml.stdout
   (with-stderr-to record-loose.ml.stderr
     (run %{bin:ocamlformat} --name record-loose.ml --margin-check --field-space=loose %{dep:../tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record-loose.ml.ref record-loose.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record-loose.ml.err record-loose.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to record-tight_decl.ml.stdout
   (with-stderr-to record-tight_decl.ml.stderr
     (run %{bin:ocamlformat} --name record-tight_decl.ml --margin-check --field-space=tight-decl %{dep:../tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record-tight_decl.ml.ref record-tight_decl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record-tight_decl.ml.err record-tight_decl.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to record.ml.stdout
   (with-stderr-to record.ml.stderr
     (run %{bin:ocamlformat} --name record.ml --margin-check --field-space=tight %{dep:../tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record.ml.ref record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record.ml.err record.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to record_punning.ml.stdout
   (with-stderr-to record_punning.ml.stderr
     (run %{bin:ocamlformat} --name record_punning.ml --margin-check %{dep:../tests/record_punning.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record_punning.ml.ref record_punning.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff record_punning.ml.err record_punning.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to reformat_string.ml.stdout
   (with-stderr-to reformat_string.ml.stderr
     (run %{bin:ocamlformat} --name reformat_string.ml --margin-check --max-iter=3 %{dep:../tests/reformat_string.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff reformat_string.ml.ref reformat_string.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff reformat_string.ml.err reformat_string.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to refs.ml.stdout
   (with-stderr-to refs.ml.stderr
     (run %{bin:ocamlformat} --name refs.ml --margin-check %{dep:../tests/refs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff refs.ml.ref refs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff refs.ml.err refs.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to remove_extra_parens.ml.stdout
   (with-stderr-to remove_extra_parens.ml.stderr
     (run %{bin:ocamlformat} --name remove_extra_parens.ml --margin-check %{dep:../tests/remove_extra_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff remove_extra_parens.ml.ref remove_extra_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff remove_extra_parens.ml.err remove_extra_parens.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to repl.ml.stdout
   (with-stderr-to repl.ml.stderr
     (run %{bin:ocamlformat} --name repl.ml --margin-check --parse-toplevel-phrases --repl-file %{dep:../tests/repl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff repl.ml.ref repl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff repl.ml.err repl.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to repl.mli.stdout
   (with-stderr-to repl.mli.stderr
     (run %{bin:ocamlformat} --name repl.mli --margin-check --parse-toplevel-phrases %{dep:../tests/repl.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff repl.mli.ref repl.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff repl.mli.err repl.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to revapply_ext.ml.stdout
   (with-stderr-to revapply_ext.ml.stderr
     (run %{bin:ocamlformat} --name revapply_ext.ml --margin-check %{dep:../tests/revapply_ext.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff revapply_ext.ml.ref revapply_ext.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff revapply_ext.ml.err revapply_ext.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to send.ml.stdout
   (with-stderr-to send.ml.stderr
     (run %{bin:ocamlformat} --name send.ml --margin-check %{dep:../tests/send.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff send.ml.ref send.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff send.ml.err send.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to sequence-preserve.ml.stdout
   (with-stderr-to sequence-preserve.ml.stderr
     (run %{bin:ocamlformat} --name sequence-preserve.ml --margin-check --sequence-blank-line=preserve-one --max-iter=3 %{dep:../tests/sequence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff sequence-preserve.ml.ref sequence-preserve.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff sequence-preserve.ml.err sequence-preserve.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to sequence.ml.stdout
   (with-stderr-to sequence.ml.stderr
     (run %{bin:ocamlformat} --name sequence.ml --margin-check --sequence-blank-line=compact %{dep:../tests/sequence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff sequence.ml.ref sequence.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff sequence.ml.err sequence.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to shebang.ml.stdout
   (with-stderr-to shebang.ml.stderr
     (run %{bin:ocamlformat} --name shebang.ml --margin-check %{dep:../tests/shebang.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff shebang.ml.ref shebang.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff shebang.ml.err shebang.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to shortcut_ext_attr.ml.stdout
   (with-stderr-to shortcut_ext_attr.ml.stderr
     (run %{bin:ocamlformat} --name shortcut_ext_attr.ml --margin-check %{dep:../tests/shortcut_ext_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff shortcut_ext_attr.ml.ref shortcut_ext_attr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff shortcut_ext_attr.ml.err shortcut_ext_attr.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to sig_value.mli.stdout
   (with-stderr-to sig_value.mli.stderr
     (run %{bin:ocamlformat} --name sig_value.mli --margin-check %{dep:../tests/sig_value.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff sig_value.mli.ref sig_value.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff sig_value.mli.err sig_value.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to single_line.mli.stdout
   (with-stderr-to single_line.mli.stderr
     (run %{bin:ocamlformat} --name single_line.mli --margin-check %{dep:../tests/single_line.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff single_line.mli.ref single_line.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff single_line.mli.err single_line.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to skip.ml.stdout
   (with-stderr-to skip.ml.stderr
     (run %{bin:ocamlformat} --name skip.ml --margin-check %{dep:../tests/skip.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff skip.ml.ref skip.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff skip.ml.err skip.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to source.ml.stdout
   (with-stderr-to source.ml.stderr
     (run %{bin:ocamlformat} --name source.ml --margin-check --max-iters=3 --ocaml-version=5.3 %{dep:../tests/source.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff source.ml.ref source.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff source.ml.err source.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to str_value.ml.stdout
   (with-stderr-to str_value.ml.stderr
     (run %{bin:ocamlformat} --name str_value.ml --margin-check %{dep:../tests/str_value.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff str_value.ml.ref str_value.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff str_value.ml.err str_value.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to string.ml.stdout
   (with-stderr-to string.ml.stderr
     (run %{bin:ocamlformat} --name string.ml --margin-check %{dep:../tests/string.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff string.ml.ref string.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff string.ml.err string.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to string_array.ml.stdout
   (with-stderr-to string_array.ml.stderr
     (run %{bin:ocamlformat} --name string_array.ml --margin-check %{dep:../tests/string_array.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff string_array.ml.ref string_array.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff string_array.ml.err string_array.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to string_wrapping.ml.stdout
   (with-stderr-to string_wrapping.ml.stderr
     (run %{bin:ocamlformat} --name string_wrapping.ml --margin-check %{dep:../tests/string_wrapping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff string_wrapping.ml.ref string_wrapping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff string_wrapping.ml.err string_wrapping.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to symbol.ml.stdout
   (with-stderr-to symbol.ml.stderr
     (run %{bin:ocamlformat} --name symbol.ml --margin-check %{dep:../tests/symbol.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff symbol.ml.ref symbol.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff symbol.ml.err symbol.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to tag_only.ml.stdout
   (with-stderr-to tag_only.ml.stderr
     (run %{bin:ocamlformat} --name tag_only.ml --margin-check %{dep:../tests/tag_only.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tag_only.ml.ref tag_only.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tag_only.ml.err tag_only.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to tag_only.mli.stdout
   (with-stderr-to tag_only.mli.stderr
     (run %{bin:ocamlformat} --name tag_only.mli --margin-check %{dep:../tests/tag_only.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tag_only.mli.ref tag_only.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tag_only.mli.err tag_only.mli.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to try_with_or_pattern.ml.stdout
   (with-stderr-to try_with_or_pattern.ml.stderr
     (run %{bin:ocamlformat} --name try_with_or_pattern.ml --margin-check %{dep:../tests/try_with_or_pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff try_with_or_pattern.ml.ref try_with_or_pattern.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff try_with_or_pattern.ml.err try_with_or_pattern.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to tuple.ml.stdout
   (with-stderr-to tuple.ml.stderr
     (run %{bin:ocamlformat} --name tuple.ml --margin-check --parens-tuple=always %{dep:../tests/tuple.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tuple.ml.ref tuple.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tuple.ml.err tuple.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to tuple_less_parens.ml.stdout
   (with-stderr-to tuple_less_parens.ml.stderr
     (run %{bin:ocamlformat} --name tuple_less_parens.ml --margin-check --parens-tuple=multi-line-only %{dep:../tests/tuple_less_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tuple_less_parens.ml.ref tuple_less_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tuple_less_parens.ml.err tuple_less_parens.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to tuple_type_parens.ml.stdout
   (with-stderr-to tuple_type_parens.ml.stderr
     (run %{bin:ocamlformat} --name tuple_type_parens.ml --margin-check %{dep:../tests/tuple_type_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tuple_type_parens.ml.ref tuple_type_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tuple_type_parens.ml.err tuple_type_parens.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to type_and_constraint.ml.stdout
   (with-stderr-to type_and_constraint.ml.stderr
     (run %{bin:ocamlformat} --name type_and_constraint.ml --margin-check %{dep:../tests/type_and_constraint.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff type_and_constraint.ml.ref type_and_constraint.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff type_and_constraint.ml.err type_and_constraint.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to type_annotations.ml.stdout
   (with-stderr-to type_annotations.ml.stderr
     (run %{bin:ocamlformat} --name type_annotations.ml --margin-check %{dep:../tests/type_annotations.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff type_annotations.ml.ref type_annotations.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff type_annotations.ml.err type_annotations.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types-compact-space_around-docked.ml.stdout
   (with-stderr-to types-compact-space_around-docked.ml.stderr
     (run %{bin:ocamlformat} --name types-compact-space_around-docked.ml --margin-check --type-decl=compact --space-around-arrays --space-around-lists --space-around-records --space-around-variants --break-separators=after --dock-collection-brackets %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-compact-space_around-docked.ml.ref types-compact-space_around-docked.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-compact-space_around-docked.ml.err types-compact-space_around-docked.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types-compact-space_around.ml.stdout
   (with-stderr-to types-compact-space_around.ml.stderr
     (run %{bin:ocamlformat} --name types-compact-space_around.ml --margin-check --type-decl=compact --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-compact-space_around.ml.ref types-compact-space_around.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-compact-space_around.ml.err types-compact-space_around.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types-compact.ml.stdout
   (with-stderr-to types-compact.ml.stderr
     (run %{bin:ocamlformat} --name types-compact.ml --margin-check --type-decl=compact %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-compact.ml.ref types-compact.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-compact.ml.err types-compact.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types-indent.ml.stdout
   (with-stderr-to types-indent.ml.stderr
     (run %{bin:ocamlformat} --name types-indent.ml --margin-check --type-decl-indent=6 %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-indent.ml.ref types-indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-indent.ml.err types-indent.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types-sparse-space_around.ml.stdout
   (with-stderr-to types-sparse-space_around.ml.stderr
     (run %{bin:ocamlformat} --name types-sparse-space_around.ml --margin-check --type-decl=sparse --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-sparse-space_around.ml.ref types-sparse-space_around.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-sparse-space_around.ml.err types-sparse-space_around.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types-sparse.ml.stdout
   (with-stderr-to types-sparse.ml.stderr
     (run %{bin:ocamlformat} --name types-sparse.ml --margin-check --type-decl=sparse %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-sparse.ml.ref types-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types-sparse.ml.err types-sparse.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to types.ml.stdout
   (with-stderr-to types.ml.stderr
     (run %{bin:ocamlformat} --name types.ml --margin-check %{dep:../tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types.ml.ref types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff types.ml.err types.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to unary.ml.stdout
   (with-stderr-to unary.ml.stderr
     (run %{bin:ocamlformat} --name unary.ml --margin-check %{dep:../tests/unary.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff unary.ml.ref unary.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff unary.ml.err unary.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to unary_hash.ml.stdout
   (with-stderr-to unary_hash.ml.stderr
     (run %{bin:ocamlformat} --name unary_hash.ml --margin-check %{dep:../tests/unary_hash.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff unary_hash.ml.ref unary_hash.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff unary_hash.ml.err unary_hash.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to unicode.ml.stdout
   (with-stderr-to unicode.ml.stderr
     (run %{bin:ocamlformat} --name unicode.ml --margin-check --margin=80 --wrap-comments %{dep:../tests/unicode.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff unicode.ml.ref unicode.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff unicode.ml.err unicode.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to use_file.mlt.stdout
   (with-stderr-to use_file.mlt.stderr
     (run %{bin:ocamlformat} --name use_file.mlt --margin-check %{dep:../tests/use_file.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff use_file.mlt.ref use_file.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff use_file.mlt.err use_file.mlt.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to utf8_identifiers.ml.stdout
   (with-stderr-to utf8_identifiers.ml.stderr
     (run %{bin:ocamlformat} --name utf8_identifiers.ml --margin-check %{dep:../tests/utf8_identifiers.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff utf8_identifiers.ml.ref utf8_identifiers.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff utf8_identifiers.ml.err utf8_identifiers.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to variants.ml.stdout
   (with-stderr-to variants.ml.stderr
     (run %{bin:ocamlformat} --name variants.ml --margin-check %{dep:../tests/variants.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff variants.ml.ref variants.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff variants.ml.err variants.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to verbatim_comments-wrap.ml.stdout
   (with-stderr-to verbatim_comments-wrap.ml.stderr
     (run %{bin:ocamlformat} --name verbatim_comments-wrap.ml --margin-check --wrap-comments %{dep:../tests/verbatim_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff verbatim_comments-wrap.ml.ref verbatim_comments-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff verbatim_comments-wrap.ml.err verbatim_comments-wrap.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to verbatim_comments.ml.stdout
   (with-stderr-to verbatim_comments.ml.stderr
     (run %{bin:ocamlformat} --name verbatim_comments.ml --margin-check %{dep:../tests/verbatim_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff verbatim_comments.ml.ref verbatim_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff verbatim_comments.ml.err verbatim_comments.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to w50.ml.stdout
   (with-stderr-to w50.ml.stderr
     (run %{bin:ocamlformat} --name w50.ml --margin-check --no-comment-check -q --max-iters=3 %{dep:../tests/w50.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff w50.ml.ref w50.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff w50.ml.err w50.ml.stderr)))

(rule
 (deps .ocamlformat)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to wrap_comments.ml.stdout
   (with-stderr-to wrap_comments.ml.stderr
     (run %{bin:ocamlformat} --name wrap_comments.ml --margin-check --max-iters=3 %{dep:../tests/wrap_comments.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff wrap_comments.ml.ref wrap_comments.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff wrap_comments.ml.err wrap_comments.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to wrap_comments_break.ml.stdout
   (with-stderr-to wrap_comments_break.ml.stderr
     (run %{bin:ocamlformat} --name wrap_comments_break.ml --margin-check --no-wrap-fun-args --margin=67 %{dep:../tests/wrap_comments_break.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff wrap_comments_break.ml.ref wrap_comments_break.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff wrap_comments_break.ml.err wrap_comments_break.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to wrap_invalid_doc_comments.ml.stdout
   (with-stderr-to wrap_invalid_doc_comments.ml.stderr
     (run %{bin:ocamlformat} --name wrap_invalid_doc_comments.ml --margin-check --parse-docstrings --wrap-comments %{dep:../tests/wrap_invalid_doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff wrap_invalid_doc_comments.ml.ref wrap_invalid_doc_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff wrap_invalid_doc_comments.ml.err wrap_invalid_doc_comments.ml.stderr)))

(rule
 (deps .ocamlformat)
 (package ocamlformat)
 (action
  (with-stdout-to wrapping_functor_args.ml.stdout
   (with-stderr-to wrapping_functor_args.ml.stderr
     (run %{bin:ocamlformat} --name wrapping_functor_args.ml --margin-check %{dep:../tests/wrapping_functor_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff wrapping_functor_args.ml.ref wrapping_functor_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff wrapping_functor_args.ml.err wrapping_functor_args.ml.stderr)))
