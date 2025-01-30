
(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to align_infix.ml.stdout
   (with-stderr-to align_infix.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=fit-or-vertical %{dep:tests/align_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/align_infix.ml align_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/align_infix.ml.err align_infix.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to align_infix.ml.js-stdout
   (with-stderr-to align_infix.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/align_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/align_infix.ml.js-ref align_infix.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/align_infix.ml.js-err align_infix.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to alignment.ml.stdout
   (with-stderr-to alignment.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/alignment.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/alignment.ml.ref alignment.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/alignment.ml.err alignment.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to alignment.ml.js-stdout
   (with-stderr-to alignment.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/alignment.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/alignment.ml.js-ref alignment.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/alignment.ml.js-err alignment.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to apply.ml.stdout
   (with-stderr-to apply.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/apply.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply.ml apply.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply.ml.err apply.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to apply.ml.js-stdout
   (with-stderr-to apply.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/apply.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply.ml.js-ref apply.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply.ml.js-err apply.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to apply_functor.ml.stdout
   (with-stderr-to apply_functor.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/apply_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply_functor.ml apply_functor.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply_functor.ml.err apply_functor.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to apply_functor.ml.js-stdout
   (with-stderr-to apply_functor.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/apply_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply_functor.ml.js-ref apply_functor.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/apply_functor.ml.js-err apply_functor.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to args_grouped-conventional.ml.stdout
   (with-stderr-to args_grouped-conventional.ml.stderr
     (run %{bin:ocamlformat} --margin-check --profile=conventional --margin=30 --max-iters=3 %{dep:tests/args_grouped.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/args_grouped-conventional.ml.ref args_grouped-conventional.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/args_grouped-conventional.ml.err args_grouped-conventional.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to args_grouped.ml.stdout
   (with-stderr-to args_grouped.ml.stderr
     (run %{bin:ocamlformat} --margin-check --profile=ocamlformat --margin=100 %{dep:tests/args_grouped.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/args_grouped.ml args_grouped.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/args_grouped.ml.err args_grouped.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to args_grouped.ml.js-stdout
   (with-stderr-to args_grouped.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/args_grouped.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/args_grouped.ml.js-ref args_grouped.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/args_grouped.ml.js-err args_grouped.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to array.ml.stdout
   (with-stderr-to array.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/array.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/array.ml array.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/array.ml.err array.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to array.ml.js-stdout
   (with-stderr-to array.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/array.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/array.ml.js-ref array.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/array.ml.js-err array.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to assignment_operator-op_begin_line.ml.stdout
   (with-stderr-to assignment_operator-op_begin_line.ml.stderr
     (run %{bin:ocamlformat} --margin-check --assignment-operator=begin-line %{dep:tests/assignment_operator.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator-op_begin_line.ml.ref assignment_operator-op_begin_line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator-op_begin_line.ml.err assignment_operator-op_begin_line.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to assignment_operator-op_begin_line.ml.js-stdout
   (with-stderr-to assignment_operator-op_begin_line.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/assignment_operator.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator-op_begin_line.ml.js-ref assignment_operator-op_begin_line.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator-op_begin_line.ml.js-err assignment_operator-op_begin_line.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to assignment_operator.ml.stdout
   (with-stderr-to assignment_operator.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/assignment_operator.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator.ml.ref assignment_operator.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator.ml.err assignment_operator.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to assignment_operator.ml.js-stdout
   (with-stderr-to assignment_operator.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/assignment_operator.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator.ml.js-ref assignment_operator.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/assignment_operator.ml.js-err assignment_operator.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attr_parens.ml.stdout
   (with-stderr-to attr_parens.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/attr_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attr_parens.ml attr_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attr_parens.ml.err attr_parens.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attr_parens.ml.js-stdout
   (with-stderr-to attr_parens.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/attr_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attr_parens.ml.js-ref attr_parens.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attr_parens.ml.js-err attr_parens.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attribute_and_expression.ml.stdout
   (with-stderr-to attribute_and_expression.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/attribute_and_expression.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_and_expression.ml attribute_and_expression.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_and_expression.ml.err attribute_and_expression.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attribute_and_expression.ml.js-stdout
   (with-stderr-to attribute_and_expression.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/attribute_and_expression.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_and_expression.ml.js-ref attribute_and_expression.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_and_expression.ml.js-err attribute_and_expression.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attribute_on_long_line.ml.stdout
   (with-stderr-to attribute_on_long_line.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/attribute_on_long_line.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_on_long_line.ml.ref attribute_on_long_line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_on_long_line.ml.err attribute_on_long_line.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attribute_on_long_line.ml.js-stdout
   (with-stderr-to attribute_on_long_line.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/attribute_on_long_line.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_on_long_line.ml.js-ref attribute_on_long_line.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attribute_on_long_line.ml.js-err attribute_on_long_line.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attributes.ml.stdout
   (with-stderr-to attributes.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/attributes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attributes.ml attributes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attributes.ml.err attributes.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attributes.mli.stdout
   (with-stderr-to attributes.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/attributes.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attributes.mli.ref attributes.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attributes.mli.err attributes.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to attributes.mli.js-stdout
   (with-stderr-to attributes.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/attributes.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attributes.mli.js-ref attributes.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/attributes.mli.js-err attributes.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to binders.ml.stdout
   (with-stderr-to binders.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/binders.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/binders.ml binders.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/binders.ml.err binders.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to binders.ml.js-stdout
   (with-stderr-to binders.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/binders.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/binders.ml.js-ref binders.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/binders.ml.js-err binders.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_before_in-auto.ml.stdout
   (with-stderr-to break_before_in-auto.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-before-in=auto %{dep:tests/break_before_in.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in-auto.ml.ref break_before_in-auto.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in-auto.ml.err break_before_in-auto.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_before_in-auto.ml.js-stdout
   (with-stderr-to break_before_in-auto.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_before_in.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in-auto.ml.js-ref break_before_in-auto.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in-auto.ml.js-err break_before_in-auto.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_before_in.ml.stdout
   (with-stderr-to break_before_in.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-before-in=fit-or-vertical %{dep:tests/break_before_in.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in.ml break_before_in.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in.ml.err break_before_in.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_before_in.ml.js-stdout
   (with-stderr-to break_before_in.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_before_in.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in.ml.js-ref break_before_in.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_before_in.ml.js-err break_before_in.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-align.ml.stdout
   (with-stderr-to break_cases-align.ml.stderr
     (run %{bin:ocamlformat} --margin-check --nested-match=align --break-cases=all %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-align.ml.ref break_cases-align.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-align.ml.err break_cases-align.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-align.ml.js-stdout
   (with-stderr-to break_cases-align.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-align.ml.js-ref break_cases-align.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-align.ml.js-err break_cases-align.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-all.ml.stdout
   (with-stderr-to break_cases-all.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=all %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-all.ml.ref break_cases-all.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-all.ml.err break_cases-all.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-all.ml.js-stdout
   (with-stderr-to break_cases-all.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-all.ml.js-ref break_cases-all.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-all.ml.js-err break_cases-all.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line.ml.stdout
   (with-stderr-to break_cases-closing_on_separate_line.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line.ml.ref break_cases-closing_on_separate_line.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line.ml.err break_cases-closing_on_separate_line.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line.ml.js-stdout
   (with-stderr-to break_cases-closing_on_separate_line.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line.ml.js-ref break_cases-closing_on_separate_line.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line.ml.js-err break_cases-closing_on_separate_line.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stdout
   (with-stderr-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line --leading-nested-match-parens %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line_leading_nested_match_parens.ml.ref break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line_leading_nested_match_parens.ml.err break_cases-closing_on_separate_line_leading_nested_match_parens.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.js-stdout
   (with-stderr-to break_cases-closing_on_separate_line_leading_nested_match_parens.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line_leading_nested_match_parens.ml.js-ref break_cases-closing_on_separate_line_leading_nested_match_parens.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-closing_on_separate_line_leading_nested_match_parens.ml.js-err break_cases-closing_on_separate_line_leading_nested_match_parens.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-cosl_lnmp_cmei.ml.stdout
   (with-stderr-to break_cases-cosl_lnmp_cmei.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=all --indicate-multiline-delimiters=closing-on-separate-line --leading-nested-match-parens --cases-matching-exp-indent=normal %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-cosl_lnmp_cmei.ml.ref break_cases-cosl_lnmp_cmei.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-cosl_lnmp_cmei.ml.err break_cases-cosl_lnmp_cmei.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-cosl_lnmp_cmei.ml.js-stdout
   (with-stderr-to break_cases-cosl_lnmp_cmei.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-cosl_lnmp_cmei.ml.js-ref break_cases-cosl_lnmp_cmei.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-cosl_lnmp_cmei.ml.js-err break_cases-cosl_lnmp_cmei.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-fit_or_vertical.ml.stdout
   (with-stderr-to break_cases-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=fit-or-vertical %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-fit_or_vertical.ml.ref break_cases-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-fit_or_vertical.ml.err break_cases-fit_or_vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-fit_or_vertical.ml.js-stdout
   (with-stderr-to break_cases-fit_or_vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-fit_or_vertical.ml.js-ref break_cases-fit_or_vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-fit_or_vertical.ml.js-err break_cases-fit_or_vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-nested.ml.stdout
   (with-stderr-to break_cases-nested.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=nested %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-nested.ml.ref break_cases-nested.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-nested.ml.err break_cases-nested.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-nested.ml.js-stdout
   (with-stderr-to break_cases-nested.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-nested.ml.js-ref break_cases-nested.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-nested.ml.js-err break_cases-nested.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-normal_indent.ml.stdout
   (with-stderr-to break_cases-normal_indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --cases-matching-exp-indent=normal --break-cases=all %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-normal_indent.ml.ref break_cases-normal_indent.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-normal_indent.ml.err break_cases-normal_indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-normal_indent.ml.js-stdout
   (with-stderr-to break_cases-normal_indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-normal_indent.ml.js-ref break_cases-normal_indent.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-normal_indent.ml.js-err break_cases-normal_indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-toplevel.ml.stdout
   (with-stderr-to break_cases-toplevel.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=toplevel --max-iter=4 %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases-toplevel.ml.ref break_cases-toplevel.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases-toplevel.ml.err break_cases-toplevel.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-toplevel.ml.js-stdout
   (with-stderr-to break_cases-toplevel.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases-toplevel.ml.js-ref break_cases-toplevel.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases-toplevel.ml.js-err break_cases-toplevel.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-vertical.ml.stdout
   (with-stderr-to break_cases-vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=vertical %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-vertical.ml.ref break_cases-vertical.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-vertical.ml.err break_cases-vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to break_cases-vertical.ml.js-stdout
   (with-stderr-to break_cases-vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-vertical.ml.js-ref break_cases-vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/break_cases-vertical.ml.js-err break_cases-vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_cases.ml.stdout
   (with-stderr-to break_cases.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-cases=fit --max-iter=4 %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases.ml.ref break_cases.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases.ml.err break_cases.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_cases.ml.js-stdout
   (with-stderr-to break_cases.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases.ml.js-ref break_cases.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_cases.ml.js-err break_cases.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_collection_expressions-wrap.ml.stdout
   (with-stderr-to break_collection_expressions-wrap.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-collection-expressions=wrap --max-iters=3 %{dep:tests/break_collection_expressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions-wrap.ml.ref break_collection_expressions-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions-wrap.ml.err break_collection_expressions-wrap.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_collection_expressions-wrap.ml.js-stdout
   (with-stderr-to break_collection_expressions-wrap.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_collection_expressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions-wrap.ml.js-ref break_collection_expressions-wrap.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions-wrap.ml.js-err break_collection_expressions-wrap.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_collection_expressions.ml.stdout
   (with-stderr-to break_collection_expressions.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-collection-expressions=fit-or-vertical --max-iters=3 %{dep:tests/break_collection_expressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions.ml.ref break_collection_expressions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions.ml.err break_collection_expressions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_collection_expressions.ml.js-stdout
   (with-stderr-to break_collection_expressions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_collection_expressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions.ml.js-ref break_collection_expressions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_collection_expressions.ml.js-err break_collection_expressions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_colon-before.ml.stdout
   (with-stderr-to break_colon-before.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-colon=before %{dep:tests/break_colon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon-before.ml.ref break_colon-before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon-before.ml.err break_colon-before.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_colon-before.ml.js-stdout
   (with-stderr-to break_colon-before.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_colon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon-before.ml.js-ref break_colon-before.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon-before.ml.js-err break_colon-before.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_colon.ml.stdout
   (with-stderr-to break_colon.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-colon=after %{dep:tests/break_colon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon.ml break_colon.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon.ml.err break_colon.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_colon.ml.js-stdout
   (with-stderr-to break_colon.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_colon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon.ml.js-ref break_colon.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_colon.ml.js-err break_colon.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-fit_or_vertical.ml.stdout
   (with-stderr-to break_fun_decl-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-fun-decl=fit-or-vertical --break-fun-sig=fit-or-vertical %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-fit_or_vertical.ml.ref break_fun_decl-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-fit_or_vertical.ml.err break_fun_decl-fit_or_vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-fit_or_vertical.ml.js-stdout
   (with-stderr-to break_fun_decl-fit_or_vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-fit_or_vertical.ml.js-ref break_fun_decl-fit_or_vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-fit_or_vertical.ml.js-err break_fun_decl-fit_or_vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-smart.ml.stdout
   (with-stderr-to break_fun_decl-smart.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-fun-decl=smart --break-fun-sig=smart %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-smart.ml.ref break_fun_decl-smart.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-smart.ml.err break_fun_decl-smart.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-smart.ml.js-stdout
   (with-stderr-to break_fun_decl-smart.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-smart.ml.js-ref break_fun_decl-smart.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-smart.ml.js-err break_fun_decl-smart.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-wrap.ml.stdout
   (with-stderr-to break_fun_decl-wrap.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-fun-decl=wrap --break-fun-sig=wrap %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-wrap.ml.ref break_fun_decl-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-wrap.ml.err break_fun_decl-wrap.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl-wrap.ml.js-stdout
   (with-stderr-to break_fun_decl-wrap.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-wrap.ml.js-ref break_fun_decl-wrap.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl-wrap.ml.js-err break_fun_decl-wrap.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl.ml.stdout
   (with-stderr-to break_fun_decl.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl.ml break_fun_decl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl.ml.err break_fun_decl.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_fun_decl.ml.js-stdout
   (with-stderr-to break_fun_decl.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl.ml.js-ref break_fun_decl.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_fun_decl.ml.js-err break_fun_decl.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_infix-fit-or-vertical.ml.stdout
   (with-stderr-to break_infix-fit-or-vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=fit-or-vertical %{dep:tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-fit-or-vertical.ml.ref break_infix-fit-or-vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-fit-or-vertical.ml.err break_infix-fit-or-vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_infix-fit-or-vertical.ml.js-stdout
   (with-stderr-to break_infix-fit-or-vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-fit-or-vertical.ml.js-ref break_infix-fit-or-vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-fit-or-vertical.ml.js-err break_infix-fit-or-vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_infix-wrap.ml.stdout
   (with-stderr-to break_infix-wrap.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=wrap %{dep:tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-wrap.ml.ref break_infix-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-wrap.ml.err break_infix-wrap.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_infix-wrap.ml.js-stdout
   (with-stderr-to break_infix-wrap.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-wrap.ml.js-ref break_infix-wrap.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix-wrap.ml.js-err break_infix-wrap.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_infix.ml.stdout
   (with-stderr-to break_infix.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=wrap-or-vertical %{dep:tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix.ml.ref break_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix.ml.err break_infix.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_infix.ml.js-stdout
   (with-stderr-to break_infix.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix.ml.js-ref break_infix.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_infix.ml.js-err break_infix.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_record.ml.stdout
   (with-stderr-to break_record.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/break_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_record.ml break_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_record.ml.err break_record.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_record.ml.js-stdout
   (with-stderr-to break_record.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_record.ml.js-ref break_record.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_record.ml.js-err break_record.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-after.ml.stdout
   (with-stderr-to break_separators-after.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-separators=after --max-iter=3 %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after.ml.ref break_separators-after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after.ml.err break_separators-after.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-after.ml.js-stdout
   (with-stderr-to break_separators-after.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after.ml.js-ref break_separators-after.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after.ml.js-err break_separators-after.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-after_docked.ml.stdout
   (with-stderr-to break_separators-after_docked.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-separators=after --dock-collection-brackets --max-iter=3 %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after_docked.ml.ref break_separators-after_docked.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after_docked.ml.err break_separators-after_docked.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-after_docked.ml.js-stdout
   (with-stderr-to break_separators-after_docked.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after_docked.ml.js-ref break_separators-after_docked.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-after_docked.ml.js-err break_separators-after_docked.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-before_docked.ml.stdout
   (with-stderr-to break_separators-before_docked.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-separators=before --dock-collection-brackets --max-iter=3 %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-before_docked.ml.ref break_separators-before_docked.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-before_docked.ml.err break_separators-before_docked.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators-before_docked.ml.js-stdout
   (with-stderr-to break_separators-before_docked.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-before_docked.ml.js-ref break_separators-before_docked.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators-before_docked.ml.js-err break_separators-before_docked.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators.ml.stdout
   (with-stderr-to break_separators.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-separators=before --max-iter=3 %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators.ml break_separators.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators.ml.err break_separators.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_separators.ml.js-stdout
   (with-stderr-to break_separators.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_separators.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators.ml.js-ref break_separators.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_separators.ml.js-err break_separators.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_sequence_before.ml.stdout
   (with-stderr-to break_sequence_before.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/break_sequence_before.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_sequence_before.ml break_sequence_before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_sequence_before.ml.err break_sequence_before.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_sequence_before.ml.js-stdout
   (with-stderr-to break_sequence_before.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_sequence_before.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_sequence_before.ml.js-ref break_sequence_before.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_sequence_before.ml.js-err break_sequence_before.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_string_literals-never.ml.stdout
   (with-stderr-to break_string_literals-never.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-string-literals=never %{dep:tests/break_string_literals.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals-never.ml.ref break_string_literals-never.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals-never.ml.err break_string_literals-never.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_string_literals-never.ml.js-stdout
   (with-stderr-to break_string_literals-never.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_string_literals.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals-never.ml.js-ref break_string_literals-never.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals-never.ml.js-err break_string_literals-never.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_string_literals.ml.stdout
   (with-stderr-to break_string_literals.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-string-literals=auto %{dep:tests/break_string_literals.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals.ml.ref break_string_literals.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals.ml.err break_string_literals.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_string_literals.ml.js-stdout
   (with-stderr-to break_string_literals.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_string_literals.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals.ml.js-ref break_string_literals.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_string_literals.ml.js-err break_string_literals.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_struct.ml.stdout
   (with-stderr-to break_struct.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/break_struct.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_struct.ml.ref break_struct.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_struct.ml.err break_struct.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to break_struct.ml.js-stdout
   (with-stderr-to break_struct.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/break_struct.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_struct.ml.js-ref break_struct.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/break_struct.ml.js-err break_struct.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to cases_exp_grouping.ml.stdout
   (with-stderr-to cases_exp_grouping.ml.stderr
     (run %{bin:ocamlformat} --margin-check --exp-grouping=preserve %{dep:tests/cases_exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cases_exp_grouping.ml.ref cases_exp_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cases_exp_grouping.ml.err cases_exp_grouping.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to cases_exp_grouping.ml.js-stdout
   (with-stderr-to cases_exp_grouping.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/cases_exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cases_exp_grouping.ml.js-ref cases_exp_grouping.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cases_exp_grouping.ml.js-err cases_exp_grouping.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to cinaps.ml.stdout
   (with-stderr-to cinaps.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/cinaps.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/cinaps.ml.ref cinaps.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/cinaps.ml.err cinaps.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to cinaps.ml.js-stdout
   (with-stderr-to cinaps.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/cinaps.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/cinaps.ml.js-ref cinaps.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/cinaps.ml.js-err cinaps.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_expr.ml.stdout
   (with-stderr-to class_expr.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/class_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_expr.ml class_expr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_expr.ml.err class_expr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_expr.ml.js-stdout
   (with-stderr-to class_expr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/class_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_expr.ml.js-ref class_expr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_expr.ml.js-err class_expr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_sig-after.mli.stdout
   (with-stderr-to class_sig-after.mli.stderr
     (run %{bin:ocamlformat} --margin-check --break-separators=after %{dep:tests/class_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig-after.mli.ref class_sig-after.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig-after.mli.err class_sig-after.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_sig-after.mli.js-stdout
   (with-stderr-to class_sig-after.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/class_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig-after.mli.js-ref class_sig-after.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig-after.mli.js-err class_sig-after.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_sig.mli.stdout
   (with-stderr-to class_sig.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/class_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig.mli.ref class_sig.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig.mli.err class_sig.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_sig.mli.js-stdout
   (with-stderr-to class_sig.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/class_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig.mli.js-ref class_sig.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_sig.mli.js-err class_sig.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_type.ml.stdout
   (with-stderr-to class_type.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=3 %{dep:tests/class_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_type.ml.ref class_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_type.ml.err class_type.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to class_type.ml.js-stdout
   (with-stderr-to class_type.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/class_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_type.ml.js-ref class_type.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/class_type.ml.js-err class_type.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to cmdline_override.ml.stdout
   (with-stderr-to cmdline_override.ml.stderr
     (run %{bin:ocamlformat} --margin-check --config=module-item-spacing=compact --module-item-spacing=sparse %{dep:tests/cmdline_override.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override.ml cmdline_override.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override.ml.err cmdline_override.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to cmdline_override.ml.js-stdout
   (with-stderr-to cmdline_override.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/cmdline_override.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override.ml.js-ref cmdline_override.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override.ml.js-err cmdline_override.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to cmdline_override2.ml.stdout
   (with-stderr-to cmdline_override2.ml.stderr
     (run %{bin:ocamlformat} --margin-check --module-item-spacing=sparse --config=module-item-spacing=compact %{dep:tests/cmdline_override2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override2.ml cmdline_override2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override2.ml.err cmdline_override2.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to cmdline_override2.ml.js-stdout
   (with-stderr-to cmdline_override2.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/cmdline_override2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override2.ml.js-ref cmdline_override2.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/cmdline_override2.ml.js-err cmdline_override2.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to coerce.ml.stdout
   (with-stderr-to coerce.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/coerce.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/coerce.ml coerce.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/coerce.ml.err coerce.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to coerce.ml.js-stdout
   (with-stderr-to coerce.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/coerce.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/coerce.ml.js-ref coerce.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/coerce.ml.js-err coerce.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_breaking.ml.stdout
   (with-stderr-to comment_breaking.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comment_breaking.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_breaking.ml comment_breaking.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_breaking.ml.err comment_breaking.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_breaking.ml.js-stdout
   (with-stderr-to comment_breaking.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comment_breaking.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_breaking.ml.js-ref comment_breaking.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_breaking.ml.js-err comment_breaking.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to comment_header.ml.stdout
   (with-stderr-to comment_header.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comment_header.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/comment_header.ml.ref comment_header.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/comment_header.ml.err comment_header.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to comment_header.ml.js-stdout
   (with-stderr-to comment_header.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comment_header.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/comment_header.ml.js-ref comment_header.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/comment_header.ml.js-err comment_header.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_in_empty.ml.stdout
   (with-stderr-to comment_in_empty.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comment_in_empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_empty.ml comment_in_empty.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_empty.ml.err comment_in_empty.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_in_empty.ml.js-stdout
   (with-stderr-to comment_in_empty.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comment_in_empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_empty.ml.js-ref comment_in_empty.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_empty.ml.js-err comment_in_empty.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_in_modules.ml.stdout
   (with-stderr-to comment_in_modules.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comment_in_modules.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_modules.ml.ref comment_in_modules.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_modules.ml.err comment_in_modules.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_in_modules.ml.js-stdout
   (with-stderr-to comment_in_modules.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comment_in_modules.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_modules.ml.js-ref comment_in_modules.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_in_modules.ml.js-err comment_in_modules.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_last.ml.stdout
   (with-stderr-to comment_last.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comment_last.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_last.ml comment_last.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_last.ml.err comment_last.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_last.ml.js-stdout
   (with-stderr-to comment_last.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comment_last.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_last.ml.js-ref comment_last.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_last.ml.js-err comment_last.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_long_js_only.ml.stdout
   (with-stderr-to comment_long_js_only.ml.stderr
     (run %{bin:ocamlformat} --margin-check --profile=janestreet %{dep:tests/comment_long_js_only.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_long_js_only.ml comment_long_js_only.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_long_js_only.ml.err comment_long_js_only.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_sparse.ml.stdout
   (with-stderr-to comment_sparse.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comment_sparse.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_sparse.ml comment_sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_sparse.ml.err comment_sparse.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comment_sparse.ml.js-stdout
   (with-stderr-to comment_sparse.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comment_sparse.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_sparse.ml.js-ref comment_sparse.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comment_sparse.ml.js-err comment_sparse.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments-no-wrap.ml.stdout
   (with-stderr-to comments-no-wrap.ml.stderr
     (run %{bin:ocamlformat} --margin-check --no-wrap-comments --max-iter=3 %{dep:tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments-no-wrap.ml.ref comments-no-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments-no-wrap.ml.err comments-no-wrap.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments-no-wrap.ml.js-stdout
   (with-stderr-to comments-no-wrap.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments-no-wrap.ml.js-ref comments-no-wrap.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments-no-wrap.ml.js-err comments-no-wrap.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments.ml.stdout
   (with-stderr-to comments.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=4 %{dep:tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.ml.ref comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.ml.err comments.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments.ml.js-stdout
   (with-stderr-to comments.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.ml.js-ref comments.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.ml.js-err comments.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments.mli.stdout
   (with-stderr-to comments.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comments.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.mli comments.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.mli.err comments.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments.mli.js-stdout
   (with-stderr-to comments.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comments.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.mli.js-ref comments.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.mli.js-err comments.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments_args.ml.stdout
   (with-stderr-to comments_args.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=4 %{dep:tests/comments_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_args.ml.ref comments_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_args.ml.err comments_args.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments_args.ml.js-stdout
   (with-stderr-to comments_args.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comments_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_args.ml.js-ref comments_args.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_args.ml.js-err comments_args.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments_around_disabled.ml.stdout
   (with-stderr-to comments_around_disabled.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comments_around_disabled.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_around_disabled.ml.ref comments_around_disabled.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_around_disabled.ml.err comments_around_disabled.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments_around_disabled.ml.js-stdout
   (with-stderr-to comments_around_disabled.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comments_around_disabled.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_around_disabled.ml.js-ref comments_around_disabled.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_around_disabled.ml.js-err comments_around_disabled.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments_in_record.ml.stdout
   (with-stderr-to comments_in_record.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comments_in_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_in_record.ml.ref comments_in_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_in_record.ml.err comments_in_record.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comments_in_record.ml.js-stdout
   (with-stderr-to comments_in_record.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comments_in_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_in_record.ml.js-ref comments_in_record.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments_in_record.ml.js-err comments_in_record.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comprehensions.ml.stdout
   (with-stderr-to comprehensions.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/comprehensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comprehensions.ml comprehensions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comprehensions.ml.err comprehensions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to comprehensions.ml.js-stdout
   (with-stderr-to comprehensions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/comprehensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comprehensions.ml.js-ref comprehensions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comprehensions.ml.js-err comprehensions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to crlf_to_crlf.ml.stdout
   (with-stderr-to crlf_to_crlf.ml.stderr
     (run %{bin:ocamlformat} --margin-check --line-endings=crlf %{dep:tests/crlf_to_crlf.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_crlf.ml.ref crlf_to_crlf.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_crlf.ml.err crlf_to_crlf.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to crlf_to_crlf.ml.js-stdout
   (with-stderr-to crlf_to_crlf.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/crlf_to_crlf.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_crlf.ml.js-ref crlf_to_crlf.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_crlf.ml.js-err crlf_to_crlf.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to crlf_to_lf.ml.stdout
   (with-stderr-to crlf_to_lf.ml.stderr
     (run %{bin:ocamlformat} --margin-check --line-endings=lf %{dep:tests/crlf_to_lf.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_lf.ml.ref crlf_to_lf.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_lf.ml.err crlf_to_lf.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to crlf_to_lf.ml.js-stdout
   (with-stderr-to crlf_to_lf.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/crlf_to_lf.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_lf.ml.js-ref crlf_to_lf.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/crlf_to_lf.ml.js-err crlf_to_lf.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to custom_list.ml.stdout
   (with-stderr-to custom_list.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/custom_list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/custom_list.ml custom_list.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/custom_list.ml.err custom_list.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to custom_list.ml.js-stdout
   (with-stderr-to custom_list.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/custom_list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/custom_list.ml.js-ref custom_list.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/custom_list.ml.js-err custom_list.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to directives.mlt.stdout
   (with-stderr-to directives.mlt.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/directives.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/directives.mlt.ref directives.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/directives.mlt.err directives.mlt.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to directives.mlt.js-stdout
   (with-stderr-to directives.mlt.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/directives.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/directives.mlt.js-ref directives.mlt.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/directives.mlt.js-err directives.mlt.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disable_attr.ml.stdout
   (with-stderr-to disable_attr.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/disable_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_attr.ml disable_attr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_attr.ml.err disable_attr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disable_attr.ml.js-stdout
   (with-stderr-to disable_attr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/disable_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_attr.ml.js-ref disable_attr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_attr.ml.js-err disable_attr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disable_conf_attrs.ml.stdout
   (with-stderr-to disable_conf_attrs.ml.stderr
     (run %{bin:ocamlformat} --margin-check --disable-conf-attrs %{dep:tests/disable_conf_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_conf_attrs.ml.ref disable_conf_attrs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_conf_attrs.ml.err disable_conf_attrs.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disable_conf_attrs.ml.js-stdout
   (with-stderr-to disable_conf_attrs.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/disable_conf_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_conf_attrs.ml.js-ref disable_conf_attrs.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disable_conf_attrs.ml.js-err disable_conf_attrs.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disabled.ml.stdout
   (with-stderr-to disabled.ml.stderr
     (run %{bin:ocamlformat} --margin-check --disable %{dep:tests/disabled.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disabled.ml disabled.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disabled.ml.err disabled.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disabled_attr.ml.stdout
   (with-stderr-to disabled_attr.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/disabled_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disabled_attr.ml disabled_attr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disabled_attr.ml.err disabled_attr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disabled_attr.ml.js-stdout
   (with-stderr-to disabled_attr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/disabled_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disabled_attr.ml.js-ref disabled_attr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disabled_attr.ml.js-err disabled_attr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disambiguate.ml.stdout
   (with-stderr-to disambiguate.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/disambiguate.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguate.ml disambiguate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguate.ml.err disambiguate.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disambiguate.ml.js-stdout
   (with-stderr-to disambiguate.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/disambiguate.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguate.ml.js-ref disambiguate.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguate.ml.js-err disambiguate.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disambiguated_types.ml.stdout
   (with-stderr-to disambiguated_types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/disambiguated_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguated_types.ml disambiguated_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguated_types.ml.err disambiguated_types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to disambiguated_types.ml.js-stdout
   (with-stderr-to disambiguated_types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/disambiguated_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguated_types.ml.js-ref disambiguated_types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/disambiguated_types.ml.js-err disambiguated_types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc.mld.stdout
   (with-stderr-to doc.mld.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/doc.mld})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc.mld.ref doc.mld.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc.mld.err doc.mld.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc.mld.js-stdout
   (with-stderr-to doc.mld.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc.mld})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc.mld.js-ref doc.mld.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc.mld.js-err doc.mld.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-after.ml.stdout
   (with-stderr-to doc_comments-after.ml.stderr
     (run %{bin:ocamlformat} --margin-check --doc-comments=after-when-possible %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-after.ml.ref doc_comments-after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-after.ml.err doc_comments-after.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-after.ml.js-stdout
   (with-stderr-to doc_comments-after.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-after.ml.js-ref doc_comments-after.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-after.ml.js-err doc_comments-after.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-before-except-val.ml.stdout
   (with-stderr-to doc_comments-before-except-val.ml.stderr
     (run %{bin:ocamlformat} --margin-check --doc-comments=before-except-val %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before-except-val.ml.ref doc_comments-before-except-val.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before-except-val.ml.err doc_comments-before-except-val.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-before-except-val.ml.js-stdout
   (with-stderr-to doc_comments-before-except-val.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before-except-val.ml.js-ref doc_comments-before-except-val.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before-except-val.ml.js-err doc_comments-before-except-val.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-before.ml.stdout
   (with-stderr-to doc_comments-before.ml.stderr
     (run %{bin:ocamlformat} --margin-check --doc-comments=before %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before.ml.ref doc_comments-before.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before.ml.err doc_comments-before.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-before.ml.js-stdout
   (with-stderr-to doc_comments-before.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before.ml.js-ref doc_comments-before.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-before.ml.js-err doc_comments-before.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-no-parse-docstrings.mli.stdout
   (with-stderr-to doc_comments-no-parse-docstrings.mli.stderr
     (run %{bin:ocamlformat} --margin-check --no-parse-docstrings --max-iters=3 %{dep:tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-no-parse-docstrings.mli.ref doc_comments-no-parse-docstrings.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-no-parse-docstrings.mli.err doc_comments-no-parse-docstrings.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-no-parse-docstrings.mli.js-stdout
   (with-stderr-to doc_comments-no-parse-docstrings.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-no-parse-docstrings.mli.js-ref doc_comments-no-parse-docstrings.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments-no-parse-docstrings.mli.js-err doc_comments-no-parse-docstrings.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-no-wrap.mli.stdout
   (with-stderr-to doc_comments-no-wrap.mli.stderr
     (run %{bin:ocamlformat} --margin-check --no-wrap-comments %{dep:tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments-no-wrap.mli.ref doc_comments-no-wrap.mli.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments-no-wrap.mli.err doc_comments-no-wrap.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments-no-wrap.mli.js-stdout
   (with-stderr-to doc_comments-no-wrap.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments-no-wrap.mli.js-ref doc_comments-no-wrap.mli.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments-no-wrap.mli.js-err doc_comments-no-wrap.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments.ml.stdout
   (with-stderr-to doc_comments.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments.ml.ref doc_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments.ml.err doc_comments.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments.ml.js-stdout
   (with-stderr-to doc_comments.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments.ml.js-ref doc_comments.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments.ml.js-err doc_comments.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments.mli.stdout
   (with-stderr-to doc_comments.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments.mli.ref doc_comments.mli.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments.mli.err doc_comments.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments.mli.js-stdout
   (with-stderr-to doc_comments.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments.mli.js-ref doc_comments.mli.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/doc_comments.mli.js-err doc_comments.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments_padding.ml.stdout
   (with-stderr-to doc_comments_padding.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/doc_comments_padding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments_padding.ml doc_comments_padding.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments_padding.ml.err doc_comments_padding.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_comments_padding.ml.js-stdout
   (with-stderr-to doc_comments_padding.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_comments_padding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments_padding.ml.js-ref doc_comments_padding.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_comments_padding.ml.js-err doc_comments_padding.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_repl.mld.stdout
   (with-stderr-to doc_repl.mld.stderr
     (run %{bin:ocamlformat} --margin-check --parse-toplevel-phrases %{dep:tests/doc_repl.mld})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_repl.mld.ref doc_repl.mld.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_repl.mld.err doc_repl.mld.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to doc_repl.mld.js-stdout
   (with-stderr-to doc_repl.mld.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/doc_repl.mld})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_repl.mld.js-ref doc_repl.mld.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/doc_repl.mld.js-err doc_repl.mld.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to docstrings_toplevel_directives.mlt.stdout
   (with-stderr-to docstrings_toplevel_directives.mlt.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/docstrings_toplevel_directives.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/docstrings_toplevel_directives.mlt docstrings_toplevel_directives.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/docstrings_toplevel_directives.mlt.err docstrings_toplevel_directives.mlt.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to docstrings_toplevel_directives.mlt.js-stdout
   (with-stderr-to docstrings_toplevel_directives.mlt.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/docstrings_toplevel_directives.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/docstrings_toplevel_directives.mlt.js-ref docstrings_toplevel_directives.mlt.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/docstrings_toplevel_directives.mlt.js-err docstrings_toplevel_directives.mlt.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to dropped_attribute.ml.stdout
   (with-stderr-to dropped_attribute.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/dropped_attribute.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/dropped_attribute.ml.ref dropped_attribute.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/dropped_attribute.ml.err dropped_attribute.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to dropped_attribute.ml.js-stdout
   (with-stderr-to dropped_attribute.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/dropped_attribute.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/dropped_attribute.ml.js-ref dropped_attribute.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/dropped_attribute.ml.js-err dropped_attribute.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to eliom_ext.eliom.stdout
   (with-stderr-to eliom_ext.eliom.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/eliom_ext.eliom})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/eliom_ext.eliom eliom_ext.eliom.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/eliom_ext.eliom.err eliom_ext.eliom.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty.ml.stdout
   (with-stderr-to empty.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty.ml empty.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty.ml.err empty.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty.ml.js-stdout
   (with-stderr-to empty.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty.ml.js-ref empty.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty.ml.js-err empty.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty_ml.ml.stdout
   (with-stderr-to empty_ml.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/empty_ml.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_ml.ml empty_ml.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_ml.ml.err empty_ml.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty_ml.ml.js-stdout
   (with-stderr-to empty_ml.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/empty_ml.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_ml.ml.js-ref empty_ml.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_ml.ml.js-err empty_ml.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty_mli.mli.stdout
   (with-stderr-to empty_mli.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/empty_mli.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mli.mli empty_mli.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mli.mli.err empty_mli.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty_mli.mli.js-stdout
   (with-stderr-to empty_mli.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/empty_mli.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mli.mli.js-ref empty_mli.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mli.mli.js-err empty_mli.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty_mlt.mlt.stdout
   (with-stderr-to empty_mlt.mlt.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/empty_mlt.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mlt.mlt empty_mlt.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mlt.mlt.err empty_mlt.mlt.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to empty_mlt.mlt.js-stdout
   (with-stderr-to empty_mlt.mlt.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/empty_mlt.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mlt.mlt.js-ref empty_mlt.mlt.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/empty_mlt.mlt.js-err empty_mlt.mlt.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error1.ml.stdout
   (with-stderr-to error1.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --margin-check %{dep:tests/error1.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error1.ml.ref error1.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error1.ml.err error1.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error1.ml.js-stdout
   (with-stderr-to error1.ml.js-stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/error1.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error1.ml.js-ref error1.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error1.ml.js-err error1.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error2.ml.stdout
   (with-stderr-to error2.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --margin-check %{dep:tests/error2.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error2.ml.ref error2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error2.ml.err error2.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error2.ml.js-stdout
   (with-stderr-to error2.ml.js-stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/error2.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error2.ml.js-ref error2.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error2.ml.js-err error2.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error3.ml.stdout
   (with-stderr-to error3.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --margin-check %{dep:tests/error3.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error3.ml.ref error3.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error3.ml.err error3.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error3.ml.js-stdout
   (with-stderr-to error3.ml.js-stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/error3.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error3.ml.js-ref error3.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error3.ml.js-err error3.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to error4.ml.stdout
   (with-stderr-to error4.ml.stderr
     (run %{bin:ocamlformat} --margin-check --no-comment-check %{dep:tests/error4.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error4.ml.ref error4.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/error4.ml.err error4.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to escaped_nl.ml.stdout
   (with-stderr-to escaped_nl.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/escaped_nl.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/escaped_nl.ml.ref escaped_nl.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/escaped_nl.ml.err escaped_nl.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to escaped_nl.ml.js-stdout
   (with-stderr-to escaped_nl.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/escaped_nl.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/escaped_nl.ml.js-ref escaped_nl.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/escaped_nl.ml.js-err escaped_nl.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exceptions.ml.stdout
   (with-stderr-to exceptions.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/exceptions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.ml exceptions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.ml.err exceptions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exceptions.ml.js-stdout
   (with-stderr-to exceptions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/exceptions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.ml.js-ref exceptions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.ml.js-err exceptions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exceptions.mli.stdout
   (with-stderr-to exceptions.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/exceptions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.mli exceptions.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.mli.err exceptions.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exceptions.mli.js-stdout
   (with-stderr-to exceptions.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/exceptions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.mli.js-ref exceptions.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exceptions.mli.js-err exceptions.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exp_grouping-parens.ml.stdout
   (with-stderr-to exp_grouping-parens.ml.stderr
     (run %{bin:ocamlformat} --margin-check --exp-grouping=parens %{dep:tests/exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping-parens.ml.ref exp_grouping-parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping-parens.ml.err exp_grouping-parens.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exp_grouping-parens.ml.js-stdout
   (with-stderr-to exp_grouping-parens.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping-parens.ml.js-ref exp_grouping-parens.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping-parens.ml.js-err exp_grouping-parens.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exp_grouping.ml.stdout
   (with-stderr-to exp_grouping.ml.stderr
     (run %{bin:ocamlformat} --margin-check --exp-grouping=preserve %{dep:tests/exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping.ml.ref exp_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping.ml.err exp_grouping.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exp_grouping.ml.js-stdout
   (with-stderr-to exp_grouping.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping.ml.js-ref exp_grouping.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_grouping.ml.js-err exp_grouping.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exp_record.ml.stdout
   (with-stderr-to exp_record.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/exp_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_record.ml exp_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_record.ml.err exp_record.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to exp_record.ml.js-stdout
   (with-stderr-to exp_record.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/exp_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_record.ml.js-ref exp_record.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exp_record.ml.js-err exp_record.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to expect_test.ml.stdout
   (with-stderr-to expect_test.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/expect_test.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/expect_test.ml expect_test.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/expect_test.ml.err expect_test.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to expect_test.ml.js-stdout
   (with-stderr-to expect_test.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/expect_test.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/expect_test.ml.js-ref expect_test.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/expect_test.ml.js-err expect_test.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to explicitly_quantified_value_descriptions.mli.stdout
   (with-stderr-to explicitly_quantified_value_descriptions.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/explicitly_quantified_value_descriptions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/explicitly_quantified_value_descriptions.mli explicitly_quantified_value_descriptions.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/explicitly_quantified_value_descriptions.mli.err explicitly_quantified_value_descriptions.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to explicitly_quantified_value_descriptions.mli.js-stdout
   (with-stderr-to explicitly_quantified_value_descriptions.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/explicitly_quantified_value_descriptions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/explicitly_quantified_value_descriptions.mli.js-ref explicitly_quantified_value_descriptions.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/explicitly_quantified_value_descriptions.mli.js-err explicitly_quantified_value_descriptions.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions-indent.ml.stdout
   (with-stderr-to extensions-indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=3 --extension-indent=5 --stritem-extension-indent=3 %{dep:tests/extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.ml.ref extensions-indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.ml.err extensions-indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions-indent.ml.js-stdout
   (with-stderr-to extensions-indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.ml.js-ref extensions-indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.ml.js-err extensions-indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions-indent.mli.stdout
   (with-stderr-to extensions-indent.mli.stderr
     (run %{bin:ocamlformat} --margin-check --extension-indent=5 --stritem-extension-indent=3 %{dep:tests/extensions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.mli.ref extensions-indent.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.mli.err extensions-indent.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions-indent.mli.js-stdout
   (with-stderr-to extensions-indent.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/extensions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.mli.js-ref extensions-indent.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions-indent.mli.js-err extensions-indent.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions.ml.stdout
   (with-stderr-to extensions.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=3 %{dep:tests/extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.ml.ref extensions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.ml.err extensions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions.ml.js-stdout
   (with-stderr-to extensions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.ml.js-ref extensions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.ml.js-err extensions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions.mli.stdout
   (with-stderr-to extensions.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/extensions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.mli extensions.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.mli.err extensions.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions.mli.js-stdout
   (with-stderr-to extensions.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/extensions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.mli.js-ref extensions.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions.mli.js-err extensions.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions_exp_grouping.ml.stdout
   (with-stderr-to extensions_exp_grouping.ml.stderr
     (run %{bin:ocamlformat} --margin-check --exp-grouping=preserve %{dep:tests/extensions_exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions_exp_grouping.ml.ref extensions_exp_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions_exp_grouping.ml.err extensions_exp_grouping.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to extensions_exp_grouping.ml.js-stdout
   (with-stderr-to extensions_exp_grouping.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/extensions_exp_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions_exp_grouping.ml.js-ref extensions_exp_grouping.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/extensions_exp_grouping.ml.js-err extensions_exp_grouping.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to field-op_begin_line.ml.stdout
   (with-stderr-to field-op_begin_line.ml.stderr
     (run %{bin:ocamlformat} --margin-check --assignment-operator=begin-line %{dep:tests/field.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field-op_begin_line.ml.ref field-op_begin_line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field-op_begin_line.ml.err field-op_begin_line.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to field-op_begin_line.ml.js-stdout
   (with-stderr-to field-op_begin_line.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/field.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field-op_begin_line.ml.js-ref field-op_begin_line.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field-op_begin_line.ml.js-err field-op_begin_line.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to field.ml.stdout
   (with-stderr-to field.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/field.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field.ml field.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field.ml.err field.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to field.ml.js-stdout
   (with-stderr-to field.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/field.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field.ml.js-ref field.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/field.ml.js-err field.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to first_class_module.ml.stdout
   (with-stderr-to first_class_module.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/first_class_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/first_class_module.ml.ref first_class_module.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/first_class_module.ml.err first_class_module.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to first_class_module.ml.js-stdout
   (with-stderr-to first_class_module.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/first_class_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/first_class_module.ml.js-ref first_class_module.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/first_class_module.ml.js-err first_class_module.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to floating_doc.ml.stdout
   (with-stderr-to floating_doc.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/floating_doc.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/floating_doc.ml floating_doc.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/floating_doc.ml.err floating_doc.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to floating_doc.ml.js-stdout
   (with-stderr-to floating_doc.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/floating_doc.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/floating_doc.ml.js-ref floating_doc.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/floating_doc.ml.js-err floating_doc.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to for_while.ml.stdout
   (with-stderr-to for_while.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/for_while.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/for_while.ml for_while.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/for_while.ml.err for_while.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to for_while.ml.js-stdout
   (with-stderr-to for_while.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/for_while.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/for_while.ml.js-ref for_while.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/for_while.ml.js-err for_while.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to fun_decl.ml.stdout
   (with-stderr-to fun_decl.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_decl.ml fun_decl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_decl.ml.err fun_decl.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to fun_decl.ml.js-stdout
   (with-stderr-to fun_decl.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/fun_decl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_decl.ml.js-ref fun_decl.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_decl.ml.js-err fun_decl.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to fun_function.ml.stdout
   (with-stderr-to fun_function.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/fun_function.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_function.ml fun_function.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_function.ml.err fun_function.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to fun_function.ml.js-stdout
   (with-stderr-to fun_function.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/fun_function.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_function.ml.js-ref fun_function.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/fun_function.ml.js-err fun_function.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to function_constraint.ml.stdout
   (with-stderr-to function_constraint.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=5 %{dep:tests/function_constraint.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_constraint.ml.ref function_constraint.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_constraint.ml.err function_constraint.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to function_constraint.ml.js-stdout
   (with-stderr-to function_constraint.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/function_constraint.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_constraint.ml.js-ref function_constraint.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_constraint.ml.js-err function_constraint.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to function_indent-never.ml.stdout
   (with-stderr-to function_indent-never.ml.stderr
     (run %{bin:ocamlformat} --margin-check --function-indent=4 --function-indent-nested=never %{dep:tests/function_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent-never.ml.ref function_indent-never.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent-never.ml.err function_indent-never.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to function_indent-never.ml.js-stdout
   (with-stderr-to function_indent-never.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/function_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent-never.ml.js-ref function_indent-never.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent-never.ml.js-err function_indent-never.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to function_indent.ml.stdout
   (with-stderr-to function_indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --function-indent=4 --function-indent-nested=always %{dep:tests/function_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent.ml.ref function_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent.ml.err function_indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to function_indent.ml.js-stdout
   (with-stderr-to function_indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/function_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent.ml.js-ref function_indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/function_indent.ml.js-err function_indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to functor.ml.stdout
   (with-stderr-to functor.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/functor.ml functor.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/functor.ml.err functor.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to functor.ml.js-stdout
   (with-stderr-to functor.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/functor.ml.js-ref functor.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/functor.ml.js-err functor.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to funsig.ml.stdout
   (with-stderr-to funsig.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/funsig.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/funsig.ml funsig.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/funsig.ml.err funsig.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to funsig.ml.js-stdout
   (with-stderr-to funsig.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/funsig.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/funsig.ml.js-ref funsig.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/funsig.ml.js-err funsig.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to gadt.ml.stdout
   (with-stderr-to gadt.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters 3 %{dep:tests/gadt.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/gadt.ml.ref gadt.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/gadt.ml.err gadt.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to gadt.ml.js-stdout
   (with-stderr-to gadt.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/gadt.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/gadt.ml.js-ref gadt.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/gadt.ml.js-err gadt.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to generative.ml.stdout
   (with-stderr-to generative.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=3 %{dep:tests/generative.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/generative.ml.ref generative.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/generative.ml.err generative.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to generative.ml.js-stdout
   (with-stderr-to generative.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/generative.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/generative.ml.js-ref generative.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/generative.ml.js-err generative.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to hash_bang.ml.stdout
   (with-stderr-to hash_bang.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/hash_bang.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_bang.ml hash_bang.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_bang.ml.err hash_bang.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to hash_bang.ml.js-stdout
   (with-stderr-to hash_bang.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/hash_bang.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_bang.ml.js-ref hash_bang.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_bang.ml.js-err hash_bang.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to hash_types.ml.stdout
   (with-stderr-to hash_types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/hash_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_types.ml hash_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_types.ml.err hash_types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to hash_types.ml.js-stdout
   (with-stderr-to hash_types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/hash_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_types.ml.js-ref hash_types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/hash_types.ml.js-err hash_types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to holes.ml.stdout
   (with-stderr-to holes.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/holes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/holes.ml.ref holes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/holes.ml.err holes.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to holes.ml.js-stdout
   (with-stderr-to holes.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/holes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/holes.ml.js-ref holes.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/holes.ml.js-err holes.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ifand.ml.stdout
   (with-stderr-to ifand.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/ifand.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ifand.ml.ref ifand.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ifand.ml.err ifand.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ifand.ml.js-stdout
   (with-stderr-to ifand.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ifand.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ifand.ml.js-ref ifand.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ifand.ml.js-err ifand.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to immutable_arrays.ml.stdout
   (with-stderr-to immutable_arrays.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/immutable_arrays.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/immutable_arrays.ml immutable_arrays.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/immutable_arrays.ml.err immutable_arrays.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to immutable_arrays.ml.js-stdout
   (with-stderr-to immutable_arrays.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/immutable_arrays.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/immutable_arrays.ml.js-ref immutable_arrays.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/immutable_arrays.ml.js-err immutable_arrays.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to implicit_source_position-erased.ml.stdout
   (with-stderr-to implicit_source_position-erased.ml.stderr
     (run %{bin:ocamlformat} --margin-check --erase-jane-syntax --max-iter=3 %{dep:tests/implicit_source_position.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position-erased.ml.ref implicit_source_position-erased.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position-erased.ml.err implicit_source_position-erased.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to implicit_source_position-erased.ml.js-stdout
   (with-stderr-to implicit_source_position-erased.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/implicit_source_position.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position-erased.ml.js-ref implicit_source_position-erased.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position-erased.ml.js-err implicit_source_position-erased.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to implicit_source_position.ml.stdout
   (with-stderr-to implicit_source_position.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/implicit_source_position.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position.ml.ref implicit_source_position.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position.ml.err implicit_source_position.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to implicit_source_position.ml.js-stdout
   (with-stderr-to implicit_source_position.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/implicit_source_position.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position.ml.js-ref implicit_source_position.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/implicit_source_position.ml.js-err implicit_source_position.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to include_at.ml.stdout
   (with-stderr-to include_at.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=3 %{dep:tests/include_at.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_at.ml.ref include_at.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_at.ml.err include_at.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to include_at.ml.js-stdout
   (with-stderr-to include_at.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/include_at.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_at.ml.js-ref include_at.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_at.ml.js-err include_at.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to include_functor.ml.stdout
   (with-stderr-to include_functor.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/include_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml include_functor.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml.err include_functor.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to include_functor.ml.js-stdout
   (with-stderr-to include_functor.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/include_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml.js-ref include_functor.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml.js-err include_functor.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to index_op.ml.stdout
   (with-stderr-to index_op.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/index_op.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/index_op.ml index_op.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/index_op.ml.err index_op.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to index_op.ml.js-stdout
   (with-stderr-to index_op.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/index_op.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/index_op.ml.js-ref index_op.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/index_op.ml.js-err index_op.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters-cosl.ml.stdout
   (with-stderr-to indicate_multiline_delimiters-cosl.ml.stderr
     (run %{bin:ocamlformat} --margin-check --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-cosl.ml.ref indicate_multiline_delimiters-cosl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-cosl.ml.err indicate_multiline_delimiters-cosl.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters-cosl.ml.js-stdout
   (with-stderr-to indicate_multiline_delimiters-cosl.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-cosl.ml.js-ref indicate_multiline_delimiters-cosl.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-cosl.ml.js-err indicate_multiline_delimiters-cosl.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters-space.ml.stdout
   (with-stderr-to indicate_multiline_delimiters-space.ml.stderr
     (run %{bin:ocamlformat} --margin-check --indicate-multiline-delimiters=space %{dep:tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-space.ml.ref indicate_multiline_delimiters-space.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-space.ml.err indicate_multiline_delimiters-space.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters-space.ml.js-stdout
   (with-stderr-to indicate_multiline_delimiters-space.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-space.ml.js-ref indicate_multiline_delimiters-space.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters-space.ml.js-err indicate_multiline_delimiters-space.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters.ml.stdout
   (with-stderr-to indicate_multiline_delimiters.ml.stderr
     (run %{bin:ocamlformat} --margin-check --indicate-multiline-delimiters=no %{dep:tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters.ml indicate_multiline_delimiters.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters.ml.err indicate_multiline_delimiters.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to indicate_multiline_delimiters.ml.js-stdout
   (with-stderr-to indicate_multiline_delimiters.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/indicate_multiline_delimiters.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters.ml.js-ref indicate_multiline_delimiters.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indicate_multiline_delimiters.ml.js-err indicate_multiline_delimiters.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_arg_grouping.ml.stdout
   (with-stderr-to infix_arg_grouping.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/infix_arg_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_arg_grouping.ml infix_arg_grouping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_arg_grouping.ml.err infix_arg_grouping.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_arg_grouping.ml.js-stdout
   (with-stderr-to infix_arg_grouping.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/infix_arg_grouping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_arg_grouping.ml.js-ref infix_arg_grouping.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_arg_grouping.ml.js-err infix_arg_grouping.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-break.ml.stdout
   (with-stderr-to infix_bind-break.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=wrap --break-infix-before-func %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-break.ml.ref infix_bind-break.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-break.ml.err infix_bind-break.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-break.ml.js-stdout
   (with-stderr-to infix_bind-break.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-break.ml.js-ref infix_bind-break.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-break.ml.js-err infix_bind-break.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-fit_or_vertical-break.ml.stdout
   (with-stderr-to infix_bind-fit_or_vertical-break.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=fit-or-vertical --break-infix-before-func %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical-break.ml.ref infix_bind-fit_or_vertical-break.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical-break.ml.err infix_bind-fit_or_vertical-break.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-fit_or_vertical-break.ml.js-stdout
   (with-stderr-to infix_bind-fit_or_vertical-break.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical-break.ml.js-ref infix_bind-fit_or_vertical-break.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical-break.ml.js-err infix_bind-fit_or_vertical-break.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-fit_or_vertical.ml.stdout
   (with-stderr-to infix_bind-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=fit-or-vertical --no-break-infix-before-func %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical.ml.ref infix_bind-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical.ml.err infix_bind-fit_or_vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind-fit_or_vertical.ml.js-stdout
   (with-stderr-to infix_bind-fit_or_vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical.ml.js-ref infix_bind-fit_or_vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind-fit_or_vertical.ml.js-err infix_bind-fit_or_vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind.ml.stdout
   (with-stderr-to infix_bind.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-infix=wrap --no-break-infix-before-func %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind.ml infix_bind.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind.ml.err infix_bind.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_bind.ml.js-stdout
   (with-stderr-to infix_bind.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/infix_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind.ml.js-ref infix_bind.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_bind.ml.js-err infix_bind.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_precedence.ml.stdout
   (with-stderr-to infix_precedence.ml.stderr
     (run %{bin:ocamlformat} --margin-check --infix-precedence=parens %{dep:tests/infix_precedence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_precedence.ml infix_precedence.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_precedence.ml.err infix_precedence.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to infix_precedence.ml.js-stdout
   (with-stderr-to infix_precedence.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/infix_precedence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_precedence.ml.js-ref infix_precedence.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/infix_precedence.ml.js-err infix_precedence.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to injectivity.ml.stdout
   (with-stderr-to injectivity.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/injectivity.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/injectivity.ml injectivity.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/injectivity.ml.err injectivity.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to injectivity.ml.js-stdout
   (with-stderr-to injectivity.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/injectivity.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/injectivity.ml.js-ref injectivity.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/injectivity.ml.js-err injectivity.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to into_infix.ml.stdout
   (with-stderr-to into_infix.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/into_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/into_infix.ml.ref into_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/into_infix.ml.err into_infix.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to into_infix.ml.js-stdout
   (with-stderr-to into_infix.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/into_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/into_infix.ml.js-ref into_infix.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/into_infix.ml.js-err into_infix.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to invalid.ml.stdout
   (with-stderr-to invalid.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/invalid.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid.ml invalid.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid.ml.err invalid.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to invalid.ml.js-stdout
   (with-stderr-to invalid.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/invalid.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid.ml.js-ref invalid.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid.ml.js-err invalid.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to invalid_docstring.ml.stdout
   (with-stderr-to invalid_docstring.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/invalid_docstring.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid_docstring.ml.ref invalid_docstring.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid_docstring.ml.err invalid_docstring.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to invalid_docstring.ml.js-stdout
   (with-stderr-to invalid_docstring.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/invalid_docstring.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid_docstring.ml.js-ref invalid_docstring.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/invalid_docstring.ml.js-err invalid_docstring.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to invalid_docstrings.mli.stdout
   (with-stderr-to invalid_docstrings.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/invalid_docstrings.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/invalid_docstrings.mli.ref invalid_docstrings.mli.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/invalid_docstrings.mli.err invalid_docstrings.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to invalid_docstrings.mli.js-stdout
   (with-stderr-to invalid_docstrings.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/invalid_docstrings.mli})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/invalid_docstrings.mli.js-ref invalid_docstrings.mli.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/invalid_docstrings.mli.js-err invalid_docstrings.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue114.ml.stdout
   (with-stderr-to issue114.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue114.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue114.ml issue114.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue114.ml.err issue114.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue114.ml.js-stdout
   (with-stderr-to issue114.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue114.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue114.ml.js-ref issue114.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue114.ml.js-err issue114.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue1750.ml.stdout
   (with-stderr-to issue1750.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue1750.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue1750.ml issue1750.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue1750.ml.err issue1750.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue1750.ml.js-stdout
   (with-stderr-to issue1750.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue1750.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue1750.ml.js-ref issue1750.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue1750.ml.js-err issue1750.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue289.ml.stdout
   (with-stderr-to issue289.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue289.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue289.ml issue289.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue289.ml.err issue289.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue289.ml.js-stdout
   (with-stderr-to issue289.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue289.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue289.ml.js-ref issue289.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue289.ml.js-err issue289.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue48.ml.stdout
   (with-stderr-to issue48.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue48.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue48.ml issue48.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue48.ml.err issue48.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue48.ml.js-stdout
   (with-stderr-to issue48.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue48.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue48.ml.js-ref issue48.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue48.ml.js-err issue48.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue51.ml.stdout
   (with-stderr-to issue51.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue51.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue51.ml issue51.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue51.ml.err issue51.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue51.ml.js-stdout
   (with-stderr-to issue51.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue51.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue51.ml.js-ref issue51.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue51.ml.js-err issue51.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue57.ml.stdout
   (with-stderr-to issue57.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue57.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue57.ml issue57.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue57.ml.err issue57.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue57.ml.js-stdout
   (with-stderr-to issue57.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue57.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue57.ml.js-ref issue57.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue57.ml.js-err issue57.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue60.ml.stdout
   (with-stderr-to issue60.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue60.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue60.ml issue60.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue60.ml.err issue60.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue60.ml.js-stdout
   (with-stderr-to issue60.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue60.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue60.ml.js-ref issue60.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue60.ml.js-err issue60.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue77.ml.stdout
   (with-stderr-to issue77.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue77.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue77.ml issue77.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue77.ml.err issue77.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue77.ml.js-stdout
   (with-stderr-to issue77.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue77.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue77.ml.js-ref issue77.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue77.ml.js-err issue77.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue85.ml.stdout
   (with-stderr-to issue85.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue85.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue85.ml issue85.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue85.ml.err issue85.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue85.ml.js-stdout
   (with-stderr-to issue85.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue85.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue85.ml.js-ref issue85.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue85.ml.js-err issue85.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue89.ml.stdout
   (with-stderr-to issue89.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/issue89.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue89.ml issue89.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue89.ml.err issue89.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to issue89.ml.js-stdout
   (with-stderr-to issue89.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/issue89.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue89.ml.js-ref issue89.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/issue89.ml.js-err issue89.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-compact.ml.stdout
   (with-stderr-to ite-compact.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=compact %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact.ml.ref ite-compact.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact.ml.err ite-compact.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-compact.ml.js-stdout
   (with-stderr-to ite-compact.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact.ml.js-ref ite-compact.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact.ml.js-err ite-compact.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-compact_closing.ml.stdout
   (with-stderr-to ite-compact_closing.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=compact --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact_closing.ml.ref ite-compact_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact_closing.ml.err ite-compact_closing.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-compact_closing.ml.js-stdout
   (with-stderr-to ite-compact_closing.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact_closing.ml.js-ref ite-compact_closing.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-compact_closing.ml.js-err ite-compact_closing.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical.ml.stdout
   (with-stderr-to ite-fit_or_vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=fit-or-vertical %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical.ml.ref ite-fit_or_vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical.ml.err ite-fit_or_vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical.ml.js-stdout
   (with-stderr-to ite-fit_or_vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical.ml.js-ref ite-fit_or_vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical.ml.js-err ite-fit_or_vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical_closing.ml.stdout
   (with-stderr-to ite-fit_or_vertical_closing.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else fit-or-vertical --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_closing.ml.ref ite-fit_or_vertical_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_closing.ml.err ite-fit_or_vertical_closing.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical_closing.ml.js-stdout
   (with-stderr-to ite-fit_or_vertical_closing.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_closing.ml.js-ref ite-fit_or_vertical_closing.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_closing.ml.js-err ite-fit_or_vertical_closing.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical_no_indicate.ml.stdout
   (with-stderr-to ite-fit_or_vertical_no_indicate.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=fit-or-vertical --indicate-multiline-delimiters=no %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_no_indicate.ml.ref ite-fit_or_vertical_no_indicate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_no_indicate.ml.err ite-fit_or_vertical_no_indicate.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-fit_or_vertical_no_indicate.ml.js-stdout
   (with-stderr-to ite-fit_or_vertical_no_indicate.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_no_indicate.ml.js-ref ite-fit_or_vertical_no_indicate.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-fit_or_vertical_no_indicate.ml.js-err ite-fit_or_vertical_no_indicate.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kr.ml.stdout
   (with-stderr-to ite-kr.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=k-r --max-iters=3 %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr.ml.ref ite-kr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr.ml.err ite-kr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kr.ml.js-stdout
   (with-stderr-to ite-kr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr.ml.js-ref ite-kr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr.ml.js-err ite-kr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kr_closing.ml.stdout
   (with-stderr-to ite-kr_closing.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=k-r --max-iters=3 --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr_closing.ml.ref ite-kr_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr_closing.ml.err ite-kr_closing.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kr_closing.ml.js-stdout
   (with-stderr-to ite-kr_closing.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr_closing.ml.js-ref ite-kr_closing.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kr_closing.ml.js-err ite-kr_closing.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first.ml.stdout
   (with-stderr-to ite-kw_first.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=keyword-first %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first.ml.ref ite-kw_first.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first.ml.err ite-kw_first.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first.ml.js-stdout
   (with-stderr-to ite-kw_first.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first.ml.js-ref ite-kw_first.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first.ml.js-err ite-kw_first.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first_closing.ml.stdout
   (with-stderr-to ite-kw_first_closing.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else keyword-first --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_closing.ml.ref ite-kw_first_closing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_closing.ml.err ite-kw_first_closing.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first_closing.ml.js-stdout
   (with-stderr-to ite-kw_first_closing.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_closing.ml.js-ref ite-kw_first_closing.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_closing.ml.js-err ite-kw_first_closing.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first_no_indicate.ml.stdout
   (with-stderr-to ite-kw_first_no_indicate.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=keyword-first --indicate-multiline-delimiters=no %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_no_indicate.ml.ref ite-kw_first_no_indicate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_no_indicate.ml.err ite-kw_first_no_indicate.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-kw_first_no_indicate.ml.js-stdout
   (with-stderr-to ite-kw_first_no_indicate.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_no_indicate.ml.js-ref ite-kw_first_no_indicate.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-kw_first_no_indicate.ml.js-err ite-kw_first_no_indicate.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-no_indicate.ml.stdout
   (with-stderr-to ite-no_indicate.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=compact --indicate-multiline-delimiters=no %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-no_indicate.ml.ref ite-no_indicate.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-no_indicate.ml.err ite-no_indicate.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-no_indicate.ml.js-stdout
   (with-stderr-to ite-no_indicate.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-no_indicate.ml.js-ref ite-no_indicate.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-no_indicate.ml.js-err ite-no_indicate.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-vertical.ml.stdout
   (with-stderr-to ite-vertical.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=vertical %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-vertical.ml.ref ite-vertical.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-vertical.ml.err ite-vertical.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite-vertical.ml.js-stdout
   (with-stderr-to ite-vertical.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-vertical.ml.js-ref ite-vertical.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite-vertical.ml.js-err ite-vertical.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite.ml.stdout
   (with-stderr-to ite.ml.stderr
     (run %{bin:ocamlformat} --margin-check --if-then-else=compact %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite.ml.ref ite.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite.ml.err ite.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ite.ml.js-stdout
   (with-stderr-to ite.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite.ml.js-ref ite.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ite.ml.js-err ite.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_args.ml.stdout
   (with-stderr-to js_args.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/js_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_args.ml.ref js_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_args.ml.err js_args.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_args.ml.js-stdout
   (with-stderr-to js_args.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_args.ml.js-ref js_args.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_args.ml.js-err js_args.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_begin.ml.stdout
   (with-stderr-to js_begin.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_begin.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_begin.ml.ref js_begin.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_begin.ml.err js_begin.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_begin.ml.js-stdout
   (with-stderr-to js_begin.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_begin.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_begin.ml.js-ref js_begin.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_begin.ml.js-err js_begin.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_bind.ml.stdout
   (with-stderr-to js_bind.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_bind.ml.ref js_bind.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_bind.ml.err js_bind.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_bind.ml.js-stdout
   (with-stderr-to js_bind.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_bind.ml.js-ref js_bind.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_bind.ml.js-err js_bind.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_fun.ml.stdout
   (with-stderr-to js_fun.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/js_fun.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_fun.ml.ref js_fun.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_fun.ml.err js_fun.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_fun.ml.js-stdout
   (with-stderr-to js_fun.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_fun.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_fun.ml.js-ref js_fun.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_fun.ml.js-err js_fun.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_map.ml.stdout
   (with-stderr-to js_map.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/js_map.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_map.ml.ref js_map.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_map.ml.err js_map.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_map.ml.js-stdout
   (with-stderr-to js_map.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_map.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_map.ml.js-ref js_map.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_map.ml.js-err js_map.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_pattern.ml.stdout
   (with-stderr-to js_pattern.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_pattern.ml.ref js_pattern.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_pattern.ml.err js_pattern.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_pattern.ml.js-stdout
   (with-stderr-to js_pattern.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_pattern.ml.js-ref js_pattern.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_pattern.ml.js-err js_pattern.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_poly.ml.stdout
   (with-stderr-to js_poly.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/js_poly.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_poly.ml.ref js_poly.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_poly.ml.err js_poly.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_poly.ml.js-stdout
   (with-stderr-to js_poly.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_poly.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_poly.ml.js-ref js_poly.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_poly.ml.js-err js_poly.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_record.ml.stdout
   (with-stderr-to js_record.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_record.ml.ref js_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_record.ml.err js_record.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_record.ml.js-stdout
   (with-stderr-to js_record.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_record.ml.js-ref js_record.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_record.ml.js-err js_record.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_sig.mli.stdout
   (with-stderr-to js_sig.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_sig.mli.ref js_sig.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_sig.mli.err js_sig.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_sig.mli.js-stdout
   (with-stderr-to js_sig.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_sig.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_sig.mli.js-ref js_sig.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_sig.mli.js-err js_sig.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_syntax.ml.stdout
   (with-stderr-to js_syntax.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_syntax.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_syntax.ml.ref js_syntax.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_syntax.ml.err js_syntax.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_syntax.ml.js-stdout
   (with-stderr-to js_syntax.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_syntax.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_syntax.ml.js-ref js_syntax.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_syntax.ml.js-err js_syntax.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to js_to_do.ml.stdout
   (with-stderr-to js_to_do.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_to_do.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/js_to_do.ml.ref js_to_do.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/js_to_do.ml.err js_to_do.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to js_to_do.ml.js-stdout
   (with-stderr-to js_to_do.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_to_do.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/js_to_do.ml.js-ref js_to_do.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/js_to_do.ml.js-err js_to_do.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_upon.ml.stdout
   (with-stderr-to js_upon.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/js_upon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_upon.ml.ref js_upon.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_upon.ml.err js_upon.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to js_upon.ml.js-stdout
   (with-stderr-to js_upon.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/js_upon.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_upon.ml.js-ref js_upon.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_upon.ml.js-err js_upon.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to kw_extentions.ml.stdout
   (with-stderr-to kw_extentions.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/kw_extentions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/kw_extentions.ml kw_extentions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/kw_extentions.ml.err kw_extentions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to kw_extentions.ml.js-stdout
   (with-stderr-to kw_extentions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/kw_extentions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/kw_extentions.ml.js-ref kw_extentions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/kw_extentions.ml.js-err kw_extentions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to label_option_default_args.ml.stdout
   (with-stderr-to label_option_default_args.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=4 %{dep:tests/label_option_default_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/label_option_default_args.ml.ref label_option_default_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/label_option_default_args.ml.err label_option_default_args.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to label_option_default_args.ml.js-stdout
   (with-stderr-to label_option_default_args.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/label_option_default_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/label_option_default_args.ml.js-ref label_option_default_args.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/label_option_default_args.ml.js-err label_option_default_args.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuple_patterns.ml.stdout
   (with-stderr-to labeled_tuple_patterns.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labeled_tuple_patterns.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuple_patterns.ml.ref labeled_tuple_patterns.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuple_patterns.ml.err labeled_tuple_patterns.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuple_patterns.ml.js-stdout
   (with-stderr-to labeled_tuple_patterns.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labeled_tuple_patterns.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuple_patterns.ml.js-ref labeled_tuple_patterns.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuple_patterns.ml.js-err labeled_tuple_patterns.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples.ml.stdout
   (with-stderr-to labeled_tuples.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labeled_tuples.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples.ml.ref labeled_tuples.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples.ml.err labeled_tuples.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples.ml.js-stdout
   (with-stderr-to labeled_tuples.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labeled_tuples.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples.ml.js-ref labeled_tuples.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples.ml.js-err labeled_tuples.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_cmts_attrs.ml.stdout
   (with-stderr-to labeled_tuples_cmts_attrs.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labeled_tuples_cmts_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs.ml.ref labeled_tuples_cmts_attrs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs.ml.err labeled_tuples_cmts_attrs.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_cmts_attrs.ml.js-stdout
   (with-stderr-to labeled_tuples_cmts_attrs.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labeled_tuples_cmts_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs.ml.js-ref labeled_tuples_cmts_attrs.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs.ml.js-err labeled_tuples_cmts_attrs.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_cmts_attrs_move.ml.stdout
   (with-stderr-to labeled_tuples_cmts_attrs_move.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labeled_tuples_cmts_attrs_move.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs_move.ml.ref labeled_tuples_cmts_attrs_move.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs_move.ml.err labeled_tuples_cmts_attrs_move.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_cmts_attrs_move.ml.js-stdout
   (with-stderr-to labeled_tuples_cmts_attrs_move.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labeled_tuples_cmts_attrs_move.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs_move.ml.js-ref labeled_tuples_cmts_attrs_move.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_cmts_attrs_move.ml.js-err labeled_tuples_cmts_attrs_move.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_regressions.ml.stdout
   (with-stderr-to labeled_tuples_regressions.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labeled_tuples_regressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.ml.ref labeled_tuples_regressions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.ml.err labeled_tuples_regressions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_regressions.ml.js-stdout
   (with-stderr-to labeled_tuples_regressions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labeled_tuples_regressions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.ml.js-ref labeled_tuples_regressions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.ml.js-err labeled_tuples_regressions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_regressions.mli.stdout
   (with-stderr-to labeled_tuples_regressions.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labeled_tuples_regressions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.mli.ref labeled_tuples_regressions.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.mli.err labeled_tuples_regressions.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labeled_tuples_regressions.mli.js-stdout
   (with-stderr-to labeled_tuples_regressions.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labeled_tuples_regressions.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.mli.js-ref labeled_tuples_regressions.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labeled_tuples_regressions.mli.js-err labeled_tuples_regressions.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labelled_args-414.ml.stdout
   (with-stderr-to labelled_args-414.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocaml-version=4.14.0 %{dep:tests/labelled_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args-414.ml.ref labelled_args-414.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args-414.ml.err labelled_args-414.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labelled_args-414.ml.js-stdout
   (with-stderr-to labelled_args-414.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labelled_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args-414.ml.js-ref labelled_args-414.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args-414.ml.js-err labelled_args-414.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labelled_args.ml.stdout
   (with-stderr-to labelled_args.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/labelled_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args.ml labelled_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args.ml.err labelled_args.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to labelled_args.ml.js-stdout
   (with-stderr-to labelled_args.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/labelled_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args.ml.js-ref labelled_args.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/labelled_args.ml.js-err labelled_args.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_abbreviation-erased.ml.stdout
   (with-stderr-to layout_abbreviation-erased.ml.stderr
     (run %{bin:ocamlformat} --margin-check --erase-jane-syntax --max-iter=5 %{dep:tests/layout_abbreviation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation-erased.ml.ref layout_abbreviation-erased.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation-erased.ml.err layout_abbreviation-erased.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_abbreviation-erased.ml.js-stdout
   (with-stderr-to layout_abbreviation-erased.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_abbreviation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation-erased.ml.js-ref layout_abbreviation-erased.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation-erased.ml.js-err layout_abbreviation-erased.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_abbreviation.ml.stdout
   (with-stderr-to layout_abbreviation.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=5 %{dep:tests/layout_abbreviation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation.ml.ref layout_abbreviation.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation.ml.err layout_abbreviation.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_abbreviation.ml.js-stdout
   (with-stderr-to layout_abbreviation.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_abbreviation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation.ml.js-ref layout_abbreviation.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_abbreviation.ml.js-err layout_abbreviation.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation-erased.ml.stdout
   (with-stderr-to layout_annotation-erased.ml.stderr
     (run %{bin:ocamlformat} --margin-check --erase-jane-syntax --max-iter=4 %{dep:tests/layout_annotation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation-erased.ml.ref layout_annotation-erased.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation-erased.ml.err layout_annotation-erased.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation-erased.ml.js-stdout
   (with-stderr-to layout_annotation-erased.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_annotation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation-erased.ml.js-ref layout_annotation-erased.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation-erased.ml.js-err layout_annotation-erased.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation.ml.stdout
   (with-stderr-to layout_annotation.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters 3 %{dep:tests/layout_annotation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation.ml.ref layout_annotation.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation.ml.err layout_annotation.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation.ml.js-stdout
   (with-stderr-to layout_annotation.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_annotation.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation.ml.js-ref layout_annotation.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation.ml.js-err layout_annotation.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_composed.ml.stdout
   (with-stderr-to layout_annotation_composed.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters 3 %{dep:tests/layout_annotation_composed.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_composed.ml.ref layout_annotation_composed.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_composed.ml.err layout_annotation_composed.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_composed.ml.js-stdout
   (with-stderr-to layout_annotation_composed.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_annotation_composed.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_composed.ml.js-ref layout_annotation_composed.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_composed.ml.js-err layout_annotation_composed.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_immediate_rewrite.ml.stdout
   (with-stderr-to layout_annotation_immediate_rewrite.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters 3 %{dep:tests/layout_annotation_immediate_rewrite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_immediate_rewrite.ml.ref layout_annotation_immediate_rewrite.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_immediate_rewrite.ml.err layout_annotation_immediate_rewrite.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_immediate_rewrite.ml.js-stdout
   (with-stderr-to layout_annotation_immediate_rewrite.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_annotation_immediate_rewrite.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_immediate_rewrite.ml.js-ref layout_annotation_immediate_rewrite.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_immediate_rewrite.ml.js-err layout_annotation_immediate_rewrite.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_mod.ml.stdout
   (with-stderr-to layout_annotation_mod.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters 3 %{dep:tests/layout_annotation_mod.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_mod.ml.ref layout_annotation_mod.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_mod.ml.err layout_annotation_mod.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_mod.ml.js-stdout
   (with-stderr-to layout_annotation_mod.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_annotation_mod.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_mod.ml.js-ref layout_annotation_mod.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_mod.ml.js-err layout_annotation_mod.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_with.ml.stdout
   (with-stderr-to layout_annotation_with.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters 3 %{dep:tests/layout_annotation_with.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_with.ml.ref layout_annotation_with.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_with.ml.err layout_annotation_with.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to layout_annotation_with.ml.js-stdout
   (with-stderr-to layout_annotation_with.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/layout_annotation_with.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_with.ml.js-ref layout_annotation_with.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/layout_annotation_with.ml.js-err layout_annotation_with.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to lazy.ml.stdout
   (with-stderr-to lazy.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/lazy.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/lazy.ml lazy.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/lazy.ml.err lazy.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to lazy.ml.js-stdout
   (with-stderr-to lazy.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/lazy.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/lazy.ml.js-ref lazy.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/lazy.ml.js-err lazy.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-in_indent.ml.stdout
   (with-stderr-to let_binding-in_indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --indent-after-in=4 %{dep:tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-in_indent.ml.ref let_binding-in_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-in_indent.ml.err let_binding-in_indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-in_indent.ml.js-stdout
   (with-stderr-to let_binding-in_indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-in_indent.ml.js-ref let_binding-in_indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-in_indent.ml.js-err let_binding-in_indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-indent.ml.stdout
   (with-stderr-to let_binding-indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --let-binding-indent=6 %{dep:tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-indent.ml.ref let_binding-indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-indent.ml.err let_binding-indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding-indent.ml.js-stdout
   (with-stderr-to let_binding-indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-indent.ml.js-ref let_binding-indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding-indent.ml.js-err let_binding-indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding.ml.stdout
   (with-stderr-to let_binding.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding.ml.ref let_binding.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding.ml.err let_binding.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding.ml.js-stdout
   (with-stderr-to let_binding.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding.ml.js-ref let_binding.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding.ml.js-err let_binding.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing-double-semicolon.ml.stdout
   (with-stderr-to let_binding_spacing-double-semicolon.ml.stderr
     (run %{bin:ocamlformat} --margin-check --let-binding-spacing=double-semicolon %{dep:tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-double-semicolon.ml.ref let_binding_spacing-double-semicolon.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-double-semicolon.ml.err let_binding_spacing-double-semicolon.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing-double-semicolon.ml.js-stdout
   (with-stderr-to let_binding_spacing-double-semicolon.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-double-semicolon.ml.js-ref let_binding_spacing-double-semicolon.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-double-semicolon.ml.js-err let_binding_spacing-double-semicolon.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing-sparse.ml.stdout
   (with-stderr-to let_binding_spacing-sparse.ml.stderr
     (run %{bin:ocamlformat} --margin-check --let-binding-spacing=sparse %{dep:tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-sparse.ml.ref let_binding_spacing-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-sparse.ml.err let_binding_spacing-sparse.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing-sparse.ml.js-stdout
   (with-stderr-to let_binding_spacing-sparse.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-sparse.ml.js-ref let_binding_spacing-sparse.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing-sparse.ml.js-err let_binding_spacing-sparse.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing.ml.stdout
   (with-stderr-to let_binding_spacing.ml.stderr
     (run %{bin:ocamlformat} --margin-check --let-binding-spacing=compact %{dep:tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing.ml let_binding_spacing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing.ml.err let_binding_spacing.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_binding_spacing.ml.js-stdout
   (with-stderr-to let_binding_spacing.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_binding_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing.ml.js-ref let_binding_spacing.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_binding_spacing.ml.js-err let_binding_spacing.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_in_constr.ml.stdout
   (with-stderr-to let_in_constr.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/let_in_constr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_in_constr.ml let_in_constr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_in_constr.ml.err let_in_constr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_in_constr.ml.js-stdout
   (with-stderr-to let_in_constr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_in_constr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_in_constr.ml.js-ref let_in_constr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_in_constr.ml.js-err let_in_constr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_module-sparse.ml.stdout
   (with-stderr-to let_module-sparse.ml.stderr
     (run %{bin:ocamlformat} --margin-check --let-module=sparse %{dep:tests/let_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module-sparse.ml.ref let_module-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module-sparse.ml.err let_module-sparse.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_module-sparse.ml.js-stdout
   (with-stderr-to let_module-sparse.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module-sparse.ml.js-ref let_module-sparse.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module-sparse.ml.js-err let_module-sparse.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_module.ml.stdout
   (with-stderr-to let_module.ml.stderr
     (run %{bin:ocamlformat} --margin-check --let-module=compact %{dep:tests/let_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module.ml.ref let_module.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module.ml.err let_module.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_module.ml.js-stdout
   (with-stderr-to let_module.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module.ml.js-ref let_module.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_module.ml.js-err let_module.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_punning.ml.stdout
   (with-stderr-to let_punning.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/let_punning.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_punning.ml.ref let_punning.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_punning.ml.err let_punning.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to let_punning.ml.js-stdout
   (with-stderr-to let_punning.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/let_punning.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_punning.ml.js-ref let_punning.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/let_punning.ml.js-err let_punning.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to line_directives.ml.stdout
   (with-stderr-to line_directives.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --margin-check %{dep:tests/line_directives.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/line_directives.ml.ref line_directives.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/line_directives.ml.err line_directives.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to line_directives.ml.js-stdout
   (with-stderr-to line_directives.ml.js-stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/line_directives.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/line_directives.ml.js-ref line_directives.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/line_directives.ml.js-err line_directives.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list-space_around.ml.stdout
   (with-stderr-to list-space_around.ml.stderr
     (run %{bin:ocamlformat} --margin-check --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:tests/list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list-space_around.ml.ref list-space_around.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list-space_around.ml.err list-space_around.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list-space_around.ml.js-stdout
   (with-stderr-to list-space_around.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list-space_around.ml.js-ref list-space_around.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list-space_around.ml.js-err list-space_around.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list.ml.stdout
   (with-stderr-to list.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list.ml list.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list.ml.err list.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list.ml.js-stdout
   (with-stderr-to list.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/list.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list.ml.js-ref list.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list.ml.js-err list.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list_and_comments.ml.stdout
   (with-stderr-to list_and_comments.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/list_and_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_and_comments.ml.ref list_and_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_and_comments.ml.err list_and_comments.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list_and_comments.ml.js-stdout
   (with-stderr-to list_and_comments.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/list_and_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_and_comments.ml.js-ref list_and_comments.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_and_comments.ml.js-err list_and_comments.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list_normalized.ml.stdout
   (with-stderr-to list_normalized.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=4 %{dep:tests/list_normalized.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_normalized.ml.ref list_normalized.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_normalized.ml.err list_normalized.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to list_normalized.ml.js-stdout
   (with-stderr-to list_normalized.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/list_normalized.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_normalized.ml.js-ref list_normalized.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/list_normalized.ml.js-err list_normalized.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to loc_stack.ml.stdout
   (with-stderr-to loc_stack.ml.stderr
     (run %{bin:ocamlformat} --margin-check -n 3 %{dep:tests/loc_stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/loc_stack.ml.ref loc_stack.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/loc_stack.ml.err loc_stack.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to loc_stack.ml.js-stdout
   (with-stderr-to loc_stack.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/loc_stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/loc_stack.ml.js-ref loc_stack.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/loc_stack.ml.js-err loc_stack.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to local.ml.stdout
   (with-stderr-to local.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/local.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local.ml.ref local.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local.ml.err local.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to local.ml.js-stdout
   (with-stderr-to local.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/local.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local.ml.js-ref local.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local.ml.js-err local.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to local_erased.ml.stdout
   (with-stderr-to local_erased.ml.stderr
     (run %{bin:ocamlformat} --margin-check --erase-jane-syntax --max-iter=3 %{dep:tests/local_erased.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local_erased.ml.ref local_erased.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local_erased.ml.err local_erased.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to local_erased.ml.js-stdout
   (with-stderr-to local_erased.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/local_erased.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local_erased.ml.js-ref local_erased.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/local_erased.ml.js-err local_erased.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to locally_abtract_types.ml.stdout
   (with-stderr-to locally_abtract_types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/locally_abtract_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/locally_abtract_types.ml locally_abtract_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/locally_abtract_types.ml.err locally_abtract_types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to locally_abtract_types.ml.js-stdout
   (with-stderr-to locally_abtract_types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/locally_abtract_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/locally_abtract_types.ml.js-ref locally_abtract_types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/locally_abtract_types.ml.js-err locally_abtract_types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to margin_80.ml.stdout
   (with-stderr-to margin_80.ml.stderr
     (run %{bin:ocamlformat} --margin-check --margin=80 %{dep:tests/margin_80.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/margin_80.ml.ref margin_80.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/margin_80.ml.err margin_80.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to margin_80.ml.js-stdout
   (with-stderr-to margin_80.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/margin_80.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/margin_80.ml.js-ref margin_80.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/margin_80.ml.js-err margin_80.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match.ml.stdout
   (with-stderr-to match.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/match.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match.ml match.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match.ml.err match.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match.ml.js-stdout
   (with-stderr-to match.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/match.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match.ml.js-ref match.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match.ml.js-err match.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match2.ml.stdout
   (with-stderr-to match2.ml.stderr
     (run %{bin:ocamlformat} --margin-check --leading-nested-match-parens %{dep:tests/match2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match2.ml match2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match2.ml.err match2.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match2.ml.js-stdout
   (with-stderr-to match2.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/match2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match2.ml.js-ref match2.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match2.ml.js-err match2.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match_indent-never.ml.stdout
   (with-stderr-to match_indent-never.ml.stderr
     (run %{bin:ocamlformat} --margin-check --match-indent=4 --match-indent-nested=never %{dep:tests/match_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent-never.ml.ref match_indent-never.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent-never.ml.err match_indent-never.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match_indent-never.ml.js-stdout
   (with-stderr-to match_indent-never.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/match_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent-never.ml.js-ref match_indent-never.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent-never.ml.js-err match_indent-never.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match_indent.ml.stdout
   (with-stderr-to match_indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --match-indent=4 --match-indent-nested=always %{dep:tests/match_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent.ml.ref match_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent.ml.err match_indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to match_indent.ml.js-stdout
   (with-stderr-to match_indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/match_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent.ml.js-ref match_indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_indent.ml.js-err match_indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to max_indent.ml.stdout
   (with-stderr-to max_indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-indent=2 %{dep:tests/max_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/max_indent.ml max_indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/max_indent.ml.err max_indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to max_indent.ml.js-stdout
   (with-stderr-to max_indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/max_indent.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/max_indent.ml.js-ref max_indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/max_indent.ml.js-err max_indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to mod_type_subst.ml.stdout
   (with-stderr-to mod_type_subst.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/mod_type_subst.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/mod_type_subst.ml mod_type_subst.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/mod_type_subst.ml.err mod_type_subst.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to mod_type_subst.ml.js-stdout
   (with-stderr-to mod_type_subst.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/mod_type_subst.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/mod_type_subst.ml.js-ref mod_type_subst.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/mod_type_subst.ml.js-err mod_type_subst.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes-erased.ml.stdout
   (with-stderr-to modes-erased.ml.stderr
     (run %{bin:ocamlformat} --margin-check --erase-jane-syntax --max-iter=3 %{dep:tests/modes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes-erased.ml.ref modes-erased.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes-erased.ml.err modes-erased.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes-erased.ml.js-stdout
   (with-stderr-to modes-erased.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/modes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes-erased.ml.js-ref modes-erased.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes-erased.ml.js-err modes-erased.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes-ocaml_version.ml.stdout
   (with-stderr-to modes-ocaml_version.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocaml-version=4.14.0 %{dep:tests/modes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes-ocaml_version.ml.ref modes-ocaml_version.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes-ocaml_version.ml.err modes-ocaml_version.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes.ml.stdout
   (with-stderr-to modes.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/modes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes.ml.ref modes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes.ml.err modes.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes.ml.js-stdout
   (with-stderr-to modes.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/modes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes.ml.js-ref modes.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes.ml.js-err modes.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_attrs.ml.stdout
   (with-stderr-to modes_attrs.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/modes_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_attrs.ml.ref modes_attrs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_attrs.ml.err modes_attrs.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_attrs.ml.js-stdout
   (with-stderr-to modes_attrs.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/modes_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_attrs.ml.js-ref modes_attrs.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_attrs.ml.js-err modes_attrs.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_cmts-break_separators_after.ml.stdout
   (with-stderr-to modes_cmts-break_separators_after.ml.stderr
     (run %{bin:ocamlformat} --margin-check --break-separators=after %{dep:tests/modes_cmts.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts-break_separators_after.ml.ref modes_cmts-break_separators_after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts-break_separators_after.ml.err modes_cmts-break_separators_after.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_cmts.ml.stdout
   (with-stderr-to modes_cmts.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/modes_cmts.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts.ml.ref modes_cmts.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts.ml.err modes_cmts.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_cmts.ml.js-stdout
   (with-stderr-to modes_cmts.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/modes_cmts.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts.ml.js-ref modes_cmts.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts.ml.js-err modes_cmts.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_cmts_move.ml.stdout
   (with-stderr-to modes_cmts_move.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/modes_cmts_move.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts_move.ml.ref modes_cmts_move.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts_move.ml.err modes_cmts_move.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to modes_cmts_move.ml.js-stdout
   (with-stderr-to modes_cmts_move.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/modes_cmts_move.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts_move.ml.js-ref modes_cmts_move.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/modes_cmts_move.ml.js-err modes_cmts_move.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module.ml.stdout
   (with-stderr-to module.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module.ml module.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module.ml.err module.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module.ml.js-stdout
   (with-stderr-to module.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module.ml.js-ref module.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module.ml.js-err module.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_anonymous.ml.stdout
   (with-stderr-to module_anonymous.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/module_anonymous.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_anonymous.ml module_anonymous.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_anonymous.ml.err module_anonymous.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_anonymous.ml.js-stdout
   (with-stderr-to module_anonymous.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_anonymous.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_anonymous.ml.js-ref module_anonymous.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_anonymous.ml.js-err module_anonymous.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_attributes.ml.stdout
   (with-stderr-to module_attributes.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/module_attributes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_attributes.ml.ref module_attributes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_attributes.ml.err module_attributes.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_attributes.ml.js-stdout
   (with-stderr-to module_attributes.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_attributes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_attributes.ml.js-ref module_attributes.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_attributes.ml.js-err module_attributes.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing-preserve.ml.stdout
   (with-stderr-to module_item_spacing-preserve.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 --module-item-spacing=preserve %{dep:tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-preserve.ml.ref module_item_spacing-preserve.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-preserve.ml.err module_item_spacing-preserve.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing-preserve.ml.js-stdout
   (with-stderr-to module_item_spacing-preserve.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-preserve.ml.js-ref module_item_spacing-preserve.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-preserve.ml.js-err module_item_spacing-preserve.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing-sparse.ml.stdout
   (with-stderr-to module_item_spacing-sparse.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 --module-item-spacing=sparse %{dep:tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-sparse.ml.ref module_item_spacing-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-sparse.ml.err module_item_spacing-sparse.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing-sparse.ml.js-stdout
   (with-stderr-to module_item_spacing-sparse.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-sparse.ml.js-ref module_item_spacing-sparse.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing-sparse.ml.js-err module_item_spacing-sparse.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing.ml.stdout
   (with-stderr-to module_item_spacing.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 --module-item-spacing=compact %{dep:tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.ml.ref module_item_spacing.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.ml.err module_item_spacing.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing.ml.js-stdout
   (with-stderr-to module_item_spacing.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_item_spacing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.ml.js-ref module_item_spacing.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.ml.js-err module_item_spacing.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing.mli.stdout
   (with-stderr-to module_item_spacing.mli.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=3 %{dep:tests/module_item_spacing.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.mli.ref module_item_spacing.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.mli.err module_item_spacing.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_item_spacing.mli.js-stdout
   (with-stderr-to module_item_spacing.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_item_spacing.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.mli.js-ref module_item_spacing.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_item_spacing.mli.js-err module_item_spacing.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_type.ml.stdout
   (with-stderr-to module_type.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/module_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.ml module_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.ml.err module_type.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_type.ml.js-stdout
   (with-stderr-to module_type.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.ml.js-ref module_type.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.ml.js-err module_type.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_type.mli.stdout
   (with-stderr-to module_type.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/module_type.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.mli.ref module_type.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.mli.err module_type.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_type.mli.js-stdout
   (with-stderr-to module_type.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_type.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.mli.js-ref module_type.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type.mli.js-err module_type.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_type_strengthening.ml.stdout
   (with-stderr-to module_type_strengthening.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/module_type_strengthening.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type_strengthening.ml module_type_strengthening.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type_strengthening.ml.err module_type_strengthening.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to module_type_strengthening.ml.js-stdout
   (with-stderr-to module_type_strengthening.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/module_type_strengthening.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type_strengthening.ml.js-ref module_type_strengthening.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module_type_strengthening.ml.js-err module_type_strengthening.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to monadic_binding.ml.stdout
   (with-stderr-to monadic_binding.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/monadic_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/monadic_binding.ml monadic_binding.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/monadic_binding.ml.err monadic_binding.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to monadic_binding.ml.js-stdout
   (with-stderr-to monadic_binding.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/monadic_binding.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/monadic_binding.ml.js-ref monadic_binding.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/monadic_binding.ml.js-err monadic_binding.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to multi_index_op.ml.stdout
   (with-stderr-to multi_index_op.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/multi_index_op.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/multi_index_op.ml multi_index_op.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/multi_index_op.ml.err multi_index_op.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to multi_index_op.ml.js-stdout
   (with-stderr-to multi_index_op.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/multi_index_op.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/multi_index_op.ml.js-ref multi_index_op.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/multi_index_op.ml.js-err multi_index_op.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to named_existentials.ml.stdout
   (with-stderr-to named_existentials.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/named_existentials.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/named_existentials.ml named_existentials.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/named_existentials.ml.err named_existentials.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to named_existentials.ml.js-stdout
   (with-stderr-to named_existentials.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/named_existentials.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/named_existentials.ml.js-ref named_existentials.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/named_existentials.ml.js-err named_existentials.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to need_format.ml.stdout
   (with-stderr-to need_format.ml.stderr
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} --margin-check --max-iters=1 %{dep:tests/need_format.ml}))))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/need_format.ml.ref need_format.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/need_format.ml.err need_format.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to new.ml.stdout
   (with-stderr-to new.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/new.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/new.ml new.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/new.ml.err new.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to new.ml.js-stdout
   (with-stderr-to new.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/new.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/new.ml.js-ref new.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/new.ml.js-err new.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object.ml.stdout
   (with-stderr-to object.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/object.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object.ml object.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object.ml.err object.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object.ml.js-stdout
   (with-stderr-to object.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/object.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object.ml.js-ref object.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object.ml.js-err object.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object2.ml.stdout
   (with-stderr-to object2.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/object2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object2.ml.ref object2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object2.ml.err object2.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object2.ml.js-stdout
   (with-stderr-to object2.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/object2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object2.ml.js-ref object2.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object2.ml.js-err object2.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object_expr-414.ml.stdout
   (with-stderr-to object_expr-414.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocaml-version=4.14.0 %{dep:tests/object_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr-414.ml.ref object_expr-414.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr-414.ml.err object_expr-414.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object_expr-414.ml.js-stdout
   (with-stderr-to object_expr-414.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/object_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr-414.ml.js-ref object_expr-414.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr-414.ml.js-err object_expr-414.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object_expr.ml.stdout
   (with-stderr-to object_expr.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/object_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr.ml.ref object_expr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr.ml.err object_expr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object_expr.ml.js-stdout
   (with-stderr-to object_expr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/object_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr.ml.js-ref object_expr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_expr.ml.js-err object_expr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object_type.ml.stdout
   (with-stderr-to object_type.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/object_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_type.ml.ref object_type.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_type.ml.err object_type.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to object_type.ml.js-stdout
   (with-stderr-to object_type.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/object_type.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_type.ml.js-ref object_type.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object_type.ml.js-err object_type.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to obuild.ml.stdout
   (with-stderr-to obuild.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/obuild.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/obuild.ml.ref obuild.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/obuild.ml.err obuild.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to obuild.ml.js-stdout
   (with-stderr-to obuild.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/obuild.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/obuild.ml.js-ref obuild.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/obuild.ml.js-err obuild.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_compat-break_colon_after.ml.stdout
   (with-stderr-to ocp_indent_compat-break_colon_after.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocp-indent-compat --break-colon=after %{dep:tests/ocp_indent_compat.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat-break_colon_after.ml.ref ocp_indent_compat-break_colon_after.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat-break_colon_after.ml.err ocp_indent_compat-break_colon_after.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_compat-break_colon_after.ml.js-stdout
   (with-stderr-to ocp_indent_compat-break_colon_after.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ocp_indent_compat.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat-break_colon_after.ml.js-ref ocp_indent_compat-break_colon_after.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat-break_colon_after.ml.js-err ocp_indent_compat-break_colon_after.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_compat.ml.stdout
   (with-stderr-to ocp_indent_compat.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocp-indent-compat --break-colon=before %{dep:tests/ocp_indent_compat.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat.ml ocp_indent_compat.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat.ml.err ocp_indent_compat.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_compat.ml.js-stdout
   (with-stderr-to ocp_indent_compat.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ocp_indent_compat.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat.ml.js-ref ocp_indent_compat.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_compat.ml.js-err ocp_indent_compat.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_options.ml.stdout
   (with-stderr-to ocp_indent_options.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocp-indent-config %{dep:tests/ocp_indent_options.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_options.ml.ref ocp_indent_options.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_options.ml.err ocp_indent_options.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to ocp_indent_options.ml.js-stdout
   (with-stderr-to ocp_indent_options.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/ocp_indent_options.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_options.ml.js-ref ocp_indent_options.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocp_indent_options.ml.js-err ocp_indent_options.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to open-closing-on-separate-line.ml.stdout
   (with-stderr-to open-closing-on-separate-line.ml.stderr
     (run %{bin:ocamlformat} --margin-check --indicate-multiline-delimiters=closing-on-separate-line %{dep:tests/open.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open-closing-on-separate-line.ml.ref open-closing-on-separate-line.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open-closing-on-separate-line.ml.err open-closing-on-separate-line.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to open-closing-on-separate-line.ml.js-stdout
   (with-stderr-to open-closing-on-separate-line.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/open.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open-closing-on-separate-line.ml.js-ref open-closing-on-separate-line.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open-closing-on-separate-line.ml.js-err open-closing-on-separate-line.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to open.ml.stdout
   (with-stderr-to open.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/open.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open.ml.ref open.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open.ml.err open.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to open.ml.js-stdout
   (with-stderr-to open.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/open.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open.ml.js-ref open.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open.ml.js-err open.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to open_types.ml.stdout
   (with-stderr-to open_types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/open_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open_types.ml open_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open_types.ml.err open_types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to open_types.ml.js-stdout
   (with-stderr-to open_types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/open_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open_types.ml.js-ref open_types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/open_types.ml.js-err open_types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to option.ml.stdout
   (with-stderr-to option.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/option.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/option.ml.ref option.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/option.ml.err option.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to option.ml.js-stdout
   (with-stderr-to option.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/option.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/option.ml.js-ref option.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/option.ml.js-err option.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to override.ml.stdout
   (with-stderr-to override.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/override.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/override.ml.ref override.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/override.ml.err override.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to override.ml.js-stdout
   (with-stderr-to override.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/override.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/override.ml.js-ref override.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/override.ml.js-err override.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to parens_tuple_patterns.ml.stdout
   (with-stderr-to parens_tuple_patterns.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/parens_tuple_patterns.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/parens_tuple_patterns.ml parens_tuple_patterns.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/parens_tuple_patterns.ml.err parens_tuple_patterns.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to parens_tuple_patterns.ml.js-stdout
   (with-stderr-to parens_tuple_patterns.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/parens_tuple_patterns.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/parens_tuple_patterns.ml.js-ref parens_tuple_patterns.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/parens_tuple_patterns.ml.js-err parens_tuple_patterns.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to polyparams.ml.stdout
   (with-stderr-to polyparams.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/polyparams.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polyparams.ml polyparams.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polyparams.ml.err polyparams.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to polyparams.ml.js-stdout
   (with-stderr-to polyparams.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/polyparams.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polyparams.ml.js-ref polyparams.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polyparams.ml.js-err polyparams.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to polytypes-default.ml.stdout
   (with-stderr-to polytypes-default.ml.stderr
     (run %{bin:ocamlformat} --margin-check --profile=default %{dep:tests/polytypes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polytypes-default.ml.ref polytypes-default.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polytypes-default.ml.err polytypes-default.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to polytypes.ml.stdout
   (with-stderr-to polytypes.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/polytypes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polytypes.ml polytypes.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polytypes.ml.err polytypes.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to polytypes.ml.js-stdout
   (with-stderr-to polytypes.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/polytypes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polytypes.ml.js-ref polytypes.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/polytypes.ml.js-err polytypes.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to pre_post_extensions.ml.stdout
   (with-stderr-to pre_post_extensions.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/pre_post_extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/pre_post_extensions.ml pre_post_extensions.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/pre_post_extensions.ml.err pre_post_extensions.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to pre_post_extensions.ml.js-stdout
   (with-stderr-to pre_post_extensions.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/pre_post_extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/pre_post_extensions.ml.js-ref pre_post_extensions.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/pre_post_extensions.ml.js-err pre_post_extensions.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to precedence.ml.stdout
   (with-stderr-to precedence.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/precedence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/precedence.ml precedence.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/precedence.ml.err precedence.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to precedence.ml.js-stdout
   (with-stderr-to precedence.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/precedence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/precedence.ml.js-ref precedence.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/precedence.ml.js-err precedence.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to prefix_infix.ml.stdout
   (with-stderr-to prefix_infix.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/prefix_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/prefix_infix.ml prefix_infix.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/prefix_infix.ml.err prefix_infix.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to prefix_infix.ml.js-stdout
   (with-stderr-to prefix_infix.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/prefix_infix.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/prefix_infix.ml.js-ref prefix_infix.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/prefix_infix.ml.js-err prefix_infix.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat tests/dir1/.ocamlformat tests/dir1/dir2/.ocamlformat tests/dir1/dir2/print_config.ml)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to print_config.ml.stdout
   (with-stderr-to print_config.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/dir1/dir2/print_config.ml} --print-config --config=max-iters=2 %{dep:tests/print_config.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/print_config.ml.ref print_config.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/print_config.ml.err print_config.ml.stderr)))

(rule
 (deps tests/.ocamlformat tests/dir1/.ocamlformat tests/dir1/dir2/.ocamlformat tests/dir1/dir2/print_config.ml)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to print_config.ml.js-stdout
   (with-stderr-to print_config.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/print_config.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/print_config.ml.js-ref print_config.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/print_config.ml.js-err print_config.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to profiles.ml.stdout
   (with-stderr-to profiles.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/profiles.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles.ml profiles.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles.ml.err profiles.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to profiles.ml.js-stdout
   (with-stderr-to profiles.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/profiles.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles.ml.js-ref profiles.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles.ml.js-err profiles.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to profiles2.ml.stdout
   (with-stderr-to profiles2.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/profiles2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles2.ml profiles2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles2.ml.err profiles2.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to profiles2.ml.js-stdout
   (with-stderr-to profiles2.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/profiles2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles2.ml.js-ref profiles2.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/profiles2.ml.js-err profiles2.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to protected_object_types.ml.stdout
   (with-stderr-to protected_object_types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/protected_object_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/protected_object_types.ml protected_object_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/protected_object_types.ml.err protected_object_types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to protected_object_types.ml.js-stdout
   (with-stderr-to protected_object_types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/protected_object_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/protected_object_types.ml.js-ref protected_object_types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/protected_object_types.ml.js-err protected_object_types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to qtest.ml.stdout
   (with-stderr-to qtest.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/qtest.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/qtest.ml qtest.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/qtest.ml.err qtest.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to qtest.ml.js-stdout
   (with-stderr-to qtest.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/qtest.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/qtest.ml.js-ref qtest.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/qtest.ml.js-err qtest.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to quoted_strings.ml.stdout
   (with-stderr-to quoted_strings.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/quoted_strings.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/quoted_strings.ml quoted_strings.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/quoted_strings.ml.err quoted_strings.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to quoted_strings.ml.js-stdout
   (with-stderr-to quoted_strings.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/quoted_strings.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/quoted_strings.ml.js-ref quoted_strings.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/quoted_strings.ml.js-err quoted_strings.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to recmod.mli.stdout
   (with-stderr-to recmod.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/recmod.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/recmod.mli recmod.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/recmod.mli.err recmod.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to recmod.mli.js-stdout
   (with-stderr-to recmod.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/recmod.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/recmod.mli.js-ref recmod.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/recmod.mli.js-err recmod.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record-402.ml.stdout
   (with-stderr-to record-402.ml.stderr
     (run %{bin:ocamlformat} --margin-check --ocaml-version=4.02 %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-402.ml.ref record-402.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-402.ml.err record-402.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record-402.ml.js-stdout
   (with-stderr-to record-402.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-402.ml.js-ref record-402.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-402.ml.js-err record-402.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record-loose.ml.stdout
   (with-stderr-to record-loose.ml.stderr
     (run %{bin:ocamlformat} --margin-check --field-space=loose %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-loose.ml.ref record-loose.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-loose.ml.err record-loose.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record-loose.ml.js-stdout
   (with-stderr-to record-loose.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-loose.ml.js-ref record-loose.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-loose.ml.js-err record-loose.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record-tight_decl.ml.stdout
   (with-stderr-to record-tight_decl.ml.stderr
     (run %{bin:ocamlformat} --margin-check --field-space=tight-decl %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-tight_decl.ml.ref record-tight_decl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-tight_decl.ml.err record-tight_decl.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record-tight_decl.ml.js-stdout
   (with-stderr-to record-tight_decl.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-tight_decl.ml.js-ref record-tight_decl.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record-tight_decl.ml.js-err record-tight_decl.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record.ml.stdout
   (with-stderr-to record.ml.stderr
     (run %{bin:ocamlformat} --margin-check --field-space=tight %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record.ml.ref record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record.ml.err record.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record.ml.js-stdout
   (with-stderr-to record.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record.ml.js-ref record.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record.ml.js-err record.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record_punning.ml.stdout
   (with-stderr-to record_punning.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/record_punning.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record_punning.ml.ref record_punning.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record_punning.ml.err record_punning.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to record_punning.ml.js-stdout
   (with-stderr-to record_punning.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/record_punning.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record_punning.ml.js-ref record_punning.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record_punning.ml.js-err record_punning.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to reformat_string.ml.stdout
   (with-stderr-to reformat_string.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iter=2 %{dep:tests/reformat_string.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/reformat_string.ml.ref reformat_string.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/reformat_string.ml.err reformat_string.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to reformat_string.ml.js-stdout
   (with-stderr-to reformat_string.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/reformat_string.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/reformat_string.ml.js-ref reformat_string.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/reformat_string.ml.js-err reformat_string.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to refs.ml.stdout
   (with-stderr-to refs.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/refs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/refs.ml refs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/refs.ml.err refs.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to refs.ml.js-stdout
   (with-stderr-to refs.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/refs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/refs.ml.js-ref refs.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/refs.ml.js-err refs.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to remove_extra_parens.ml.stdout
   (with-stderr-to remove_extra_parens.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/remove_extra_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/remove_extra_parens.ml.ref remove_extra_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/remove_extra_parens.ml.err remove_extra_parens.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to remove_extra_parens.ml.js-stdout
   (with-stderr-to remove_extra_parens.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/remove_extra_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/remove_extra_parens.ml.js-ref remove_extra_parens.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/remove_extra_parens.ml.js-err remove_extra_parens.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to repl.ml.stdout
   (with-stderr-to repl.ml.stderr
     (run %{bin:ocamlformat} --margin-check --parse-toplevel-phrases --repl-file %{dep:tests/repl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/repl.ml.ref repl.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/repl.ml.err repl.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to repl.mli.stdout
   (with-stderr-to repl.mli.stderr
     (run %{bin:ocamlformat} --margin-check --parse-toplevel-phrases %{dep:tests/repl.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/repl.mli.ref repl.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/repl.mli.err repl.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to repl.mli.js-stdout
   (with-stderr-to repl.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/repl.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/repl.mli.js-ref repl.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/repl.mli.js-err repl.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to revapply_ext.ml.stdout
   (with-stderr-to revapply_ext.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/revapply_ext.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/revapply_ext.ml revapply_ext.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/revapply_ext.ml.err revapply_ext.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to revapply_ext.ml.js-stdout
   (with-stderr-to revapply_ext.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/revapply_ext.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/revapply_ext.ml.js-ref revapply_ext.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/revapply_ext.ml.js-err revapply_ext.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to send.ml.stdout
   (with-stderr-to send.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/send.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/send.ml send.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/send.ml.err send.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to send.ml.js-stdout
   (with-stderr-to send.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/send.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/send.ml.js-ref send.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/send.ml.js-err send.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to sequence-preserve.ml.stdout
   (with-stderr-to sequence-preserve.ml.stderr
     (run %{bin:ocamlformat} --margin-check --sequence-blank-line=preserve-one --max-iter=3 %{dep:tests/sequence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence-preserve.ml.ref sequence-preserve.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence-preserve.ml.err sequence-preserve.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to sequence-preserve.ml.js-stdout
   (with-stderr-to sequence-preserve.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/sequence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence-preserve.ml.js-ref sequence-preserve.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence-preserve.ml.js-err sequence-preserve.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to sequence.ml.stdout
   (with-stderr-to sequence.ml.stderr
     (run %{bin:ocamlformat} --margin-check --sequence-blank-line=compact %{dep:tests/sequence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence.ml.ref sequence.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence.ml.err sequence.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to sequence.ml.js-stdout
   (with-stderr-to sequence.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/sequence.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence.ml.js-ref sequence.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sequence.ml.js-err sequence.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to shebang.ml.stdout
   (with-stderr-to shebang.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/shebang.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shebang.ml shebang.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shebang.ml.err shebang.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to shebang.ml.js-stdout
   (with-stderr-to shebang.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/shebang.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shebang.ml.js-ref shebang.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shebang.ml.js-err shebang.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to shortcut_ext_attr.ml.stdout
   (with-stderr-to shortcut_ext_attr.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/shortcut_ext_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shortcut_ext_attr.ml shortcut_ext_attr.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shortcut_ext_attr.ml.err shortcut_ext_attr.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to shortcut_ext_attr.ml.js-stdout
   (with-stderr-to shortcut_ext_attr.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/shortcut_ext_attr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shortcut_ext_attr.ml.js-ref shortcut_ext_attr.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/shortcut_ext_attr.ml.js-err shortcut_ext_attr.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to sig_value.mli.stdout
   (with-stderr-to sig_value.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/sig_value.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sig_value.mli.ref sig_value.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sig_value.mli.err sig_value.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to sig_value.mli.js-stdout
   (with-stderr-to sig_value.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/sig_value.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sig_value.mli.js-ref sig_value.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/sig_value.mli.js-err sig_value.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to single_line.mli.stdout
   (with-stderr-to single_line.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/single_line.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/single_line.mli single_line.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/single_line.mli.err single_line.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to single_line.mli.js-stdout
   (with-stderr-to single_line.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/single_line.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/single_line.mli.js-ref single_line.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/single_line.mli.js-err single_line.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to skip.ml.stdout
   (with-stderr-to skip.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/skip.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/skip.ml skip.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/skip.ml.err skip.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to skip.ml.js-stdout
   (with-stderr-to skip.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/skip.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/skip.ml.js-ref skip.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/skip.ml.js-err skip.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to source.ml.stdout
   (with-stderr-to source.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=5 %{dep:tests/source.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/source.ml.ref source.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/source.ml.err source.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to source.ml.js-stdout
   (with-stderr-to source.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/source.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/source.ml.js-ref source.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/source.ml.js-err source.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to stack-erased.ml.stdout
   (with-stderr-to stack-erased.ml.stderr
     (run %{bin:ocamlformat} --margin-check --erase-jane-syntax --max-iter=3 %{dep:tests/stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack-erased.ml.ref stack-erased.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack-erased.ml.err stack-erased.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to stack-erased.ml.js-stdout
   (with-stderr-to stack-erased.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack-erased.ml.js-ref stack-erased.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack-erased.ml.js-err stack-erased.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to stack.ml.stdout
   (with-stderr-to stack.ml.stderr
     (run %{bin:ocamlformat} --margin-check --max-iters=3 %{dep:tests/stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack.ml.ref stack.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack.ml.err stack.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to stack.ml.js-stdout
   (with-stderr-to stack.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/stack.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack.ml.js-ref stack.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/stack.ml.js-err stack.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to str_value.ml.stdout
   (with-stderr-to str_value.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/str_value.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/str_value.ml str_value.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/str_value.ml.err str_value.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to str_value.ml.js-stdout
   (with-stderr-to str_value.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/str_value.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/str_value.ml.js-ref str_value.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/str_value.ml.js-err str_value.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to string.ml.stdout
   (with-stderr-to string.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/string.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string.ml.ref string.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string.ml.err string.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to string.ml.js-stdout
   (with-stderr-to string.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/string.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string.ml.js-ref string.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string.ml.js-err string.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to string_array.ml.stdout
   (with-stderr-to string_array.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/string_array.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_array.ml string_array.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_array.ml.err string_array.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to string_array.ml.js-stdout
   (with-stderr-to string_array.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/string_array.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_array.ml.js-ref string_array.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_array.ml.js-err string_array.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to string_wrapping.ml.stdout
   (with-stderr-to string_wrapping.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/string_wrapping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_wrapping.ml string_wrapping.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_wrapping.ml.err string_wrapping.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to string_wrapping.ml.js-stdout
   (with-stderr-to string_wrapping.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/string_wrapping.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_wrapping.ml.js-ref string_wrapping.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/string_wrapping.ml.js-err string_wrapping.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to symbol.ml.stdout
   (with-stderr-to symbol.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/symbol.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/symbol.ml symbol.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/symbol.ml.err symbol.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to symbol.ml.js-stdout
   (with-stderr-to symbol.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/symbol.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/symbol.ml.js-ref symbol.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/symbol.ml.js-err symbol.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tag_only.ml.stdout
   (with-stderr-to tag_only.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/tag_only.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.ml tag_only.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.ml.err tag_only.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tag_only.ml.js-stdout
   (with-stderr-to tag_only.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/tag_only.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.ml.js-ref tag_only.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.ml.js-err tag_only.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tag_only.mli.stdout
   (with-stderr-to tag_only.mli.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/tag_only.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.mli tag_only.mli.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.mli.err tag_only.mli.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tag_only.mli.js-stdout
   (with-stderr-to tag_only.mli.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/tag_only.mli})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.mli.js-ref tag_only.mli.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tag_only.mli.js-err tag_only.mli.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to try_with_or_pattern.ml.stdout
   (with-stderr-to try_with_or_pattern.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/try_with_or_pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/try_with_or_pattern.ml try_with_or_pattern.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/try_with_or_pattern.ml.err try_with_or_pattern.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to try_with_or_pattern.ml.js-stdout
   (with-stderr-to try_with_or_pattern.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/try_with_or_pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/try_with_or_pattern.ml.js-ref try_with_or_pattern.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/try_with_or_pattern.ml.js-err try_with_or_pattern.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tuple.ml.stdout
   (with-stderr-to tuple.ml.stderr
     (run %{bin:ocamlformat} --margin-check --parens-tuple=always %{dep:tests/tuple.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple.ml tuple.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple.ml.err tuple.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tuple.ml.js-stdout
   (with-stderr-to tuple.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/tuple.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple.ml.js-ref tuple.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple.ml.js-err tuple.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tuple_less_parens.ml.stdout
   (with-stderr-to tuple_less_parens.ml.stderr
     (run %{bin:ocamlformat} --margin-check --parens-tuple=multi-line-only %{dep:tests/tuple_less_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_less_parens.ml tuple_less_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_less_parens.ml.err tuple_less_parens.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tuple_less_parens.ml.js-stdout
   (with-stderr-to tuple_less_parens.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/tuple_less_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_less_parens.ml.js-ref tuple_less_parens.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_less_parens.ml.js-err tuple_less_parens.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tuple_type_parens.ml.stdout
   (with-stderr-to tuple_type_parens.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/tuple_type_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_type_parens.ml tuple_type_parens.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_type_parens.ml.err tuple_type_parens.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to tuple_type_parens.ml.js-stdout
   (with-stderr-to tuple_type_parens.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/tuple_type_parens.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_type_parens.ml.js-ref tuple_type_parens.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/tuple_type_parens.ml.js-err tuple_type_parens.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to type_and_constraint.ml.stdout
   (with-stderr-to type_and_constraint.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/type_and_constraint.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_and_constraint.ml type_and_constraint.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_and_constraint.ml.err type_and_constraint.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to type_and_constraint.ml.js-stdout
   (with-stderr-to type_and_constraint.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/type_and_constraint.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_and_constraint.ml.js-ref type_and_constraint.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_and_constraint.ml.js-err type_and_constraint.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to type_annotations.ml.stdout
   (with-stderr-to type_annotations.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/type_annotations.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_annotations.ml type_annotations.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_annotations.ml.err type_annotations.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to type_annotations.ml.js-stdout
   (with-stderr-to type_annotations.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/type_annotations.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_annotations.ml.js-ref type_annotations.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/type_annotations.ml.js-err type_annotations.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-compact-space_around-docked.ml.stdout
   (with-stderr-to types-compact-space_around-docked.ml.stderr
     (run %{bin:ocamlformat} --margin-check --type-decl=compact --space-around-arrays --space-around-lists --space-around-records --space-around-variants --break-separators=after --dock-collection-brackets %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around-docked.ml.ref types-compact-space_around-docked.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around-docked.ml.err types-compact-space_around-docked.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-compact-space_around-docked.ml.js-stdout
   (with-stderr-to types-compact-space_around-docked.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around-docked.ml.js-ref types-compact-space_around-docked.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around-docked.ml.js-err types-compact-space_around-docked.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-compact-space_around.ml.stdout
   (with-stderr-to types-compact-space_around.ml.stderr
     (run %{bin:ocamlformat} --margin-check --type-decl=compact --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around.ml.ref types-compact-space_around.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around.ml.err types-compact-space_around.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-compact-space_around.ml.js-stdout
   (with-stderr-to types-compact-space_around.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around.ml.js-ref types-compact-space_around.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact-space_around.ml.js-err types-compact-space_around.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-compact.ml.stdout
   (with-stderr-to types-compact.ml.stderr
     (run %{bin:ocamlformat} --margin-check --type-decl=compact %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact.ml.ref types-compact.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact.ml.err types-compact.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-compact.ml.js-stdout
   (with-stderr-to types-compact.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact.ml.js-ref types-compact.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-compact.ml.js-err types-compact.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-indent.ml.stdout
   (with-stderr-to types-indent.ml.stderr
     (run %{bin:ocamlformat} --margin-check --type-decl-indent=6 %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-indent.ml.ref types-indent.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-indent.ml.err types-indent.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-indent.ml.js-stdout
   (with-stderr-to types-indent.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-indent.ml.js-ref types-indent.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-indent.ml.js-err types-indent.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-sparse-space_around.ml.stdout
   (with-stderr-to types-sparse-space_around.ml.stderr
     (run %{bin:ocamlformat} --margin-check --type-decl=sparse --space-around-arrays --space-around-lists --space-around-records --space-around-variants %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse-space_around.ml.ref types-sparse-space_around.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse-space_around.ml.err types-sparse-space_around.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-sparse-space_around.ml.js-stdout
   (with-stderr-to types-sparse-space_around.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse-space_around.ml.js-ref types-sparse-space_around.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse-space_around.ml.js-err types-sparse-space_around.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-sparse.ml.stdout
   (with-stderr-to types-sparse.ml.stderr
     (run %{bin:ocamlformat} --margin-check --type-decl=sparse %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse.ml.ref types-sparse.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse.ml.err types-sparse.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types-sparse.ml.js-stdout
   (with-stderr-to types-sparse.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse.ml.js-ref types-sparse.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types-sparse.ml.js-err types-sparse.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types.ml.stdout
   (with-stderr-to types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types.ml types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types.ml.err types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to types.ml.js-stdout
   (with-stderr-to types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types.ml.js-ref types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/types.ml.js-err types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unary.ml.stdout
   (with-stderr-to unary.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unary.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary.ml.ref unary.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary.ml.err unary.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unary.ml.js-stdout
   (with-stderr-to unary.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unary.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary.ml.js-ref unary.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary.ml.js-err unary.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unary_hash.ml.stdout
   (with-stderr-to unary_hash.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unary_hash.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary_hash.ml unary_hash.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary_hash.ml.err unary_hash.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unary_hash.ml.js-stdout
   (with-stderr-to unary_hash.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unary_hash.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary_hash.ml.js-ref unary_hash.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unary_hash.ml.js-err unary_hash.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_record.ml.stdout
   (with-stderr-to unboxed_record.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unboxed_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_record.ml.ref unboxed_record.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_record.ml.err unboxed_record.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_record.ml.js-stdout
   (with-stderr-to unboxed_record.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unboxed_record.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_record.ml.js-ref unboxed_record.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_record.ml.js-err unboxed_record.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_records_cmts_attrs.ml.stdout
   (with-stderr-to unboxed_records_cmts_attrs.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unboxed_records_cmts_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_records_cmts_attrs.ml unboxed_records_cmts_attrs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_records_cmts_attrs.ml.err unboxed_records_cmts_attrs.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_records_cmts_attrs.ml.js-stdout
   (with-stderr-to unboxed_records_cmts_attrs.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unboxed_records_cmts_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_records_cmts_attrs.ml.js-ref unboxed_records_cmts_attrs.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_records_cmts_attrs.ml.js-err unboxed_records_cmts_attrs.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_tuples.ml.stdout
   (with-stderr-to unboxed_tuples.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unboxed_tuples.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples.ml unboxed_tuples.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples.ml.err unboxed_tuples.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_tuples.ml.js-stdout
   (with-stderr-to unboxed_tuples.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unboxed_tuples.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples.ml.js-ref unboxed_tuples.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples.ml.js-err unboxed_tuples.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_tuples_cmts_attrs.ml.stdout
   (with-stderr-to unboxed_tuples_cmts_attrs.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unboxed_tuples_cmts_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples_cmts_attrs.ml unboxed_tuples_cmts_attrs.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples_cmts_attrs.ml.err unboxed_tuples_cmts_attrs.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_tuples_cmts_attrs.ml.js-stdout
   (with-stderr-to unboxed_tuples_cmts_attrs.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unboxed_tuples_cmts_attrs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples_cmts_attrs.ml.js-ref unboxed_tuples_cmts_attrs.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_tuples_cmts_attrs.ml.js-err unboxed_tuples_cmts_attrs.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_types.ml.stdout
   (with-stderr-to unboxed_types.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unboxed_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types.ml unboxed_types.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types.ml.err unboxed_types.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_types.ml.js-stdout
   (with-stderr-to unboxed_types.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unboxed_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types.ml.js-ref unboxed_types.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types.ml.js-err unboxed_types.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_types2.ml.stdout
   (with-stderr-to unboxed_types2.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/unboxed_types2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types2.ml.ref unboxed_types2.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types2.ml.err unboxed_types2.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unboxed_types2.ml.js-stdout
   (with-stderr-to unboxed_types2.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unboxed_types2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types2.ml.js-ref unboxed_types2.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unboxed_types2.ml.js-err unboxed_types2.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unicode.ml.stdout
   (with-stderr-to unicode.ml.stderr
     (run %{bin:ocamlformat} --margin-check --margin=80 --wrap-comments %{dep:tests/unicode.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unicode.ml.ref unicode.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unicode.ml.err unicode.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to unicode.ml.js-stdout
   (with-stderr-to unicode.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/unicode.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unicode.ml.js-ref unicode.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unicode.ml.js-err unicode.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to use_file.mlt.stdout
   (with-stderr-to use_file.mlt.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/use_file.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/use_file.mlt use_file.mlt.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/use_file.mlt.err use_file.mlt.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to use_file.mlt.js-stdout
   (with-stderr-to use_file.mlt.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/use_file.mlt})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/use_file.mlt.js-ref use_file.mlt.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/use_file.mlt.js-err use_file.mlt.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to variance.ml.stdout
   (with-stderr-to variance.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/variance.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variance.ml.ref variance.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variance.ml.err variance.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to variance.ml.js-stdout
   (with-stderr-to variance.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/variance.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variance.ml.js-ref variance.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variance.ml.js-err variance.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to variants.ml.stdout
   (with-stderr-to variants.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/variants.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variants.ml variants.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variants.ml.err variants.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to variants.ml.js-stdout
   (with-stderr-to variants.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/variants.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variants.ml.js-ref variants.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/variants.ml.js-err variants.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to verbatim_comments-wrap.ml.stdout
   (with-stderr-to verbatim_comments-wrap.ml.stderr
     (run %{bin:ocamlformat} --margin-check --wrap-comments %{dep:tests/verbatim_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments-wrap.ml.ref verbatim_comments-wrap.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments-wrap.ml.err verbatim_comments-wrap.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to verbatim_comments-wrap.ml.js-stdout
   (with-stderr-to verbatim_comments-wrap.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/verbatim_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments-wrap.ml.js-ref verbatim_comments-wrap.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments-wrap.ml.js-err verbatim_comments-wrap.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to verbatim_comments.ml.stdout
   (with-stderr-to verbatim_comments.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/verbatim_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments.ml.ref verbatim_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments.ml.err verbatim_comments.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to verbatim_comments.ml.js-stdout
   (with-stderr-to verbatim_comments.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/verbatim_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments.ml.js-ref verbatim_comments.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/verbatim_comments.ml.js-err verbatim_comments.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to verbose1.ml.stdout
   (with-stderr-to verbose1.ml.stderr
     (run %{bin:ocamlformat} --margin-check --print-config --doc-comments=before --config=doc-comments=before %{dep:tests/verbose1.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/verbose1.ml.ref verbose1.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/verbose1.ml.err verbose1.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to verbose1.ml.js-stdout
   (with-stderr-to verbose1.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/verbose1.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/verbose1.ml.js-ref verbose1.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/verbose1.ml.js-err verbose1.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to w50.ml.stdout
   (with-stderr-to w50.ml.stderr
     (run %{bin:ocamlformat} --margin-check --no-comment-check -q --max-iters=3 %{dep:tests/w50.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/w50.ml.ref w50.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/w50.ml.err w50.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to wrap_comments.ml.stdout
   (with-stderr-to wrap_comments.ml.stderr
     (run %{bin:ocamlformat} --margin-check --profile=ocamlformat --max-iters=3 %{dep:tests/wrap_comments.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/wrap_comments.ml.ref wrap_comments.ml.stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/wrap_comments.ml.err wrap_comments.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action
  (with-stdout-to wrap_comments.ml.js-stdout
   (with-stderr-to wrap_comments.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/wrap_comments.ml})))))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/wrap_comments.ml.js-ref wrap_comments.ml.js-stdout)))

(rule
 (alias runtest)
 (enabled_if (<> %{os_type} Win32))
 (package ocamlformat)
 (action (diff tests/wrap_comments.ml.js-err wrap_comments.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to wrap_comments_break.ml.stdout
   (with-stderr-to wrap_comments_break.ml.stderr
     (run %{bin:ocamlformat} --margin-check --no-wrap-fun-args --margin=67 %{dep:tests/wrap_comments_break.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_comments_break.ml wrap_comments_break.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_comments_break.ml.err wrap_comments_break.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to wrap_comments_break.ml.js-stdout
   (with-stderr-to wrap_comments_break.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/wrap_comments_break.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_comments_break.ml.js-ref wrap_comments_break.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_comments_break.ml.js-err wrap_comments_break.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to wrap_invalid_doc_comments.ml.stdout
   (with-stderr-to wrap_invalid_doc_comments.ml.stderr
     (run %{bin:ocamlformat} --margin-check --parse-docstrings --wrap-comments %{dep:tests/wrap_invalid_doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_invalid_doc_comments.ml wrap_invalid_doc_comments.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_invalid_doc_comments.ml.err wrap_invalid_doc_comments.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to wrap_invalid_doc_comments.ml.js-stdout
   (with-stderr-to wrap_invalid_doc_comments.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/wrap_invalid_doc_comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_invalid_doc_comments.ml.js-ref wrap_invalid_doc_comments.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrap_invalid_doc_comments.ml.js-err wrap_invalid_doc_comments.ml.js-stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to wrapping_functor_args.ml.stdout
   (with-stderr-to wrapping_functor_args.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/wrapping_functor_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrapping_functor_args.ml wrapping_functor_args.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrapping_functor_args.ml.err wrapping_functor_args.ml.stderr)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to wrapping_functor_args.ml.js-stdout
   (with-stderr-to wrapping_functor_args.ml.js-stderr
     (run %{bin:ocamlformat} --profile=janestreet --enable-outside-detected-project --disable-conf-files %{dep:tests/wrapping_functor_args.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrapping_functor_args.ml.js-ref wrapping_functor_args.ml.js-stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/wrapping_functor_args.ml.js-err wrapping_functor_args.ml.js-stderr)))
