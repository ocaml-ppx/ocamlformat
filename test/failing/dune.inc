
(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to comments.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/comments.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/comments.ml.broken-ref comments.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to core_failing.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/core_failing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/core_failing.ml.broken-ref core_failing.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to core_passing.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/core_passing.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/core_passing.ml.broken-ref core_passing.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to edge_cases.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/edge_cases.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/edge_cases.ml.broken-ref edge_cases.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to escaped_nl.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/escaped_nl.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/escaped_nl.ml.broken-ref escaped_nl.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to exprs.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/exprs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/exprs.ml.broken-ref exprs.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to ifand.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/ifand.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ifand.ml.broken-ref ifand.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to indent_empty.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/indent_empty.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/indent_empty.ml.broken-ref indent_empty.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_2018.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_2018.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_2018.ml.broken-ref js_2018.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_bench.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_bench.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_bench.ml.broken-ref js_bench.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_bind.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_bind.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_bind.ml.broken-ref js_bind.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_comment.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_comment.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_comment.ml.broken-ref js_comment.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_default.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_default.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_default.ml.broken-ref js_default.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_functor.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_functor.ml.broken-ref js_functor.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_low_priority.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_low_priority.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_low_priority.ml.broken-ref js_low_priority.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_map.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_map.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_map.ml.broken-ref js_map.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_poly.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_poly.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_poly.ml.broken-ref js_poly.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to js_test.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/js_test.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/js_test.ml.broken-ref js_test.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to lwt.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/lwt.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/lwt.ml.broken-ref lwt.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to macro.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/macro.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/macro.ml.broken-ref macro.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to match_fun.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/match_fun.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/match_fun.ml.broken-ref match_fun.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to misc_2019.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/misc_2019.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/misc_2019.ml.broken-ref misc_2019.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to module.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/module.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/module.ml.broken-ref module.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to nesting.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/nesting.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/nesting.ml.broken-ref nesting.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to never_align.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/never_align.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/never_align.ml.broken-ref never_align.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to object.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/object.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/object.ml.broken-ref object.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to obuild.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/obuild.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/obuild.ml.broken-ref obuild.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to ocamldoc.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/ocamldoc.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ocamldoc.ml.broken-ref ocamldoc.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to partial2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/partial2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/partial2.ml.broken-ref partial2.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to pattern.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/pattern.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/pattern.ml.broken-ref pattern.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to ppx_stritem_ext.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/ppx_stritem_ext.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/ppx_stritem_ext.ml.broken-ref ppx_stritem_ext.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to quotations2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/quotations2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/quotations2.ml.broken-ref quotations2.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to record_with.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/record_with.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/record_with.ml.broken-ref record_with.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_classes.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_classes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_classes.ml.broken-ref unit_classes.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_expr.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_expr.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_expr.ml.broken-ref unit_expr.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_extensions.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_extensions.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_extensions.ml.broken-ref unit_extensions.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_lex.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_lex.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_lex.ml.broken-ref unit_lex.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_modtypes.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_modtypes.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_modtypes.ml.broken-ref unit_modtypes.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_typedefs.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_typedefs.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_typedefs.ml.broken-ref unit_typedefs.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_types.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_types.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_types.ml.broken-ref unit_types.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to unit_values.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/unit_values.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/unit_values.ml.broken-ref unit_values.ml.output)))

(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
   (with-outputs-to with_2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:tests/with_2.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/with_2.ml.broken-ref with_2.ml.output)))
