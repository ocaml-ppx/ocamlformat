
(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to core_passing.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:core_passing.ml})))))

(rule
 (alias runtest)
 (action (diff core_passing.ml.broken-ref core_passing.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to edge_cases.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:edge_cases.ml})))))

(rule
 (alias runtest)
 (action (diff edge_cases.ml.broken-ref edge_cases.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to escaped_nl.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:escaped_nl.ml})))))

(rule
 (alias runtest)
 (action (diff escaped_nl.ml.broken-ref escaped_nl.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to exprs.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:exprs.ml})))))

(rule
 (alias runtest)
 (action (diff exprs.ml.broken-ref exprs.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ifand.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:ifand.ml})))))

(rule
 (alias runtest)
 (action (diff ifand.ml.broken-ref ifand.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to indent_empty.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:indent_empty.ml})))))

(rule
 (alias runtest)
 (action (diff indent_empty.ml.broken-ref indent_empty.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_2018.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_2018.ml})))))

(rule
 (alias runtest)
 (action (diff js_2018.ml.broken-ref js_2018.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_bench.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_bench.ml})))))

(rule
 (alias runtest)
 (action (diff js_bench.ml.broken-ref js_bench.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_bind.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_bind.ml})))))

(rule
 (alias runtest)
 (action (diff js_bind.ml.broken-ref js_bind.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_comment.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_comment.ml})))))

(rule
 (alias runtest)
 (action (diff js_comment.ml.broken-ref js_comment.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_default.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_default.ml})))))

(rule
 (alias runtest)
 (action (diff js_default.ml.broken-ref js_default.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_functor.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_functor.ml})))))

(rule
 (alias runtest)
 (action (diff js_functor.ml.broken-ref js_functor.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_low_priority.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_low_priority.ml})))))

(rule
 (alias runtest)
 (action (diff js_low_priority.ml.broken-ref js_low_priority.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_map.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_map.ml})))))

(rule
 (alias runtest)
 (action (diff js_map.ml.broken-ref js_map.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_poly.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_poly.ml})))))

(rule
 (alias runtest)
 (action (diff js_poly.ml.broken-ref js_poly.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to js_test.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_test.ml})))))

(rule
 (alias runtest)
 (action (diff js_test.ml.broken-ref js_test.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to lwt.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:lwt.ml})))))

(rule
 (alias runtest)
 (action (diff lwt.ml.broken-ref lwt.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to macro.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:macro.ml})))))

(rule
 (alias runtest)
 (action (diff macro.ml.broken-ref macro.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to match_fun.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:match_fun.ml})))))

(rule
 (alias runtest)
 (action (diff match_fun.ml.broken-ref match_fun.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to misc_2019.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:misc_2019.ml})))))

(rule
 (alias runtest)
 (action (diff misc_2019.ml.broken-ref misc_2019.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to module.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:module.ml})))))

(rule
 (alias runtest)
 (action (diff module.ml.broken-ref module.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to nesting.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:nesting.ml})))))

(rule
 (alias runtest)
 (action (diff nesting.ml.broken-ref nesting.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to never_align.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:never_align.ml})))))

(rule
 (alias runtest)
 (action (diff never_align.ml.broken-ref never_align.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to object.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:object.ml})))))

(rule
 (alias runtest)
 (action (diff object.ml.broken-ref object.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to obuild.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:obuild.ml})))))

(rule
 (alias runtest)
 (action (diff obuild.ml.broken-ref obuild.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ocamldoc.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:ocamldoc.ml})))))

(rule
 (alias runtest)
 (action (diff ocamldoc.ml.broken-ref ocamldoc.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to partial2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:partial2.ml})))))

(rule
 (alias runtest)
 (action (diff partial2.ml.broken-ref partial2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to pattern.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:pattern.ml})))))

(rule
 (alias runtest)
 (action (diff pattern.ml.broken-ref pattern.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ppx_stritem_ext.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:ppx_stritem_ext.ml})))))

(rule
 (alias runtest)
 (action (diff ppx_stritem_ext.ml.broken-ref ppx_stritem_ext.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to quotations2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:quotations2.ml})))))

(rule
 (alias runtest)
 (action (diff quotations2.ml.broken-ref quotations2.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to record_with.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:record_with.ml})))))

(rule
 (alias runtest)
 (action (diff record_with.ml.broken-ref record_with.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_classes.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_classes.ml})))))

(rule
 (alias runtest)
 (action (diff unit_classes.ml.broken-ref unit_classes.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_expr.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_expr.ml})))))

(rule
 (alias runtest)
 (action (diff unit_expr.ml.broken-ref unit_expr.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_extensions.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_extensions.ml})))))

(rule
 (alias runtest)
 (action (diff unit_extensions.ml.broken-ref unit_extensions.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_lex.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_lex.ml})))))

(rule
 (alias runtest)
 (action (diff unit_lex.ml.broken-ref unit_lex.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_modtypes.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_modtypes.ml})))))

(rule
 (alias runtest)
 (action (diff unit_modtypes.ml.broken-ref unit_modtypes.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_typedefs.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_typedefs.ml})))))

(rule
 (alias runtest)
 (action (diff unit_typedefs.ml.broken-ref unit_typedefs.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_types.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_types.ml})))))

(rule
 (alias runtest)
 (action (diff unit_types.ml.broken-ref unit_types.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to unit_values.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_values.ml})))))

(rule
 (alias runtest)
 (action (diff unit_values.ml.broken-ref unit_values.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to with_2.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:with_2.ml})))))

(rule
 (alias runtest)
 (action (diff with_2.ml.broken-ref with_2.ml.output)))
