
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
   (with-outputs-to js_2018.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_2018.ml})))))

(rule
 (alias runtest)
 (action (diff js_2018.ml.broken-ref js_2018.ml.output)))

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
   (with-outputs-to ocamldoc.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:ocamldoc.ml})))))

(rule
 (alias runtest)
 (action (diff ocamldoc.ml.broken-ref ocamldoc.ml.output)))

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
