
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
   (with-outputs-to unit_values.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:unit_values.ml})))))

(rule
 (alias runtest)
 (action (diff unit_values.ml.broken-ref unit_values.ml.output)))
