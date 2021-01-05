
(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to ocamldoc.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:ocamldoc.ml})))))

(rule
 (alias runtest)
 (action (diff ocamldoc.ml.broken-ref ocamldoc.ml.output)))
