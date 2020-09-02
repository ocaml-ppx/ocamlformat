
(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to comments.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:comments.ml})))))

(rule
 (alias runtest)
 (action (diff comments.ml.broken-ref comments.ml.output)))

(rule
 (deps .ocamlformat )
 (action
   (with-outputs-to core_failing.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:core_failing.ml})))))

(rule
 (alias runtest)
 (action (diff core_failing.ml.broken-ref core_failing.ml.output)))

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
   (with-outputs-to js_functor.ml.output
     (with-accepted-exit-codes 1
       (run %{bin:ocamlformat} %{dep:js_functor.ml})))))

(rule
 (alias runtest)
 (action (diff js_functor.ml.broken-ref js_functor.ml.output)))
