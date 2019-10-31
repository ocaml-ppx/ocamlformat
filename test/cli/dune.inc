
(rule
 (targets no_arg.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:no_arg.opts}"))))

(alias
 (name runtest)
 (action (diff no_arg.ref no_arg.output)))
