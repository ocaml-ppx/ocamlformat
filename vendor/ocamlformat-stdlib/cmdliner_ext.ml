include Cmdliner

let parse info validate term =
  Cmd.eval_value (Cmd.v info (Term.(ret (const validate $ term))))
