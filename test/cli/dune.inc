
(rule
 (targets default_several_file.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:default_several_file.opts}"))))

(alias
 (name runtest)
 (action (diff default_several_file.ref default_several_file.output)))

(rule
 (targets inplace_and_check.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:inplace_and_check.opts}"))))

(alias
 (name runtest)
 (action (diff inplace_and_check.ref inplace_and_check.output)))

(rule
 (targets inplace_and_output.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:inplace_and_output.opts}"))))

(alias
 (name runtest)
 (action (diff inplace_and_output.ref inplace_and_output.output)))

(rule
 (targets no_arg.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:no_arg.opts}"))))

(alias
 (name runtest)
 (action (diff no_arg.ref no_arg.output)))

(rule
 (targets output_and_check.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:output_and_check.opts}"))))

(alias
 (name runtest)
 (action (diff output_and_check.ref output_and_check.output)))

(rule
 (targets output_several_files.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:output_several_files.opts}"))))

(alias
 (name runtest)
 (action (diff output_several_files.ref output_several_files.output)))

(rule
 (targets stdin.output)
 (action
  (with-outputs-to %{targets}
   (system "%{bin:ocamlformat} %{read-lines:stdin.opts} < stdin.stdin"))))

(alias
 (name runtest)
 (action (diff stdin.ref stdin.output)))

(rule
 (targets stdin_and_file.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:stdin_and_file.opts}"))))

(alias
 (name runtest)
 (action (diff stdin_and_file.ref stdin_and_file.output)))

(rule
 (targets stdin_and_inplace.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:stdin_and_inplace.opts}"))))

(alias
 (name runtest)
 (action (diff stdin_and_inplace.ref stdin_and_inplace.output)))

(rule
 (targets stdin_no_kind.output)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:stdin_no_kind.opts}"))))

(alias
 (name runtest)
 (action (diff stdin_no_kind.ref stdin_no_kind.output)))
