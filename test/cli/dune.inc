
(rule
 (targets err_default_several_file.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_default_several_file.opts}"))))

(alias
 (name runtest)
 (action (diff err_default_several_file.ref err_default_several_file.output)))

(rule
 (targets err_inplace_and_check.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_inplace_and_check.opts}"))))

(alias
 (name runtest)
 (action (diff err_inplace_and_check.ref err_inplace_and_check.output)))

(rule
 (targets err_inplace_and_output.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_inplace_and_output.opts}"))))

(alias
 (name runtest)
 (action (diff err_inplace_and_output.ref err_inplace_and_output.output)))

(rule
 (targets err_no_arg.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_no_arg.opts}"))))

(alias
 (name runtest)
 (action (diff err_no_arg.ref err_no_arg.output)))

(rule
 (targets err_output_and_check.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_output_and_check.opts}"))))

(alias
 (name runtest)
 (action (diff err_output_and_check.ref err_output_and_check.output)))

(rule
 (targets err_output_several_files.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_output_several_files.opts}"))))

(alias
 (name runtest)
 (action (diff err_output_several_files.ref err_output_several_files.output)))

(rule
 (targets err_stdin_and_file.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_stdin_and_file.opts}"))))

(alias
 (name runtest)
 (action (diff err_stdin_and_file.ref err_stdin_and_file.output)))

(rule
 (targets err_stdin_and_inplace.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_stdin_and_inplace.opts}"))))

(alias
 (name runtest)
 (action (diff err_stdin_and_inplace.ref err_stdin_and_inplace.output)))

(rule
 (targets err_stdin_name_unknown_ext.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_stdin_name_unknown_ext.opts} < err_stdin_name_unknown_ext.stdin"))))

(alias
 (name runtest)
 (action (diff err_stdin_name_unknown_ext.ref err_stdin_name_unknown_ext.output)))

(rule
 (targets err_stdin_no_kind.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "! %{bin:ocamlformat} %{read-lines:err_stdin_no_kind.opts}"))))

(alias
 (name runtest)
 (action (diff err_stdin_no_kind.ref err_stdin_no_kind.output)))

(rule
 (targets name_unknown_ext.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "%{bin:ocamlformat} %{read-lines:name_unknown_ext.opts}"))))

(alias
 (name runtest)
 (action (diff name_unknown_ext.ref name_unknown_ext.output)))

(rule
 (targets stdin_and_impl.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "%{bin:ocamlformat} %{read-lines:stdin_and_impl.opts} < stdin_and_impl.stdin"))))

(alias
 (name runtest)
 (action (diff stdin_and_impl.ref stdin_and_impl.output)))

(rule
 (targets stdin_and_intf.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "%{bin:ocamlformat} %{read-lines:stdin_and_intf.opts} < stdin_and_intf.stdin"))))

(alias
 (name runtest)
 (action (diff stdin_and_intf.ref stdin_and_intf.output)))

(rule
 (targets stdin_and_name.output)
 (deps .ocamlformat)
 (action
  (with-outputs-to %{targets}
   (system "%{bin:ocamlformat} %{read-lines:stdin_and_name.opts} < stdin_and_name.stdin"))))

(alias
 (name runtest)
 (action (diff stdin_and_name.ref stdin_and_name.output)))
