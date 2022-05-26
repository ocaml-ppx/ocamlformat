
(rule (action (with-stdout-to binop.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/binop.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/binop.ml.ref binop.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to empty.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/empty.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/empty.ml.ref empty.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to empty.mli.stdout (run ./gen/driver.exe -signature %{dep:signature/empty.mli}))) (package ocamlformat))
(rule (alias runtest) (action (diff signature/empty.mli.ref empty.mli.stdout)) (package ocamlformat))

(rule (action (with-stdout-to invalid_after_eq.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/invalid_after_eq.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/invalid_after_eq.ml.ref invalid_after_eq.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to invalid_after_in.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/invalid_after_in.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/invalid_after_in.ml.ref invalid_after_in.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to invalid_if.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/invalid_if.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/invalid_if.ml.ref invalid_if.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to invalid_if2.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/invalid_if2.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/invalid_if2.ml.ref invalid_if2.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to invalid_seq_modules.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/invalid_seq_modules.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/invalid_seq_modules.ml.ref invalid_seq_modules.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to many_unclosed.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/many_unclosed.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/many_unclosed.ml.ref many_unclosed.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to many_unclosed.mlt.stdout (run ./gen/driver.exe -use-file %{dep:use_file/many_unclosed.mlt}))) (package ocamlformat))
(rule (alias runtest) (action (diff use_file/many_unclosed.mlt.ref many_unclosed.mlt.stdout)) (package ocamlformat))

(rule (action (with-stdout-to pr7847.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/pr7847.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/pr7847.ml.ref pr7847.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_begin.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_begin.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_begin.ml.ref unclosed_begin.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class.ml.ref unclosed_class.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class2.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class2.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class2.ml.ref unclosed_class2.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class3.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class3.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class3.ml.ref unclosed_class3.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class4.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class4.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class4.ml.ref unclosed_class4.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class5.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class5.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class5.ml.ref unclosed_class5.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class6.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class6.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class6.ml.ref unclosed_class6.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class7.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class7.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class7.ml.ref unclosed_class7.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class8.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_class8.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_class8.ml.ref unclosed_class8.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_class_sig.mli.stdout (run ./gen/driver.exe -signature %{dep:signature/unclosed_class_sig.mli}))) (package ocamlformat))
(rule (alias runtest) (action (diff signature/unclosed_class_sig.mli.ref unclosed_class_sig.mli.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_if.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_if.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_if.ml.ref unclosed_if.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_if2.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_if2.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_if2.ml.ref unclosed_if2.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_mod_expr1.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_mod_expr1.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_mod_expr1.ml.ref unclosed_mod_expr1.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_mod_expr2.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_mod_expr2.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_mod_expr2.ml.ref unclosed_mod_expr2.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_mod_expr3.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_mod_expr3.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_mod_expr3.ml.ref unclosed_mod_expr3.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_module.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_module.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_module.ml.ref unclosed_module.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_module2.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_module2.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_module2.ml.ref unclosed_module2.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_object.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_object.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_object.ml.ref unclosed_object.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_paren_module_type.mli.stdout (run ./gen/driver.exe -signature %{dep:signature/unclosed_paren_module_type.mli}))) (package ocamlformat))
(rule (alias runtest) (action (diff signature/unclosed_paren_module_type.mli.ref unclosed_paren_module_type.mli.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_sig.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_sig.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_sig.ml.ref unclosed_sig.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_sig.mli.stdout (run ./gen/driver.exe -signature %{dep:signature/unclosed_sig.mli}))) (package ocamlformat))
(rule (alias runtest) (action (diff signature/unclosed_sig.mli.ref unclosed_sig.mli.stdout)) (package ocamlformat))

(rule (action (with-stdout-to unclosed_struct.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/unclosed_struct.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/unclosed_struct.ml.ref unclosed_struct.ml.stdout)) (package ocamlformat))

(rule (action (with-stdout-to valid.ml.stdout (run ./gen/driver.exe -structure %{dep:structure/valid.ml}))) (package ocamlformat))
(rule (alias runtest) (action (diff structure/valid.ml.ref valid.ml.stdout)) (package ocamlformat))
