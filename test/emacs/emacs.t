
  $ emacs --batch --no-site-file --no-splash --load test.el --debug-init
  Debugger entered--Lisp error: (wrong-type-argument stringp nil)
    set-buffer(nil)
    (save-current-buffer (set-buffer b) ((quote ocamlformat)))
    (let ((ok (save-current-buffer (set-buffer b) ((quote ocamlformat))))) ok)
    (let* (b (file-truename (make-temp-file "tmp" nil ".ml"))) (let ((ok (save-current-buffer (set-buffer b) ((quote ocamlformat))))) ok))
    eval-buffer(#<buffer  *load*> nil "$TESTCASE_ROOT/test.el" nil t)  ; Reading at buffer position 261
    load-with-code-conversion("$TESTCASE_ROOT/test.el" "$TESTCASE_ROOT/test.el" nil t)
    load("$TESTCASE_ROOT/test.el" nil t)
    command-line-1(("--no-splash" "--load" "test.el"))
    command-line()
    normal-top-level()
  
  [255]
