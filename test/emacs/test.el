(setq debug-on-error t)

(add-to-list 'load-path "../../emacs/")

(require 'ocamlformat)

;; Runs successfully
(let* (b (file-truename (make-temp-file "tmp" nil ".ml")))
   (let
       ((ok
         (with-current-buffer b
           ('ocamlformat))))
     ok))
