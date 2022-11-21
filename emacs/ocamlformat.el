;;; ocamlformat.el --- Utility functions to format ocaml code -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "24.3"))
;; Version: 0.15.0
;; Keywords: languages, ocaml
;; URL: https://github.com/ocaml-ppx/ocamlformat

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)

;;; Commentary:
;;
;; This package provides utilities to format OCaml code using the ocamlformat
;; command.  The recommended usage is to format before saving a file:
;;
;;   (add-hook 'before-save-hook 'ocamlformat-before-save)

;;; Code:

(require 'cl-lib)
(require 'vc)

(defcustom ocamlformat-command "ocamlformat"
  "The `ocamlformat' command."
  :type 'string
  :group 'ocamlformat)

(defcustom ocamlformat-enable 'enable
  "Enable or disable ocamlformat."
  :type '(choice
          (const :tag "Enable" enable)
          (const :tag "Enable outside detected project"
                 enable-outside-detected-project)
          (const :tag "Disable" disable))
  :group 'ocamlformat)

(defcustom ocamlformat-show-errors 'buffer
  "Where to display ocamlformat error output.

It can either be displayed in the *compilation* buffer, in the
echo area, or not at all.  Please note that Emacs outputs to the
echo area when writing files and will overwrite ocamlformat's
echo output if used from inside a `before-save-hook'."
  :type '(choice
          (const :tag "*compilation* buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'ocamlformat)

(defcustom ocamlformat-margin-mode nil
  "Specify margin when formatting buffer contents."
  :type '(choice
          (const :tag "Window width" window)
          (const :tag "Fill column" fill)
          (const :tag "None" nil))
  :group 'ocamlformat)

(defcustom ocamlformat-file-kind nil
  "Add a parse argument to ocamlformat if using an unrecognized extension.

It can either be set to \\='implementation, \\='interface or
nil (default)."
  :type '(choice
          (const :tag "implementation" implementation)
          (const :tag "interface" interface)
          (const :tag "none" nil))
  :group 'ocamlformat)

;;;###autoload
(defun ocamlformat-before-save ()
  "Add this to .emacs to run ocamlformat on the current buffer when saving:

\(add-hook \\='before-save-hook \\='ocamlformat-before-save)."
  (interactive)
  (when (eq major-mode 'tuareg-mode) (ocamlformat)))

(defun ocamlformat--goto-line (line)
  "Move point to the line numbered LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun ocamlformat--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun ocamlformat--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in ocamlformat--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (ocamlformat--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (ocamlformat--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in ocamlformat--apply-rcs-patch")))))))))

(defun ocamlformat--process-errors (filename tmpfile errorfile errbuf)
  "Display ocamlformat errors in ERRBUF, a compilation buffer.

Error messages are read from ERRORFILE, and occurrences of
TMPFILE in the error messages are replaced with FILENAME."
  (with-current-buffer errbuf
    (if (eq ocamlformat-show-errors 'echo)
        (message "%s" (buffer-string))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the ocamlformat stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "ocamlformat errors:\n")
      (while (search-forward-regexp (regexp-quote tmpfile) nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

;; replace-buffer-contents is broken in emacs-26.1
;; try to detect broken implementation.
;; https://bugzilla.redhat.com/show_bug.cgi?id=1597251
(defconst ocamlformat--support-replace-buffer-contents
  (and (fboundp 'replace-buffer-contents)
       (let* ((a (generate-new-buffer "tmp"))
              (b (generate-new-buffer "tmp")))
         (with-current-buffer a
           (erase-buffer)
           (insert "\u2666\nabc\n"))
         (let ((ok (with-current-buffer b
                     (erase-buffer)
                     (insert "\u2666\naXbc\n")
                     (replace-buffer-contents a)
                     (string= (buffer-string) "\u2666\nabc\n"))))
           (kill-buffer a)
           (kill-buffer b)
           ok))))

(defun ocamlformat--replace-buffer-contents (outputfile)
  "Replace the current buffer's contents with the contents of OUTPUTFILE.

Uses `replace-buffer-contents'."
  (replace-buffer-contents (find-file-noselect outputfile))
  (kill-buffer (get-file-buffer outputfile)))

(defun ocamlformat--patch-buffer (outputfile)
  "Replace the current buffer's contents with the contents of OUTPUTFILE.

Uses `ocamlformat--apply-rcs-patch' instead of
`replace-buffer-contents'.  This function is used by
`ocamlformat' when `ocamlformat--support-replace-buffer-contents'
is nil."
  (let ((patchbuf (get-buffer-create "*OCamlFormat patch*")))
    (with-current-buffer patchbuf (erase-buffer))
    (call-process-region
     (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" outputfile)
    (ocamlformat--apply-rcs-patch patchbuf)
    (kill-buffer patchbuf)))

(defun ocamlformat ()
  "Format the current buffer according to the ocamlformat tool."
  (interactive)
  (let* ((ext (file-name-extension buffer-file-name t))
         (bufferfile (file-truename (make-temp-file "ocamlformat" nil ext)))
         (outputfile (file-truename (make-temp-file "ocamlformat" nil ext)))
         (errorfile (file-truename (make-temp-file "ocamlformat" nil ext)))
         (errbuf
          (cond
           ((eq ocamlformat-show-errors 'buffer)
            (get-buffer-create "*compilation*"))
           ((eq ocamlformat-show-errors 'echo)
            (get-buffer-create "*OCamlFormat stderr*"))))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (margin-args
          (cond
           ((equal ocamlformat-margin-mode 'window)
            (list "--margin" (number-to-string (window-body-width))))
           ((equal ocamlformat-margin-mode 'fill)
            (list "--margin" (number-to-string fill-column)))
           (t
            '())))
         (enable-args
          (cond
           ((equal ocamlformat-enable 'disable)
            (list "--disable"))
           ((equal ocamlformat-enable 'enable-outside-detected-project)
            (list "--enable-outside-detected-project"))
           (t
            '())))
         (extension-args
          (cond
           ((eq ocamlformat-file-kind 'implementation)
            (list "--impl"))
           ((eq ocamlformat-file-kind 'interface)
            (list "--intf")))))
    (unwind-protect
        (save-restriction
          (widen)
          (write-region nil nil bufferfile)
          (if (zerop
               (apply #'call-process
                      ocamlformat-command nil (list :file errorfile) nil
                      (append margin-args enable-args extension-args
                              (list
                               "--name" buffer-file-name
                               "--output" outputfile bufferfile))))
              (progn
                (if ocamlformat--support-replace-buffer-contents
                    (ocamlformat--replace-buffer-contents outputfile)
                  (ocamlformat--patch-buffer outputfile))
                (message "Applied ocamlformat on %s" buffer-file-name))
            (if errbuf
                (progn
                  (with-current-buffer errbuf
                    (setq buffer-read-only nil)
                    (erase-buffer))
                  (ocamlformat--process-errors
                   (buffer-file-name) bufferfile errorfile errbuf)))
            (message "Could not apply ocamlformat on %s" buffer-file-name)))
      (delete-file errorfile)
      (delete-file bufferfile)
      (delete-file outputfile))))

(defun ocamlformat-args (name start-line end-line)
  (let*
      ((margin-args
        (cond
         ((equal ocamlformat-margin-mode 'window)
          (list "--margin" (number-to-string (window-body-width))))
         ((equal ocamlformat-margin-mode 'fill)
          (list "--margin" (number-to-string fill-column)))
         (t
          '())))
       (enable-args
        (cond
         ((equal ocamlformat-enable 'disable)
          (list "--disable"))
         ((equal ocamlformat-enable 'enable-outside-detected-project)
          (list "--enable-outside-detected-project"))
         (t
          '())))
       (extension-args
        (cond
         ((eq ocamlformat-file-kind 'implementation)
          (list "--impl"))
         ((eq ocamlformat-file-kind 'interface)
          (list "--intf")))))
    (append margin-args enable-args extension-args
            (list
             "-"
             "--name" name
             "--numeric"
             "--range" (format "%d-%d" start-line end-line)))))

(defun ocamlformat-region (start end)
  (interactive "r")
  (let*
      ((ext (file-name-extension buffer-file-name t))
       (bufferfile (file-truename (make-temp-file "ocamlformat" nil ext)))
       (errorfile (file-truename (make-temp-file "ocamlformat" nil ext)))
       (errbuf
        (cond
         ((eq ocamlformat-show-errors 'buffer)
          (get-buffer-create "*compilation*"))
         ((eq ocamlformat-show-errors 'echo)
          (get-buffer-create "*OCamlFormat stderr*"))))
       (start-line (line-number-at-pos start))
       (end-line (line-number-at-pos end))
       (indents-str
        (with-output-to-string
          (if (/= 0
                (apply 'call-process-region
                       (point-min) (point-max) ocamlformat-command nil
                       (list standard-output errorfile) nil
                       (ocamlformat-args buffer-file-name start-line end-line)))
              (progn
                (if errbuf
                  (progn
                    (with-current-buffer errbuf
                      (setq buffer-read-only nil)
                      (erase-buffer))
                    (ocamlformat--process-errors
                     (buffer-file-name) bufferfile errorfile errbuf)))
                (message "Could not apply ocamlformat")))))
       (indents (mapcar 'string-to-number (split-string indents-str))))
    (unwind-protect
      (save-excursion
        (goto-char start)
        (mapcar
         #'(lambda (indent) (indent-line-to indent) (forward-line))
         indents))
      (delete-file errorfile)
      (delete-file bufferfile))))

(defun ocamlformat-line ()
  (interactive nil)
  (ocamlformat-region (point) (point)))

(defun ocamlformat--enable-indent ()
  "Whether the indentation feature is enabled."
  (version<= "0.19.0" (ocamlformat-version)))

;;;###autoload
(defun ocamlformat-setup-indent ()
  (interactive nil)
  (when (ocamlformat--enable-indent)
    (setq-local indent-line-function #'ocamlformat-line)
    (setq-local indent-region-function #'ocamlformat-region)))

;;;###autoload
(defun ocamlformat-caml-mode-setup ()
  (ocamlformat-setup-indent)
  (local-unset-key "\t"))  ;; caml-mode rebinds TAB !

(defun ocamlformat-newline-and-indent (&optional arg)
  "Insert a newline, then indent according to `ocamlformat-line'.
With ARG, perform this action that many times."
  (interactive "*p")
  (delete-horizontal-space t)
  (unless arg
    (setq arg 1))
  (dotimes (_ arg)
    (newline nil t)
    (insert "x")
    (ocamlformat-line)
    (delete-char -1)))

(defun ocamlformat-set-newline-and-indent ()
  "Bind RET to `ocamlformat-newline-and-indent'."
  (when (ocamlformat--enable-indent)
    (local-set-key (kbd "RET") 'ocamlformat-newline-and-indent)))

(defun ocamlformat-version ()
  "Get the version of the installed ocamlformat."
  (car
   (split-string
    (shell-command-to-string
     (format "%s --version" (shell-quote-argument ocamlformat-command)))
    "-"
    t
    split-string-default-separators)))

(defun ocamlformat--add-hooks ()
  "Link ocamlformat with tuareg-mode and caml-mode."
  (progn
    (add-hook 'tuareg-mode-hook 'ocamlformat-setup-indent t)
    (add-hook 'tuareg-mode-hook 'ocamlformat-set-newline-and-indent)
    (add-hook 'caml-mode-hook 'ocamlformat-caml-mode-setup t)
    (add-hook 'caml-mode-hook 'ocamlformat-set-newline-and-indent)))

(pcase ocamlformat-enable
  ;; never hook
  ('disable '())
  ;; always hook
  ('enable-outside-detected-project (ocamlformat--add-hooks))
  ;; only hook if there is an .ocamlformat file at the root of the project
  ('enable
   (if (vc-find-root default-directory ".ocamlformat")
    (ocamlformat--add-hooks))))

(provide 'ocamlformat)

;;; ocamlformat.el ends here
