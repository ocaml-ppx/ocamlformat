(lang dune 2.7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                              OCamlFormat                               ;
;                                                                        ;
;            Copyright (c) Facebook, Inc. and its affiliates.            ;
;                                                                        ;
;      This source code is licensed under the MIT license found in       ;
;      the LICENSE file in the root directory of this source tree.       ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(name ocamlformat)

(using menhir 2.1)

(cram enable)

(generate_opam_files true)

(authors "Josh Berdine <jjb@fb.com>")

(maintainers "OCamlFormat Team <ocamlformat-team@fb.com>")

(license MIT)

(source
 (github ocaml-ppx/ocamlformat))

(package
 (name ocamlformat)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
 (depends
  (ocaml
   (and
    (>= 4.08)
    (< 4.13)))))
