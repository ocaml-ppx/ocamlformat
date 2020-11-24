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
    (< 4.13)))
  (ocaml
   (and
    :with-test
    (< 4.12)))
  ocaml-version
  (alcotest :with-test)
  (base
   (and
    (>= v0.12.0)
    (< v0.15)))
  base-unix
  cmdliner
  dune-build-info
  fix
  fpath
  (menhir
   (>= 20180528))
  (menhirLib
   (>= 20200624))
  (menhirSdk
   (>= 20200624))
  (ocaml-migrate-parsetree
   (>= 2.1.0))
  (ocp-indent :with-test)
  (odoc
   (>= 1.4.2))
  (ppxlib
   (>= 0.18.0))
  re
  (stdio
   (< v0.15))
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))))
