(lang dune 2.8)

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

(name ocamlformat-0_26_1)

(using menhir 2.1)

(cram enable)

(generate_opam_files true)

(authors
 "Josh Berdine <jjb@fb.com>"
 "Hugo Heuzard <hugo.heuzard@gmail.com>"
 "Etienne Millon <etienne@tarides.com>"
 "Guillaume Petiot <guillaume@tarides.com>"
 "Jules Aguillon <jules@j3s.fr>")

(maintainers
 "Guillaume Petiot <guillaume@tarides.com>"
 "Jules Aguillon <jules@j3s.fr>"
 "Emile Trotignon <emile@tarides.com>")

(source
 (github ocaml-ppx/ocamlformat))

(package
 (name ocamlformat-0_26_1)
 (synopsis "ocamlformat executable for version 0.26.1")
 (description
  "**ocamlformat** is a code formatter for OCaml.\nThis package provides an executable for compatibility with ocamlformat 0.26.1.\nDo not install without the package `ocamlformat` installed.")
 (depends
  (ocaml
   (>= 4.08))
  (alcotest
   (and
    :with-test
    (>= 1.3.0)))
  (base
   (>= v0.12.0))
  dune
  dune-build-info
  either
  fix
  fpath
  (menhir
   (>= 20201216))
  (menhirLib
   (>= 20201216))
  (menhirSdk
   (>= 20201216))
  (ocaml-version
   (>= 3.5.0))
  (ocp-indent
   (or
    (and
     (= :with-test false)
     (>= 1.8.0))
    (and
     :with-test
     (>= 1.8.1))))
  stdio
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))
  (csexp
   (>= 1.4.0))
  ; for vendored odoc-parser
  astring
  result
  camlp-streams
  (cmdliner
   (or
    (and
     (= :with-test false)
     (>= 1.1.0))
    (and
     :with-test
     (>= 1.2.0))))))
