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
  "**ocamlformat** is a code formatter for OCaml.\nThis package provides an executable for compatibility with ocamlformat 0.26.1.\nDo not install without the package `ocamlformat` installed."))
