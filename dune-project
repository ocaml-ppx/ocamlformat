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

(name ocamlformat)

(using menhir 2.1)

(cram enable)

(generate_opam_files true)

(authors "Josh Berdine <jjb@fb.com>")

(maintainers "OCamlFormat Team <ocamlformat-dev@lists.ocaml.org>")

(source
 (github ocaml-ppx/ocamlformat))

(package
 (name ocamlformat)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
 (depends
  (ocaml
   (>= 4.08))
  (alcotest :with-test)
  (base
   (>= v0.12.0))
  (cmdliner
   (>= 1.1.0))
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
   (>= 3.3.0))
  (ocamlformat-rpc-lib
   (and
    :with-test
    (= :version)))
  ocp-indent
  (re
   (>= 1.10.3))
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
  camlp-streams))

(package
 (name ocamlformat-bench)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
 (depends
  (ocaml
   (>= 4.08))
  (alcotest :with-test)
  (bechamel
   (>= 0.2.0))
  (bechamel-js
   (>= 0.2.0))
  (ocamlformat
   (= :version))
  stdio
  yojson))

(package
 (name ocamlformat-rpc-lib)
 (synopsis "Auto-formatter for OCaml code (RPC mode)")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style. This package defines a RPC interface to OCamlFormat")
 (license MIT)
 (depends
  (ocaml
   (>= 4.08))
  (csexp
   (>= 1.4.0))))
