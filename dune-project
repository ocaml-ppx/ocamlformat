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
 (name ocamlformat-lib)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
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
  (ocamlformat-rpc-lib
   (and
    :with-test
    (= :version)))
  (ocp-indent
   (>= 1.8.0))
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
 (name ocamlformat)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
 (depends
  (ocaml
   (>= 4.08))
  (cmdliner
   (or
    (and
     (= :with-test false)
     (>= 1.1.0))
    (and
     :with-test
     (>= 1.2.0))))
  dune
  (ocamlformat-lib
   (= :version))
  (ocamlformat-rpc-lib
   (and
    :with-test
    (= :version)))
  (re
   (>= 1.10.3))))

(package
 (name ocamlformat-bench)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
 (depends
  (ocaml
   (>= 4.08))
  (alcotest
   (and
    :with-test
    (>= 1.3.0)))
  (bechamel
   (>= 0.2.0))
  (bechamel-js
   (>= 0.2.0))
  (ocamlformat-lib
   (= :version))
  stdio
  (yojson
   (>= 1.6.0))))

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
