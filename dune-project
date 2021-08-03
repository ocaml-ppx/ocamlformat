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
   (and
    (>= 4.08)
    (< 4.14)))
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
   (>= 20201216))
  (menhirLib
   (>= 20201216))
  (menhirSdk
   (>= 20201216))
  (ocaml-version
   (>= 3.2.0))
  ocp-indent
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (odoc-parser
   (>= 0.9.0))
  (re
   (>= 1.7.2))
  (stdio
   (< v0.15))
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))))

(package
 (name ocamlformat-rpc)
 (synopsis "Auto-formatter for OCaml code (RPC mode)")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style. This package defines a RPC interface to OCamlFormat")
 (depends
  (ocaml
   (and
    (>= 4.08)
    (< 4.14)))
  ocamlformat-rpc-lib
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
   (>= 20201216))
  (menhirLib
   (>= 20201216))
  (menhirSdk
   (>= 20201216))
  (ocaml-version
   (>= 3.2.0))
  ocp-indent
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (odoc-parser
   (>= 0.9.0))
  (re
   (>= 1.7.2))
  (stdio
   (< v0.15))
  (uuseg
   (>= 10.0.0))
  (uutf
   (>= 1.0.1))))

(package
 (name ocamlformat-rpc-lib)
 (synopsis "Auto-formatter for OCaml code (RPC mode)")
 (description
  "OCamlFormat is a tool to automatically format OCaml code in a uniform style. This package defines a RPC interface to OCamlFormat")
 (license MIT)
 (depends
  (ocaml
   (and
    (>= 4.08)
    (< 4.14)))
  csexp
  sexplib0))
