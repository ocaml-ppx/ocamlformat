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
 (name ocamlformat-lib)
 (synopsis "OCaml Code Formatter")
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
  (cmdliner
   (>= 1.1.0))
  dune
  dune-build-info
  either
  fix
  (fpath
   (>= 0.7.3))
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
  camlp-streams))

(package
 (name ocamlformat)
 (synopsis "Auto-formatter for OCaml code")
 (description
  "**ocamlformat** is a code formatter for OCaml. It comes with opinionated default settings but is also fully customizable to suit your coding style.\n\n- **Profiles:** ocamlformat offers profiles we predefined formatting configurations. Profiles include `default`, `ocamlformat`, `janestreet`.\n- **Configurable:** Users can change the formatting profile and configure every option in their `.ocamlformat` configuration file.\n- **Format Comments:** ocamlformat can format comments, docstrings, and even code blocks in your comments.\n- **RPC:** ocamlformat provides an RPC server that can be used by other tools to easily format OCaml Code.")
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
  (csexp
   (>= 1.4.0))
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
