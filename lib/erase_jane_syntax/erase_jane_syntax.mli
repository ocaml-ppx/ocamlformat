(** Whether or not Jane Street-specific syntax should be erased. Anything in
    vendor/parser-extended that could generate one of the Jane
    Street-specific constructors we add to the extended parsetree needs to
    check [should_erase] and, if true, generate the corresponding "plain"
    version, as per the compiler's [Jane_syntax] construction.

    When the user does request erasure, the result of ocamlformat {e will}
    modify the parse tree, but will also be parseable by upstream OCaml; we
    validate that the resulting parse tree is only modified to the point of
    removing Jane Street-specific syntax.

    This is done as a separate library so that it can be availble from the
    parser-extended Menhir parser, which we can't pass a local boolean flag
    into. This library is then depended on by everything that needs to know
    about this global state. While on the ocamlformat side, we could put this
    in the configuration type, that would not work for the Menhir parser;
    thus, we take this somewhat more invasive approach. *)

val set_should_erase : bool -> unit
(** Toggle whether Jane Street specific parse tree components ought to be
    erased from parsing/printing: [true] if they should be erased (so that
    the parse tree will be modified by ocamlformat), [false] if they should
    not. *)

val should_erase : unit -> bool
(** Check whether Jane Street specific parse tree components ought to be
    erased from parsing/printing: [true] if they should be erased (so that
    the parse tree will be modified by ocamlformat), [false] if they should
    not. *)
