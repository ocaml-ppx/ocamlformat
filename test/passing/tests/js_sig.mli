open! Core

(** First documentation comment. *)
exception First_exception

(** Second documentation comment. *)
exception Second_exception

[@@@ocamlformat "parse-docstrings=true"]
[@@@ocamlformat "wrap-comments=true"]

(** {e foooooooo oooooo oooo oooo ooooo oooo ooooo} {i fooooo ooooo ooo oooooo oo oooooo oooo} {b fooooooo oooooo oooooo oooooo oooooo ooooooo} *)

(** {e foooooooo oooooooooo ooooooooo ooooooooo} {{!some ref} fooooooooooooo
    oooooooo oooooooooo} {b fooooooooooooo oooooooooooo oooooo ooooooo} *)
