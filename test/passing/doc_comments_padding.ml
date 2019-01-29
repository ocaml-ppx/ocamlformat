type t = {a: int  (** a *); b: int  (** b *)}

type t = < a: int  (** a *) ; b: int  (** b *) >

type t = [`a of int  (** a *) | `b of int  (** b *)]

type t = A of int  (** a *) | B of int  (** b *)

type t += A of int  (** a *) | B of int  (** b *)

[@@@ocamlformat "doc-comments-padding=1"]

type t = {a: int (** a *); b: int (** b *)}

type t = < a: int (** a *) ; b: int (** b *) >

type t = [`a of int (** a *) | `b of int (** b *)]

type t = A of int (** a *) | B of int (** b *)

type t += A of int (** a *) | B of int (** b *)
