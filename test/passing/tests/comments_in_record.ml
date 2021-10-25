type t =
  { a: int
  (* some comment *)
  ; b: float
  ; c: string
  ; d: [`something_looooooooooooooooooooooooooooooooong] }

type t =
  { a: int
  (** some comment *)
  ; b: float
  ; c: string
  ; d: [`something_looooooooooooooooooooooooooooooooong] }

type t = { a : int (* Comment *); b : int (* Comment *) }

let { (* cmts *)
      pat
    ; loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
    ; a
    ; (* b *) b
    ; (* c *) c
    ; d=
      (* d *)
      (D: loooooooooooooooooooooooooooooooooooooooooooooooooooooooong_int)
    ; (* e *)
      e : loooooooooooooooooooooooooooooooooooooooooooooooooooooooong_int } =
  exp

let x =
  { (* Xxxx xxxxxxxx xxxxx xx xx xx xxxx xxxxxx - XXxx_xxxxx xxx'x. *)
    Irure_sed_a.in_nisi_sed= Irure_sed_fugiat.LaboRum sint_sed
  ; in_ea_deserunt= nulla }

type t =
  { a :
      int option
      (* aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc ddddddddddddddddd eeeee *)
  ; b : float
        (* aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc ddddddddddddddddd eeeee *)
  }

type t =
  | Tuple of {elts: t vector; packed: bool}
  | Struct of
      { name: string
      ; elts: t vector (* possibly cyclic, name unique *)
            [@compare.ignore] [@equal.ignore] [@sexp_drop_if fun _ -> true]
      ; elts: t vector
            (* possibly cyclic, name unique *)
            (* mooooooooooooooooooooooooooooooooooore comments *)
            [@compare.ignore] [@equal.ignore] [@sexp_drop_if fun _ -> true]
      ; packed: bool }
  | Opaque of {name: string}
[@@deriving compare, equal, hash, sexp]

type t = {
  (* c *) c (* c' *) : (* d *) d (* d' *)
}

let _ = {
  (* a *) a (* a' *) = (* b *) b (* b' *);
  (* c *) c (* c' *) : (* d *) d (* d' *) = (* e *) e (* e' *);
  (* f *) f (* f' *);
  (* g *) g (* g' *) = (* j *) ( (* h *) h (* h' *) : (* i *) i (* i' *) ) (* j' *)
}

let {
  (* a *) a (* a' *) = (* b *) b (* b' *);
  (* c *) c (* c' *) : (* d *) d (* d' *) = (* e *) e (* e' *);
  (* f *) f (* f' *);
  (* g *) g (* g' *) = (* j *) ( (* h *) h (* h' *) : (* i *) i (* i' *) ) (* j' *)
} = x
