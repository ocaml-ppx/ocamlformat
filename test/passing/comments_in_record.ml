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
