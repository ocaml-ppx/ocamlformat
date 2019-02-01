module AAAAAAAAAAAAAAAAAAA =
  Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod

let _ =
  let module A = B in
  let module AAAAAAAAAAAAAAAAAAA =
    Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod
  in
  t

let create (type a b) t i w p =
  let module T = (val (t : (a, b) t)) in
  T.create i w p

module C = struct
  module rec A : sig
    type t

    module rec B : sig
      type t

      type z
    end

    and A : B
  end =
    A

  and B : sig end = B
end

module M :
  S
  with type long_long_type = t
   and type long_long_long_type = u
   and type very_long_long_type = t =
  M

module type X = sig
  val n : (module S with type t = t)

  val o :
    (module
     S
       with type long_long_type = t
        and type long_long_long_type = u
        and type very_long_long_type = t)
end

module X = (val X.n : S with type t = t)

module Y = ( val X.o
               : S
               with type long_long_type = t
                and type long_long_long_type = u
                and type very_long_long_type = t )

let _ =
  (module M
  : S
    with type long_long_type = t
     and type long_long_long_type = u
     and type very_long_long_type = t )

let _ =
  ( module struct
    type t
  end
  : S
    with type t = t )

let _ =
  ( module struct
    type t
  end
  : S )

let _ =
  ( module struct
    type t
  end
  : Loooooooooooooooonnnnnnnnnnnnnnnng.Looooooooooooooonnnnnnnnnnnnnng.Mod
  )

let _ =
  ( module struct
    type t
  end
  : Loooooooooooooooonnnnnnnnnnnnnnnng.Looooooooooooooonnnnnnnnnnnnnng.Mod
    with type looooooooonnnnnnng_type = t and type not_long_type = t' )

let _ =
  ( module struct
    (* a *)
    type t
  end
  (* b *)
  : S
  (* c *)
    with type t = t (* d *) )

let _ =
  ( module struct
    type t
  end
  : S
    with type long_long_type = t
     and type long_long_long_type = u
     and type very_long_long_type = t )
