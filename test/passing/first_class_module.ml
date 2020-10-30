module type S = sig end

type t = (module S)

module type S = sig
  val x : int
end

module M = struct
  let x = 0
end

let m = (module M : S)

let () =
  let (module M : S) = m in
  (* error here *)
  ()

module type S = sig
  val x : int
end

module M = struct
  let x = 0
end

let m = (module M : S)

let f ((module M : S) as u) = ignore u ; M.x

let f (T {m= (module M)}) = ignore u ; M.x

let f (T {m= (module M : S)}) = ignore u ; M.x

let v = f (module M : S with type t = t)

module type S = sig
  type a

  val va : a

  type b

  val vb : b
end

let f (module M : S with type a = int and type b = int) = M.va + M.vb

let f (module M : S with type a = int and type b = int)
    (module N : SSSS
      with type a = int
       and type b = int
       and type c = int
       and type d = int
       and type e = int )
    (module N : SSSS
      with type a = int
       and type b = int
       and type c = int
       and type d = int )
    (module O : S with type a = int and type b = int and type c = int) =
  M.va + N.vb

module type M = sig
  val storage : (module S with type t = t)
end

let _ =
  let module M = (val m : M) in
  ()

let _ =
  ( module Ephemeron (HHHHHHHHHHHHHHHHHHHHHHHHHH) (HHHHHHHHHHHHHHHHHHHHHHHHHH)
  : Ephemeron.S )

let _ =
  ( module Ephemeron (HHHHHHHHHHHHHHHHHHHHHHHHHH) (HHHHHHHHHHHHHHHHHH)
  : Ephemeron.S )

let _ = (module Ephemeron (HHHHHHHHHHHHHHH) (HHHHHHHHHHHHH) : Ephemeron.S)

let _ = (module Ephemeron (HHH) : Ephemeron.S)

let _ =
  ( module Ephemeron (struct
    type t = t
  end) : Ephemeron.S )

let _ =
  ( module struct
    let a = b
  end )

(* Tests for dropped comment *)

module M = (val x : S (* a *))

module M = (val x (* b *))

[@@@ocamlformat "break-struct=natural"]

let _ =
  ( module struct
    let x = 0

    let y = 1
  end )
