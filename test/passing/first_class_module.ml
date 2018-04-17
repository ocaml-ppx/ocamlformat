module type S = sig
  
end

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

module type S = sig
  type a

  val va : a

  type b

  val vb : b
end

let f (module M : S with type a = int and type b = int) = M.va + M.vb

module type M = sig
  val storage : (module S with type t = t)
end
