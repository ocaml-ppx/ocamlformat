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

module O : sig
  type t
end
with type t := t = struct
  let () = ()
end

module O : sig
  type t
end
with type t := t
 and type s := s = struct
  let () = ()
end

include struct
  (* a *)
end

include A (struct
  (* a *)
end)

let x : (module S) = (module struct end)

let x = (module struct end : S)

module rec A : (sig
  type t
end
with type t = int) = struct
  type t = int
end
