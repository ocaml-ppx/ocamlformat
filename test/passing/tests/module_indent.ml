module type M = sig
  type t

  val f : (string * int) list -> int

  module type Inner = sig
    type inner

    val f : int -> int
  end
end

module A : sig
  type t

  val f : (string * int) list -> int
end = struct
  type t

  let f s l = 0
end

module Core = struct
  module Int = struct
    module T = struct
      type t = int

      let compare = compare
      let ( + ) x y = x + y
    end

    include T
    module Map = Map.Make (T)
  end

  module Std = struct
    module Int = Int
  end
end

