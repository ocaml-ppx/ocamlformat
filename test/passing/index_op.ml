let ( .?[] ) = Hashtbl.find_opt

let ( .@[] ) = Hashtbl.find

let ( .@[]<- ) = Hashtbl.add

let ( .@{} ) = Hashtbl.find

let ( .@{}<- ) = Hashtbl.add

let ( .@() ) = Hashtbl.find

let ( .@()<- ) = Hashtbl.add

let h = Hashtbl.create 17

;;
h.@("One") <- 1 ;
assert (h.@{"One"} = 1) ;
print_int (h.@{"One"}) ;
assert (h.?["Two"] = None)

(* from GPR#1392 *)
let ( #? ) x y = (x, y)

let ( .%() ) x y = x.(y)

let x = [|0|]

let _ = 1 #? (x.(0))

let _ = 1 #? (x.%(0))

;;
a.[b].[c]

;;
a.[b.[c]].[c]

;;
a.b.c

let _ = s.{1}

let _ = s.{1} <- 1

let _ = s.{1, 2}

let _ = s.{1, 2} <- 1

let _ = s.{1, 2, 3}

let _ = s.{1, 2, 3} <- 1

let _ = s.{1, 2, 3, 4}

let _ = s.{1, 2, 3, 4} <- 1

let _ = Bigarray.Genarray.get s 1 [||]

let _ = Bigarray.Genarray.get s [|1|]

let _ = Bigarray.Genarray.get s [|1; 2|]

let _ = Bigarray.Genarray.get s [|1; 2; 3|]

let _ = s.{1, 2, 3, 4}

let _ = Bigarray.Genarray.set s [||] 1

let _ = Bigarray.Genarray.set s [|1|] 1

let _ = Bigarray.Genarray.set s [|1; 2|] 1

let _ = Bigarray.Genarray.set s [|1; 2; 3|] 1

let _ = s.{1, 2, 3, 4} <- 1
