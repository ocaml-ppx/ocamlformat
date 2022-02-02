let ( .?[] ) = Hashtbl.find_opt

let ( .@[] ) = Hashtbl.find

let ( .@[]<- ) = Hashtbl.add

let ( .@{} ) = Hashtbl.find

let ( .@{}<- ) = Hashtbl.add

let ( .@() ) = Hashtbl.find

let ( .@()<- ) = Hashtbl.add

let h = Hashtbl.create 17 ;;

h.@("One") <- 1 ;
assert (h.@{"One"} = 1) ;
print_int h.@{"One"} ;
assert (h.?["Two"] = None)

(* from GPR#1392 *)
let ( #? ) x y = (x, y)

let ( .%() ) x y = x.(y)

let x = [|0|]

let _ = 1 #? x.(0)

let _ = 1 #? x.%(0) ;;

a.[b].[c] ;;

a.[b.[c]].[c] ;;

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

let () =
  let m = Mat.zeros 5 5 in
  m.Mat.${[[2]; [5]]} |> ignore ;
  let open Mat in
  m.${[[2]; [5]]} |> ignore

let _ = (x.*{(y, z)} <- w) @ []

let _ = (x.{y, z} <- w) @ []

let _ = (x.*(y) <- z) @ []

let _ = (x.*(y) <- z) := []

let _ = ((x.*(y) <- z), [])

let _ = x.*(y) <- z ; []

let _ = (x.(y) <- z) @ []

let _ = (x.(y) <- z) := []

let _ = ((x.(y) <- z), [])

let _ = x.(y) <- z ; []

let _ = (x.y <- z) @ []

let _ = (x.y <- z) := []

let _ = ((x.y <- z), [])

let _ =
  x.y <- z ;
  []

let _ = x.(y) <- (z.(w) <- u)

let _ = x.foo#m

class free =
  object (m : 'test)
    method get_def = m#state.def
  end

(* With path *)
let _ = a.A.B.*(b) ; a.A.B.*(b) <- c

let _ = a.*((a ; b))

let _ = a.*([|a; b|])

(* Avoid unecessary parentheses *)
let _ =
  match a with
  | A -> a.*(match b with B -> b)
  | B -> a.*(match b with B -> b) <- D
  | C -> ()

let _ = if a then a.*(if a then b) else c

(* Parentheses needed *)
let _ = a.*{(a ; b)}

let _ = a.{(a ; b)}

let _ = a.{a, b}

(* Integers on the left of indexing operators must be surrounded by
   parentheses *)
let _ = (0).*(0)

(* Integers with suffix and floats are fine *)
let _ = 0l.*(0)

let _ = 0..*(0)

let _ = 0.2.*(0)

let _ = 2e5.*(0)

let _ = 2e-2.*(0)

let _ = (String.get [@bar]) filename (len - 1) = 'i'

let _ = "hello world".[-8]

let _ = String.get "hello world" (-8)

let _ = String.unsafe_get "hello world" (-8)

let _ = [||].(-8)

let _ = Array.get [||] (-8)

let _ = Array.unsafe_get [||] (-8)

let _ = Bigarray.Genarray.get x [||] (-8)

let _ = Bigarray.Genarray.unsafe_get x [||] (-8)

let _ = [%p (Some).(tickers)]

let _ = [%p (Explicit).(0 / 2)]

let _ = [%p Some.(tickers)]

let _ = [%p Explicit.(0 / 2)]

let _ = (Some).(tickers)

let _ = (Explicit).(0 / 2)

let _ = Some.(tickers)

let _ = Explicit.(0 / 2)

let _ = f (Some).(tickers)

let _ = f (Explicit).(0 / 2)

let _ = f Some.(tickers)

let _ = f Explicit.(0 / 2)
