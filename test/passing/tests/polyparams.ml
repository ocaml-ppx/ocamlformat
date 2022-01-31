let poly1 (id : 'a. 'a -> 'a) = (id 3, id "three")

let poly2 : ('a. 'a -> 'a) -> int * string = fun id -> (id 3, id "three")

let poly3 : 'b. ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
 fun id x -> (id x, id (Some x))

let rec poly4 p (id : 'a. 'a -> 'a) =
  if p then poly4 false id else (id 4, id "four")

let rec poly5 : bool -> ('a. 'a -> 'a) -> int * string =
 fun p id -> if p then poly5 false id else (id 5, id "five")

let rec poly6 : 'b. bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
 fun p id x -> if p then poly6 false id x else (id x, id (Some x))
