let f (type v) (x: v) = x

let f (type v) (x: v) : unit = ()

let f : type s. s t -> s = function X x -> x | Y y -> y

let x = (fun (type a) x -> x) ()

let x = (fun x (type a b c) x -> x) ()

let f = function T x -> (fun (type a) (x: a t) -> x) x
