(* Layout annotations work on type params, including when there are multiple
   type params. *)
type ('a : void) t1 = 'a

type ('b, 'a : immediate) t2 = 'a

type ('b, 'a : immediate64, 'c) t3 = 'a

type ('b, 'a : immediate, 'c : any) t4 = 'a

(* We don't reformat attributes on type parameters to layout annotations
   unless there is just one attribute and it's a layout. *)
type 'a[@immediate] [@foo] t5

type 'a[@foo] [@immediate] t6

type 'a[@baz] t7
