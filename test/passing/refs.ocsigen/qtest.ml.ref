(*$T
  false
*)

(*$T foo
  foo  0 ( + ) [1;2;3] = 6  (* hehe *)
  foo  0 ( * ) [1;2;3] = 0  (* haha (*hoho *) *)
  foo  1 ( * ) [4;5]   = 20
  foo 12 ( + ) []      = 12
*)

(*$T foo
  foo 1 ( * ) [4;5] = foo 2 ( * ) [1;5;2]
*)

(*$= foo & ~printer:string_of_int
  (foo 1 ( * ) [4;5]) (foo 2 ( * ) [1;5;2])
*)

(*$Q foo
  Q.small_int (fun i-> foo i (+) [1;2;3] = List.fold_left (+) i [1;2;3])
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
*)

(*$R foo
  let thing = foo  1 ( * )
  and li = [4;5] in
  assert_bool "something_witty" (thing li = 20);
   (* pertinent comment *)
  assert_bool "something_wittier" (1=1)
*)

(*$inject let brom = baz *)
(*$T brom
  brom.[2] = 'z'
*)

(*$T &
             1    = 2-1
            2+3 \
              = \
              \
              5
  
  1+1=2
*)

(*$T & 6 \
  & =
  2*3
*)

(*$Q & ~count:10
  (Q.small_int_corners ()) (fun n-> n+3 -2 -1 = abs n)
*)

(*$Q & ~max_gen:1000000 ~count:1000000
    (Q.make (fun _ -> ())) (fun () -> true)
*)
