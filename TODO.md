## On fait pas si trop dur:

```diff
 let f ssssssssss =
-  String.fold ssssssssss ~init:innnnnnnnnnit ~f:(fun accuuuuuuuuuum ->
-    function
+  String.fold ssssssssss ~init:innnnnnnnnnit
+    ~f:(fun accuuuuuuuuuum -> function
     | '0' -> g accuuuuuuuuuum
     | '1' -> h accuuuuuuuuuum
     | _ -> i accuuuuuuuuuum )
```

```diff
 let default =
   command##hasPermission #= (fun ctx -> foooooooooooooooooo fooooooooooo) ;
   command##hasPermission #= (fun ctx ->
-  foooooooooooooooooo fooooooooooo foooooo fooooooooo foooooooooo) ;
+  foooooooooooooooooo fooooooooooo foooooo fooooooooo foooooooooo ) ;
   foo
```

```diff
let ty_list : type a e. (a, e) ty -> (a vlist, e) ty =
 fun t ->
  let targ = Pair (Pop t, Var) in
  Rec
    (Sum
       ( (function
         | `Nil -> ("Nil", None) | `Cons p -> ("Cons", Some (Tdyn (targ, p)))
         )
       , function
         | "Nil", None -> `Nil
         | "Cons", Some (Tdyn (Pair (_, Var), (p : a * a vlist))) -> `Cons p
       ) )
```

### Janestreet

```ocaml
let () =
  very_long_function_name
    ~very_long_argument_label:
      (fun
        very_long_argument_name_one
        very_long_argument_name_two
        very_long_argument_name_three
      -> ())
;;
```

```ocaml
let _ =
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    ~bbbbbbbbbbbbbbbbbbbbbbbbbbbb:
      (fun
        (_ : (ccccccccccccc * ddddddddddddddddddddddddddddd) eeee) -> FFFFFFFFF gg)
    ~h
;;
```

```ocaml
let _ =
  let x = x in
  fun foooooooooooooooooo
    foooooooooooooooooo
    foooooooooooooooooo
    foooooooooooooooooo
    foooooooooooooooooo
    foooooooooooooooooo ->
    ()
;;
```
