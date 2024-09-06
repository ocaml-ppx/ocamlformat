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

### Janestreet

```diff
let eval
      (type a)
      (type b)
      (type c)
      (bop : (a, b, c) binop)
      (x : a constant)
      (y : b constant)
  : c constant
  =
  match bop, x, y with
  | Eq, Bool x, Bool y -> Bool (if x then y else not y)
  | Leq, Int x, Int y -> Bool (x <= y)
  | Leq, Bool x, Bool y -> Bool (x <= y)
  | Add, Int x, Int y -> Int (x + y)
;;
```

```ocaml
let vexpr (type visit_action) : ('a, 'result, visit_action) context -> 'a -> visit_action
  = function
  | Local -> fun _ -> raise Exit
  | Global -> fun _ -> raise Exit
;;
```

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

```ocaml
let () =
  match () with
  | _ ->
    (fun _ : _ ->
      match () with
      | _ -> ())
  | _ -> ()
;;
```