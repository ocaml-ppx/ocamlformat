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
-let rec assoc : type a. string -> a rep -> assoc list -> a =
-  fun x r -> function
+let rec assoc : type a. string -> a rep -> assoc list -> a = fun x r -> function
   | [] -> raise Not_found
   | Assoc (x', r', v) :: env ->
     if x = x'
```

### Janestreet

```diff
   Sum
     ( (function
-        | `A n -> "A", Some (Tdyn (Int, n))
-        | `B s -> "B", Some (Tdyn (String, s))
-        | `C -> "C", None)
+       | `A n -> "A", Some (Tdyn (Int, n))
+       | `B s -> "B", Some (Tdyn (String, s))
+       | `C -> "C", None)
     , function
```


```diff
 let x =
   some_value
   |> some_fun (fun x ->
-    do_something ();
-    do_something_else ();
-    return_this_value)
+       do_something ();
+       do_something_else ();
+       return_this_value)
 ;;
```

```diff
   foo
   |> List.double_map
        ~f1:(fun x ->
-         do_something ();
-         do_something ();
-         do_something ();
-         do_something ();
-         do_something_else ())
+           do_something ();
+           do_something ();
+           do_something ();
+           do_something ();
+           do_something_else ())
        ~f2:(fun x ->
-         do_something ();
-         do_something ();
-         do_something ();
-         do_something ();
-         do_something_else ())
+           do_something ();
+           do_something ();
+           do_something ();
+           do_something ();
+           do_something_else ())
   |> bar
 ;;
```

```diff
   match () with
   | _ ->
     (fun _ : _ ->
-      match () with
-      | _ -> ())
+       (match () with
+        | _ -> ()))
   | _ -> ()
 ;;
```

```diff
 let _ =
   aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     ~bbbbbbbbbbbbbbbbbbbbbbbbbbbb:
-      (fun
-        (_ : (ccccccccccccc * ddddddddddddddddddddddddddddd) eeee) -> FFFFFFFFF gg)
+      (fun (_ : (ccccccccccccc * ddddddddddddddddddddddddddddd) eeee)
+    -> FFFFFFFFF gg)
     ~h
 ;;
```