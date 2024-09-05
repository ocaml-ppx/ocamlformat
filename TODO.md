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
