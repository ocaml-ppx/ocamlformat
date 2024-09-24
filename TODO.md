## Conventional

Arrow changed position:

```
   let sorted_values =
     List.sort
       ~compare:(fun
-          { Diagnostic.location = loc1 } { Diagnostic.location = loc2 } ->
-        Option.compare Location.compare loc1 loc2)
+                 { Diagnostic.location = loc1 }
+                 { Diagnostic.location = loc2 }
+      -> Option.compare Location.compare loc1 loc2)
       values
   in
   List.map
```

Function cases changed indentation

```
          | EArr l ->
   EArr
     (List.map l ~f:(function
-              | ElementHole -> ElementHole
-              | Element e -> Element (m#expression e)
-              | ElementSpread e -> ElementSpread (m#expression e)))
+               | ElementHole -> ElementHole
+               | Element e -> Element (m#expression e)
+               | ElementSpread e -> ElementSpread (m#expression e)))
```

Disambiguating parentheses not needed for `function` after infix:

```
   let print_merlin_conf ~selected_context file =
     to_local ~selected_context file
-    >>| (function
+    >>| ( function
           | Error s -> Merlin_conf.make_error s
-          | Ok file -> load_merlin_file file)
+          | Ok file -> load_merlin_file file )
     >>| Merlin_conf.to_stdout
```

Function cases started wrapping after infix:

```
     let try_run_and_capture ?cwd prog args =
       run_process ?cwd prog args ~split:true >>| function
-      | Ok x -> Some x
-      | Error _ -> None
+      | Ok x -> Some x | Error _ -> None
```

Argument list of a `fun` started breaking:

```
       let new_specialised_args =
         Variable.Map.mapi
-          (fun new_inner_var (definition : Definition.t) :
-               Flambda.specialised_to ->
+          (fun
+            new_inner_var
+            (definition : Definition.t)
+            :
+            Flambda.specialised_to
+          ->
```

Argument list of a `fun` changed indentation:

```
     method class_infos : 'a. ('a -> 'res) -> 'a class_infos -> 'res =
-      fun _a
-          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
+      fun
+        _a
+        { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
+      ->
         let pci_virt = self#virtual_flag pci_virt in
         let pci_params =
           self#list
```

## Janestreet

AST changed on `code/ocaml/testsuite/tests/runtime-objects/toplevel_lets.ml`.
