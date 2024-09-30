## Conventional

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
