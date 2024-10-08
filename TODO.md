## Conventional profile

`fun` indent at toplevel but not at tail

```
   let fmt_atrs = fmt_item_attributes c ~pre:(Break (1, 0)) atrs in
   let ctx = Ctf cf in
   (fun k ->
-    fmt_cmts_before
-    $ hvbox 0 ~name:"ctf"
-        (hvbox 0 (doc_before $ hvbox 0 k $ fmt_atrs $ doc_after)
-        $ fmt_cmts_after))
+  fmt_cmts_before
+  $ hvbox 0 ~name:"ctf"
+      (hvbox 0 (doc_before $ hvbox 0 k $ fmt_atrs $ doc_after) $ fmt_cmts_after))
   @@
```

Argument-list indentation at toplevel

```
let create =
  let drop_suffix name =
    if Sys.win32
    then Option.value ~default:name (String.drop_suffix name ~suffix:".exe")
    else name
  in
  fun (context : Context.t)
    ~(local_bins : origin Appendable_list.t Filename.Map.t Memo.Lazy.t) ->
    let local_bins =
      Memo.lazy_ (fun () ->
        let+ local_bins = Memo.Lazy.force local_bins in
        Filename.Map.to_list_map local_bins ~f:(fun name sources ->
          let sources = Appendable_list.to_list sources in
          drop_suffix name, Origin sources)
        |> Filename.Map.of_list_exn)
    in
    { context; local_bins }
```

Argument-list wrapping at toplevel

```
-  fun ~dir ~scope ~target_dir ~sctx ~requires_link ~mode
-      (mel : Melange_stanzas.Emit.t) ->
+  fun ~dir
+    ~scope
+    ~target_dir
+    ~sctx
+    ~requires_link
+    ~mode
+    (mel : Melange_stanzas.Emit.t)
+  ->
     let build_js = build_js ~sctx ~mode ~module_systems:mel.module_systems in
```

Indentation of `fun` in a match clause

```
let rec expression : Typedtree.expression -> term_judg =
  fun exp -> match exp.exp_desc with
    | Texp_ident (pth, _, _) ->
      path pth
    | Texp_let (rec_flag, bindings, body) ->
      (*
         G  |- <bindings> : m -| G'
         G' |- body : m
         -------------------------------
         G |- let <bindings> in body : m
      *)
      value_bindings rec_flag bindings >> expression body
    | Texp_letmodule (x, _, _, mexp, e) ->
      module_binding (x, mexp) >> expression e
    | Texp_match (e, cases, eff_cases, _) ->
      (* TODO: update comment below for eff_cases
         (Gi; mi |- pi -> ei : m)^i
         G |- e : sum(mi)^i
         ----------------------------------------------
         G + sum(Gi)^i |- match e with (pi -> ei)^i : m
       *)
      (fun mode ->
        let pat_envs, pat_modes =
          List.split (List.map (fun c -> case c mode) cases) in
        let env_e = expression e (List.fold_left Mode.join Ignore pat_modes) in
        let eff_envs, eff_modes =
          List.split (List.map (fun c -> case c mode) eff_cases) in
        let eff_e = expression e (List.fold_left Mode.join Ignore eff_modes) in
        Env.join_list
          ((Env.join_list (env_e :: pat_envs)) :: (eff_e :: eff_envs)))
```

## Janestreet profile

`function` after infix indentation

```
         Protocol.Init.read_input ic
         >>= (function
-         | `Version v when v = latest -> return (get_client ~pid ic oc v)
-         | `Version v ->
-           (match others with
-            | h :: _ when v = h -> return (get_client ~pid ic oc v)
-            | _ -> aux others)
-         | `Unknown -> aux others
-         | `Halt ->
-           return
-             (Error
-                (`Msg
-                    "OCamlFormat-RPC did not respond. Check that a compatible version of \
-                     the OCamlFormat RPC server (ocamlformat-rpc >= 0.18.0) is \
-                     installed.")))
+        | `Version v when v = latest -> return (get_client ~pid ic oc v)
+        | `Version v ->
+          (match others with
+           | h :: _ when v = h -> return (get_client ~pid ic oc v)
+           | _ -> aux others)
+        | `Unknown -> aux others
+        | `Halt ->
+          return
+            (Error
+               (`Msg
+                   "OCamlFormat-RPC did not respond. Check that a compatible version of \
+                    the OCamlFormat RPC server (ocamlformat-rpc >= 0.18.0) is installed.")))
     in
     aux versions
```

`fun -> function` argument

```
           List.fold_left
             (fun acc -> function
-               | List [ Atom name; Atom value ] -> (name, value) :: acc
-               | _ -> acc)
+              | List [ Atom name; Atom value ] -> (name, value) :: acc
+              | _ -> acc)
             []
             l
           |> List.rev
```

`fun` at toplevel but not tail

```
   let fmt_atrs = fmt_item_attributes c ~pre:(Break (1, 0)) atrs in
   let ctx = Ctf cf in
   (fun k ->
-    fmt_cmts_before
-    $ hvbox
-        0
-        ~name:"ctf"
-        (hvbox 0 (doc_before $ hvbox 0 k $ fmt_atrs $ doc_after) $ fmt_cmts_after))
+  fmt_cmts_before
+  $ hvbox
+      0
+      ~name:"ctf"
+      (hvbox 0 (doc_before $ hvbox 0 k $ fmt_atrs $ doc_after) $ fmt_cmts_after))
   @@
```

`fun` argument-list unwrapped at toplevel

```
         -> acc
         -> acc Lwt.t
       =
-      fun ~order ~force ~cache ~uniq ~pre ~post ~path ?depth ~node ~contents ~tree t acc ->
+      fun ~order
+        ~force
+        ~cache
+        ~uniq
+        ~pre
+        ~post
+        ~path
+        ?depth
+        ~node
+        ~contents
+        ~tree
+        t
+        acc ->
       let env = t.info.env in
```

Break after `->` at toplevel

```
-  fun s m -> List.fold_right (fun it env -> structure_item it m env) s.str_items Env.empty
+  fun s m ->
+  List.fold_right (fun it env -> structure_item it m env) s.str_items Env.empty
```
