## Conventional profile

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

## Janestreet profile
