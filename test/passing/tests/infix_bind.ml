;;
f x >>= fun y ->
g y >>= fun () ->
f x >>= fun y ->
g y >>= fun () ->
f x >>= fun y ->
g y >>= fun () -> y ()

;;
f x >>= function
| A -> (
    g y >>= fun () ->
    f x >>= fun y ->
    g y >>= function
    | x -> (
        f x >>= fun y ->
        g y >>= function _ -> y () ) )

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee |> fun x -> x

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
|> fun xxxxxx xxxxxxxxxx xxxxxxxx xxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeeee
  eeeeeeeeeeee eeeeeeeeee
|> fun x -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeeee
  eeeeeeeeeeee eeeeeeeeee
|> fun xxxxxxxxxxxxx xxxxxxxxxxxxxx xxxxxxxxxxxxxx xxxxxxxxx ->
xxxxxxxxxxx xxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxx

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee |> fun x -> x

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
|> fun xxxxxx xxxxxxxxxx xxxxxxxx xxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee |> fun xxxxxxxxx xxxxxxxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeee
|> fun xxxxxxxx xxxxxxxxx xxxxxxxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee |> fun xxxxxxxxxxxxx ->
xxxxxxxx xxxxxxxxxx xxxxxxxxx xxxxxxxxxxxxx

;;
eeeeeeeeeeeee eeeeeeeeee
|> fun xxxxxxxxxxxxx xxxxxxxxxxxxxx xxxxxxxxxxxxxx xxxxxxxxx ->
xxxxxxxxxxx xxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxx

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee |> function x -> x

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee |> function
| xxxxxx, xxxxxxxxxx, xxxxxxxx, xxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeeee
  eeeeeeeeeeee eeeeeeeeee
|> function
| x -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeeee
  eeeeeeeeeeee eeeeeeeeee
|> function
| xxxxxxxxxxxxx, xxxxxxxxxxxxxx, xxxxxxxxxxxxxx, xxxxxxxxx ->
    xxxxxxxxxxx xxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxx

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee |> function x -> x

;;
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee |> function
| xxxxxx, xxxxxxxxxx, xxxxxxxx, xxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee |> function xxxxxxxxx, xxxxxxxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee eeeeeeeeeeeeeeeeeee |> function
| xxxxxxxx, xxxxxxxxx, xxxxxxxxxxxxx -> x

;;
eeeeeeeeeeeee eeeeeeeeeeeeeeeeee |> function
| xxxxxxxxxxxxx -> xxxxxxxx xxxxxxxxxx xxxxxxxxx xxxxxxxxxxxxx

;;
eeeeeeeeeeeee eeeeeeeeee |> function
| xxxxxxxxxxxxx, xxxxxxxxxxxxxx, xxxxxxxxxxxxxx, xxxxxxxxx ->
    xxxxxxxxxxx xxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxx

let parens =
  match body with
  | {pexp_desc= Pexp_function cs; pexp_attributes; pexp_loc} ->
      update_config_maybe_disabled c pexp_loc pexp_attributes @@ fun c ->
      fmt "@ "
      $ Cmts.fmt c.cmts pexp_loc
          (wrap_if parens "(" ")"
             ( fmt "function"
             $ fmt_extension_suffix c ext
             $ fmt_attributes c ~key:"@" pexp_attributes
             $ close_box $ fmt "@ " $ fmt_cases c ctx cs ) )
  | _ ->
      close_box $ fmt "@ " $ fmt_expression c ~eol:(fmt "@;<1000 0>") xbody

let parens =
  match body with
  | {pexp_desc= Pexp_function cs; pexp_attributes; pexp_loc} -> (
      update_config_maybe_disabled c pexp_loc pexp_attributes @@ function
      | _ ->
          fmt "@ "
          $ Cmts.fmt c.cmts pexp_loc
              (wrap_if parens "(" ")"
                 ( fmt "function"
                 $ fmt_extension_suffix c ext
                 $ fmt_attributes c ~key:"@" pexp_attributes
                 $ close_box $ fmt "@ " $ fmt_cases c ctx cs ) )
      | _ ->
          close_box $ fmt "@ "
          $ fmt_expression c ~eol:(fmt "@;<1000 0>") xbody )

let end_gen_implementation ?toplevel ~ppf_dump
    (clambda : clambda_and_constants) =
  Emit.begin_assembly () ;
  ( clambda
  ++ Profile.record "cmm" (Cmmgen.compunit ~ppf_dump)
  ++ Profile.record "compile_phrases" (List.iter (compile_phrase ~ppf_dump))
  ++ fun () -> () ) ;
  fooooooooooooooo

let foo =
  (* get the tree origin *)
  get_store_tree s >>= function
  | None -> f t >|= fun x -> Ok x (* no transaction is needed *)
  | Some (origin, old_tree) ->
      let batch = {repo; tree= old_tree; origin} in
      let b = Batch batch in
      foo

let _ = foo >>= function[@warning "-4"] A -> false | B -> true

let _ =
  foo >>= function[@warning "-4"]
  | Afoooooooooooooooooo fooooooooo -> false
  | Bfoooooooooooooooooooooo fooooooooo -> true

let _ = foo >>= fun [@warning "-4"] x -> fooooooooooooooooooooooo

let _ =
  foo >>= fun [@warning "-4"] x y ->
  fooooooooooooooooooooooo fooooooooooooooooooooooo fooooooooooooooooooooooo
    fooooooooooooooooooooooo

let _ =
  foo >>= function(* foo before *) [@warning "-4"] (* foo after *)
  | Afoooooooooooooooooo fooooooooo -> false
  | Bfoooooooooooooooooooooo fooooooooo -> true

let _ =
  foo >>= fun (* foo before *) [@warning "-4"] (* foo after *) x ->
  fooooooooooooooooooooooo

let f = Ok () >>= (* *) fun _ -> Ok ()

let f =
  (* fooooooooooooooo foooooooooooooooo *)
  Ok () >>= (* *) fun _ ->
  Ok foooooooooooooooooooooooooooooooooooooooooooooooooo

let f = Ok () >>= (* *) function Foo -> Ok ()

let f =
  (* fooooooooooooooo foooooooooooooooo *)
  Ok () >>= (* *) function
  | Foo -> Ok foooooooooooooooooooooooooooooooooooooooooooooooooo

let f =
  Ok ()
  >>=
  (* fooooooooooooooo fooooooooooooooo fooooooooooooooo foooooooooooooooo *)
  fun foooooo fooooo foooo foooooo ->
  Ok foooooooooooooooooooooooooooooooooooooooooooooooooo

let f =
  Ok ()
  >>=
  (* fooooooooooooooo fooooooooooooooo fooooooooooooooo foooooooooooooooo *)
  function
  | Foo -> Ok foooooooooooooooooooooooooooooooooooooooooooooooooo

(** The tests below are testing a dropped comment with
    `--no-break-infix-before-func` *)

let _ = x |> fun y -> y (* *)

let _ = x |> function y -> y (* *)

let _ = match () with A -> ( x |> function y -> y (* *) ) | B -> ()

let _ = x |> function y -> ( function _ -> y (* A *) ) (* B *)

let _ = () (* This is needed here to avoid the comment above from moving *)

let encoder f =
  let field_encode = unstage (t f.ftype) in
  stagged @@ fun x k : t -> field_encode (f.fget x) k

let encoder f =
  let field_encode = unstage (t f.ftype) in
  stagged @@ fun x k : t -> field_encode (f.fget x) k
