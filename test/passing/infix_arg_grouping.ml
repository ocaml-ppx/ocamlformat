;;
vbox 1
  ( str (Sexp.to_string_hum (Itv.sexp_of_t root))
  $ wrap_if (not (List.is_empty children)) "@,{" " }" (dump_ tree children)
  )

;;
user_error
  ( "version mismatch: .ocamlformat requested " ^ value ^ " but version is "
  ^ Version.version )

;;
hvbox 1
  ( str "\""
  $ list_pn lines (fun ?prev curr ?next ->
        let drop = function ' ' -> true | _ -> false in
        let line =
          if Option.is_none prev then curr else String.lstrip ~drop curr
        in
        fmt_line line
        $ opt next (fun next ->
              let spc =
                match String.lfindi next ~f:(fun _ c -> not (drop c)) with
                | Some 0 -> ""
                | Some i -> escape_string (String.sub next 0 i)
                | None -> escape_string next
              in
              fmt "\\n"
              $ fmt_if_k
                  (not (String.is_empty next))
                  (str spc $ pre_break 0 "\\" 0)))
  $ str "\"" $ Option.call ~f:epi )

;;
hvbox 0
  (wrap_fits_breaks "<" ">"
     ( list fields "@ ; " (function
         | Otag (lab_loc, attrs, typ) ->
             (* label loc * attributes * core_type -> object_field *)
             let doc, atrs = doc_atrs attrs in
             let fmt_cmts = Cmts.fmt c lab_loc.loc in
             fmt_cmts
             @@ hvbox 4
                  ( hvbox 2
                      ( Cmts.fmt c lab_loc.loc @@ str lab_loc.txt
                      $ fmt ":@ "
                      $ fmt_core_type c (sub_typ ~ctx typ) )
                  $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc
                  $ fmt_attributes c (fmt " ") ~key:"@" atrs (fmt "") )
         | Oinherit typ -> fmt_core_type c (sub_typ ~ctx typ))
     $ fmt_if
         Poly.(closedness = Open)
         (match fields with [] -> "@ .. " | _ -> "@ ; .. ") ))

;;
hvbox 0
  ( fmt "functor@ "
  $ wrap "(" ")"
      ( str txt
      $ opt mt (fun _ ->
            fmt "@ : " $ Option.call ~f:pro_t $ psp_t $ fmt "@;<1 2>"
            $ bdy_t $ esp_t $ Option.call ~f:epi_t) )
  $ fmt " ->@ " $ Option.call ~f:pro_e $ psp_e $ bdy_e $ esp_e
  $ Option.call ~f:epi_e )

let to_json {integers; floats; strings} =
  `Assoc
    [ ("int", yojson_of_integers integers)
    ; ("double", yojson_of_floats floats)
    ; ("normal", yojson_of_strings strings) ]
  |> Yojson.Basic.to_string

let rename (us, q) sub =
  ( Var.Set.union
      (Var.Set.diff us (Var.Subst.domain sub))
      (Var.Subst.range sub)
  , rename_q q sub )
  |> check invariant

let _ =
  List.map ~f
    ( [ aaaaaaaaaaaaaaa
      ; bbbbbbbbbbbbbbb
      ; ccccccccccccccc
      ; ddddddddddddddd
      ; eeeeeeeeeeeeeee ]
    @ l )

let sigma_seed =
  create_seed_vars
    ( (* formals already there plus new ones *)
      prop.Prop.sigma @ sigma_new_formals )

;;
match
  "\"" ^ line ^ " \""
  |> (* split by whitespace *)
     Str.split (Str.regexp_string "\" \"")
with
| prog :: args -> fooooooooooooooooooooo

let () =
  (* Open the repo *)
  initialise
  >>= (* Perform a subsequent action *)
      subsequent_action
  >|= (* Keep going... *)
      another_action
  |> fun t ->
  (* And finally do this *)
  final_action t
