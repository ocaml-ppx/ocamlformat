type t =
  { (* fooooooooooooooooooooooooooooooooooooooooooooooooooooooo *)
    foooooooooooooooooooooooo: foooooooooooooooooooooooooooooooooooooooo
        (* foooooooooooooooooooooooooooooooooooooooooooo *)
  ; fooooooooooooooooooooooooooooo: fooooooooooooooooooooooooooo }

type x =
  | B of
      { (* fooooooooooooooooooooooooooooooooooooooooo *)
        aaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaa
            (* foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*)
      ; bbbbbbbbbbbbbbbbbbbbbbb: bbbbbbbbbbbbbbb }

type t =
  { aaaaaaaaaaaaaaaaaaaaaaaaa: aaaa aaaaaaaaaaaaaaaaaaa
  ; bbbbbbbbbbbbbbbbbbbbbbbbb: bbbbbbbbbbbb bbbb
  ; cccccccccccccccccccccc: ccccccc ccccccccccc cccccccc }

type x =
  | B of
      { aaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaa
      ; bbbbbbbbbbbbbbbbbbbbbbb: bbbbbbbbbbbbbbb }

type t =
  { break_cases: [`Fit | `Nested | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; break_separators: bool
  ; break_sequences: bool
  ; break_string_literals: [`Newlines | `Never | `Wrap]
        (** How to potentially break string literals into new lines. *)
  ; break_struct: bool
  ; cases_exp_indent: int
  ; comment_check: bool
  ; disable: bool
  ; doc_comments: [`Before | `After]
  ; escape_chars: [`Decimal | `Hexadecimal | `Preserve]
        (** Escape encoding for chars literals. *)
  ; escape_strings: [`Decimal | `Hexadecimal | `Preserve]
        (** Escape encoding for string literals. *)
  ; extension_sugar: [`Preserve | `Always]
  ; field_space: [`Tight | `Loose]
  ; if_then_else: [`Compact | `Keyword_first]
  ; indicate_multiline_delimiters: bool
  ; indicate_nested_or_patterns: bool
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_open: [`Preserve | `Auto | `Short | `Long]
  ; margin: int  (** Format code to fit within [margin] columns. *)
  ; max_iters: int
        (** Fail if output of formatting does not stabilize within
            [max_iters] iterations. *)
  ; module_item_spacing: [`Compact | `Sparse]
  ; ocp_indent_compat: bool  (** Try to indent like ocp-indent *)
  ; parens_ite: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; parens_tuple_patterns: [`Always | `Multi_line_only]
  ; parse_docstrings: bool
  ; quiet: bool
  ; sequence_style: [`Separator | `Terminator]
  ; single_case: [`Compact | `Sparse]
  ; type_decl: [`Compact | `Sparse]
  ; wrap_comments: bool  (** Wrap comments at margin. *)
  ; wrap_fun_args: bool }

let _ =
  match something with
  | { very_very_long_field_name_running_out_of_space= 1
    ; another_very_very_long_field_name_running_out_of_space= 2
    ; _ } ->
      0
  | _ -> 1

let _ =
  match something with
  | [ very_very_long_field_name_running_out_of_space
    ; another_very_very_long_field_name_running_out_of_space
    ; _ ] ->
      0
  | _ -> 1

let _ =
  match something with
  | [| very_very_long_field_name_running_out_of_space
     ; another_very_very_long_field_name_running_out_of_space
     ; _ |] ->
      0
  | _ -> 1

[@@@ocamlformat "type-decl=compact"]

type t = {aaaaaaaaa: aaaa; bbbbbbbbb: bbbb}

type trace_mod_funs =
  {trace_mod: bool option; trace_funs: bool Map.M(String).t}

[@@@ocamlformat "type-decl=sparse"]

module X = struct
  val select :
       (* The fsevents context *)
       env
    -> (* Additional file descriptor to select for reading *)
       ?read_fdl:fd_select list
    -> (* Additional file descriptor to select for writing *)
       ?write_fdl:fd_select list
    -> (* Timeout...like Unix.select *)
       timeout:float
    -> (* The callback for file system events *)
       (event list -> unit)
    -> unit
end

type t =
  { aaaaaaaaa: aaaa
  ; bbbbbbbbb: bbbb }

type trace_mod_funs =
  { trace_mod: bool option
  ; trace_funs: bool Map.M(String).t }

let x {aaaaaaaaaaaaaa; aaaaaaaaaaaaa; aaaaaaaaaa} =
  {aaaaaaaaaaaa= aaaaaaaaaaaaaaaaa; bbbbbbbbbbbbb= bbb bb bbbbbb}

let x
    { aaaaaaaaaaaaaaaaaaaaaa
    ; aaaaaaaaaaaaaaaaaaa
    ; aaaaaaaaaaaaaa
    ; aaaaaaaaaaaaaaaaaa
    ; aaaaaaaaaa } =
  { aaaaaaaaaaaa= aaaaaaaaaaaaaaaaa
  ; bbbbbbbbbbbbb= bbb bb bbbbbb
  ; cccccc= cccc ccccccccccccccccccccccc }

(* this is an array *)
let length =
  [| 0
   ; 269999999999999999999999999999999999999999999999999
   ; 26
   ; (* foo *) 27 (* foo *)
   ; 27
   ; 27 |]
  [@foo]

(* this is a list *)
let length =
  [ 0
  ; 14
  ; (* foo *)
    14
  ; 17 (* foo *)
  ; 17
  ; 2777777777777777777777777777777777
  ; 27 ]
  [@foo]
;;

Fooooooo.foo ~foooooooooooooo ~fooooooooo:""
  (Foo.foo ~foo ~foo ~foooo:()
     [ ("fooooo", Foo.fooo ~foooo ~foooo:(foooo >*> fooooo))
     ; ("foooo", fooooooo)
     ; ("foooooo", foooooooo)
     ; ("fooooooooo", foooooooo) ] )

class
  [ 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  , 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ]
  x =
  [ 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  , 'yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy ]
  k

type ( 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     , 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbb )
     a =
  ( 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  , 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb )
  e

type ( 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     , 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbb )
     a =
  ('aaaaaaaaa, 'bbbbbbbbbbbb) e

let ( xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    , yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
    , zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
    , (aaaaaaaaaaaa, bbbbbbbbbbbb) ) =
  ( ( xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    , yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
    , zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz )
  , (aaaaaaaaaaaaaa, bbbbbbbbbbbb) )

type t = aaaaaaaaaaaa -> bbbbbbbbbbbb -> cccccccccc

type t =
     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -> bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  -> ccccccccccccccccccccccccc

type t =
     (* foooooooooooo *)
     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -> (* foooooooooooooooooooooooooooooooo*)
     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  -> (* fooooooooooooooooo *)
     ccccccccccccccccccccccccc
  -> (* foooooo *)
     foo * [`Foo of foo * foo]
  -> (* foooooooooooooooo *)
     foo
     * foo
     * foo
     * foo
     * [ `Foo of
            (* fooooooooooooooooooo *)
            foo * foo * foo
         -> foo
         -> foo
         -> (* foooooooooooo *)
            foo
         -> foo
         -> foo * foo
         -> foo * foo
         -> foo * foo ]
  -> (* foooooooooooooooo *)
     fooooooooooooooooo

type t =
  { (* fooooooooooooooooo *)
    foo: foo
        (* foooooooooooooooooooooo fooooooooooooooooooo fooooooooooooooo
           foooooooooooooooooo foooooooooooooooo *)
  ; foo:
         (* fooooooooooooooooooo *)
         foooooooooooo
      -> (* foooooooooooooo *)
         foooooooooooooooo
      -> foooooooooooooo
      -> foooooooooo
      -> fooooooooooooooo
  ; foo: foo }

[@@@ocamlformat "ocp-indent-compat"]

type t =
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -> bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  -> ccccccccccccccccccccccccc

type t =
  (string Location.loc * payload) list
  -> (string Location.loc * bool) list option
     * (string Location.loc * payload) list
  -> (string Location.loc * bool) list option
     * (string Location.loc * payload) list
  -> (string Location.loc * bool) list option
     * (string Location.loc * payload) list

let x {aaaaaaaaaaaaaa; aaaaaaaaaaaaa; aaaaaaaaaa} =
  {aaaaaaaaaaaa= aaaaaaaaaaaaaaaaa; bbbbbbbbbbbbb= bbb bb bbbbbb}

let x
    { aaaaaaaaaaaaaaaaaaaaaa
    ; aaaaaaaaaaaaaaaaaaa
    ; aaaaaaaaaaaaaa
    ; aaaaaaaaaaaaaaaaaa
    ; aaaaaaaaaa }
  =
  { aaaaaaaaaaaa= aaaaaaaaaaaaaaaaa
  ; bbbbbbbbbbbbb= bbb bb bbbbbb
  ; cccccc= cccc ccccccccccccccccccccccc }

let foooooooooooooooooooooooooooooooooo =
  { (* foooooooooooooooooooooooooooooooooooooooooooooooooooooooooo *)
    aaaaaaaaaaaa= aaaaaaaaaaaaaaaaa
  ; (* fooooooooooooooooooooooooooooooooooooooooooooooooooooooooo *)
    bbbbbbbbbbbbb= bbb bb bbbbbb
  ; cccccc= cccc ccccccccccccccccccccccc }

let foooooooooooo =
  { foooooooooooooo with
    fooooooooooooooooooooooooooooo= fooooooooooooo
  ; fooooooooooooo= foooooooooooooo }

let foooooooooooo =
  { foooooooooooooo with
    (* foooooooooooooooo fooooooooooooooooooooooooo
       foooooooooooooooooooooo *)
    fooooooooooooooooooooooooooooo= fooooooooooooo
  ; fooooooooooooo= foooooooooooooo }

let fooooooooooo = function
  | Pmty_alias lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi= Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }

let f () =
  let { aaaaaaaa
      ; bbbbbbbbbb
      ; ccccccccc
      ; dddddddddd
      ; eeeeeeeee
      ; ffffffffff
      ; gggggggggg
      ; hhhhhhhhhh }
    =
    some_value
  in
  foooooooooooo

let f () =
  let [ aaaaaaaa
      ; bbbbbbbbbb
      ; ccccccccc
      ; dddddddddd
      ; eeeeeeeee
      ; ffffffffff
      ; gggggggggg
      ; hhhhhhhhhh ]
    =
    some_value
  in
  foooooooooooo

let f () =
  let [| aaaaaaaa
       ; bbbbbbbbbb
       ; ccccccccc
       ; dddddddddd
       ; eeeeeeeee
       ; ffffffffff
       ; gggggggggg
       ; hhhhhhhhhh |]
    =
    some_value
  in
  foooooooooooo

let g () =
  match some_value with
  | { aaaaaaaa
    ; bbbbbbbbbb
    ; ccccccccc
    ; dddddddddd
    ; eeeeeeeee
    ; ffffffffff
    ; gggggggggg
    ; hhhhhhhhhh } ->
      foooooooo
  | [ aaaaaaaa
    ; bbbbbbbbbb
    ; ccccccccc
    ; dddddddddd
    ; eeeeeeeee
    ; ffffffffff
    ; gggggggggg
    ; hhhhhhhhhh ] ->
      fooooooooo
  | [| aaaaaaaa
     ; bbbbbbbbbb
     ; ccccccccc
     ; dddddddddd
     ; eeeeeeeee
     ; ffffffffff
     ; gggggggggg
     ; hhhhhhhhhh |] ->
      fooooooooo

let () = match x with _, (* line 1 line 2 *) Some _ -> x

let () =
  match x with
  | ( _
    , (* verrrrrrrrrrrrrrrrrrrrrrrry looooooooooooooooong line 1
         veeeeeeeeeeeeeeeeeeeeeryyyy loooooooooooooooooong line 2 *)
      Some _ ) ->
      x
