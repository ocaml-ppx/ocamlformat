type uu = A of int | B of (< leq: 'a > as 'a)

type uu = A of int | B of (< leq: 'a > as 'a) * 'a

type uu = A of (int as 'a) | B of 'a * (< leq: 'a > as 'a)

type uu += A of (int as 'a)

type uu += B of 'a * (< leq: 'a > as 'a)

let _ = ignore Async_unix.Fd.(([stdin (); stdout (); stderr ()] : t list))

type t = {a: int; b: int}

type t = [`A | `B]

type t =
  | Internal_error of
      [ `Doc_comment of
        [ `Moved of Location.t * Location.t * string
        | `Unstable of Location.t * string ] ]

val x :
  [ `X of int
    (** foooooooooooooooo foooooooooooooooooooooooo fooooooooooooooooooooooo
        fooooooooooooooooooo fooooooooooooooo*) ]

val x :
  [ `X of
    int
    * foooooooooooooo
    * fooooooooooo
    * fooooooooooo foooooooooo
    * foooooooooooo
    (** foooooooooooooooo foooooooooooooooooooooooo fooooooooooooooooooooooo
        fooooooooooooooooooo fooooooooooooooo*) ]

val x :
  [ `X of int (* booooom *)
    (** foooooooooooooooo foooooooooooooooooooooooo fooooooooooooooooooooooo
        fooooooooooooooooooo fooooooooooooooo*) ]

val x :
  [ `X of
    int
    * foooooooooooooo
    * fooooooooooo
    * fooooooooooo foooooooooo
    * foooooooooooo
    (* boooooom *)
    (** foooooooooooooooo foooooooooooooooooooooooo fooooooooooooooooooooooo
        fooooooooooooooooooo fooooooooooooooo*) ]

type voting_period =
      Tezos_client_alpha.Proto_alpha.Alpha_context.Voting_period.kind =
  | Proposal
  | Testing_vote

(** foooooooo *)
type voting_period =
      (* foooooooooooo *)
      (* foooooooooo ooooooooooooooooo ooooooooooooo *)
      Tezos_client_alpha.Proto_alpha.Alpha_context.Voting_period.kind
      (* fooooooooooooooo oooooooooooooooooooo ooooooooooooooooooooo *)
      (* fooooo *) =
  (* foooooooooo *)
  | Proposal
  | Testing_vote  (** fooooooooooo *)

type ( 'context
     , 'f_in
     , 'f_out
     , 'captured_types
     , 'markers_in
     , 'markers_out
     , 'list_constraint )
     templ_matcher =
  { on_objc_cpp: 'context -> 'f_in
  ; on_objc_cpp: 'context -> 'f_in
  ; on_objc_cpp: 'context -> 'f_in
  ; on_objc_cpp: 'context -> 'f_in }

type ( 'context
     , 'f_in
     , 'f_out
     , 'captured_types
     , 'markers_in
     , 'markers_out
     , 'list_constraint )
     templ_matcher =
      ( 'context
      , 'f_in
      , 'f_out
      , 'captured_types
      , 'markers_in
      , 'markers_out
      , 'list_constraint )
      templ_matcher =
  { on_objc_cpp: 'context -> 'f_in
  ; on_objc_cpp: 'context -> 'f_in
  ; on_objc_cpp: 'context -> 'f_in
  ; on_objc_cpp: 'context -> 'f_in }

type ( 'context
     , 'f_in
     , 'f_out
     , 'captured_types
     , 'markers_in
     , 'markers_out
     , 'list_constraint )
     templ_matcher +=
  | On_objc_cpp : 'context -> 'f_in
  | On_objc_cpp : 'context -> 'f_in
  | On_objc_cpp : 'context -> 'f_in
  | On_objc_cpp : 'context -> 'f_in
