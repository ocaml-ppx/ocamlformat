type foo = int                          (* just in case *)



(* These two shouldn't be indented differently, but are. *)

type z =
  [ `Bar of foo
    (* a comment [expected to apply to `Foo as below] *)
  | `Foo ]

type z =
  [ `Bar
  (* a comment *)
  | `Foo ]



(* On second thought, I kind of like this way of thinking about this
indentation, even though it is kind of parasyntactic: *)

type z =
  (* Applies to "[" or `Bar. *)
  [ `Bar of foo
    (* Applies to "|" or `Foo.  Indented too much. *)
  | `Foo ]

type z =
  (* Applies to "[" or `Bar. *)
  [ `Bar
  (* Applies to "|" or `Foo. *)
  | `Foo ]

(* The way we write code, that will line up more nicely. *)



let _ =
  (foo
     (* This is indented too far to the left *)
     (bar))

(* It looks to me like we generally want the comment to apply to the
     following line in most circumstances, including this one.  The default indent
for an empty line after a function application that isn't terminated with a
     ";" or something would probably also be in a bit, in anticipation of an
argument, although I don't think that's crucial. *)
let _ =
  foo quux
(* about bar *)
    bar
(* about baz *)
    baz

(** Trying lists within comments:
    - this is a
    multi-line element of a list.
    - and this is a one-liner
    - this
    has
  many
         more
    lines
      - and this is indented like a sub-list
      - but isn't one at
        -all

    this is outside of the list though.
    
    - and this is
    - another
    list

    - and another
one

the end
*)

(* There is an issue with toplevel sessions:
     # expr1;;
     - : type1 = value1
     # expr2;;
     - : type2 = value2

     Comment. *)

(* Comment:

        - [code];
      - {[ code ]} *)
