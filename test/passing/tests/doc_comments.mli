(** Manpages. See {!Cmdliner.Manpage}. *)

type block =
  [ `S of string
  | `P of string
  | `Pre of string
  | `I of string * string
  | `Noblank
  | `Blocks of block list ]

include M with type t := t
(** Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod *)

val escape : string -> string
(** [escape s] escapes [s] from the doc language. *)

type title = string * int * string * string * string

(** {1:standard-section-names Standard section names} *)

val s_name : string

(** {1:section-maps Section maps}

    Used for handling the merging of metadata doc strings. *)

type smap

val smap_append_block : smap -> sec:string -> block -> smap
(** [smap_append_block smap sec b] appends [b] at the end of section [sec]
    creating it at the right place if needed. *)

(** {1:content-boilerplate Content boilerplate} *)

val s_environment_intro : block

(** {1:output Output} *)

type format = [`Auto | `Pager | `Plain | `Groff]

val print :
     ?errs:Format.formatter
  -> ?subst:(string -> string option)
  -> format
  -> Format.formatter
  -> t
  -> unit

(** {1:printers-and-escapes-used-by-cmdliner-module Printers and escapes
    used by Cmdliner module} *)

val subst_vars :
     errs:Format.formatter
  -> subst:(string -> string option)
  -> Buffer.t
  -> string
  -> string
(** [subst b ~subst s], using [b], substitutes in [s] variables of the form
    "$(doc)" by their [subst] definition. This leaves escapes and markup
    directives $(markup,...) intact.

    @raise Invalid_argument in case of illegal syntax. *)

val doc_to_plain :
     errs:Format.formatter
  -> subst:(string -> string option)
  -> Buffer.t
  -> string
  -> string
(** [doc_to_plain b ~subst s] using [b], subsitutes in [s] variables by
    their [subst] definition and renders cmdliner directives to plain text.

    @raise Invalid_argument in case of illegal syntax. *)

val k : k
(** this is a comment

    @author foo

    @author Foooooooooooooooooooooooooooooooooooo Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar

    @version foo

    @version Foooooooooooooooooooooooooooooooooooo Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar

    @see <foo> foo

    @see <https://slash-create.js.org/#/docs/main/latest/class/SlashCreator?scrollTo=registerCommandsIn> this url is very long

    @since foo

    @since Foooooooooooooooooooooooooooooooooooo.Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar

    @before foo [foo]

    @before Foooooooooooooooooooooooooooooooooooo.Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar Foo bar

    @deprecated [foo]

    @deprecated Foooooooooooooooooooooooooooooooooooo Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar Foo bar

    @param foo [foo]

    @param Foooooooooooooo_Baaaaaaaaaaaaar Fooooooooooo foooooooooooo fooooooooooo baaaaaaaaar

    @param Foooooooooooooooooooooooooooooooooooo_baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar Foo bar

    @raise foo [foo]

    @raise Foooooooooooooooooooooooooooooooooooo_baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar Foo bar

    @return [foo]

    @inline

    @canonical foo

    @canonical Foooooooooooooooooooooooooooooooooooo.Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar *)

val x : x
(** a comment

    @version foo *)

(** Managing Chunks.

    This module exposes functors to store raw contents into append-only
    stores as chunks of same size. It exposes the {{!AO} AO} functor which
    split the raw contents into [Data] blocks, addressed by [Node] blocks.
    That's the usual rope-like representation of strings, but chunk trees
    are always build as perfectly well-balanced and blocks are addressed by
    their hash (or by the stable keys returned by the underlying store).

    A chunk has the following structure:

    {v
    --------------------------      --------------------------
    | uint8_t type            |     | uint8_t type            |
    ---------------------------     ---------------------------
    | uint16_t                |     | uint64_t                |
    ---------------------------     ---------------------------
    | key children[length]    |     | byte data[length]       |
    ---------------------------     ---------------------------
    v}

    [type] is either [Data] (0) or [Index] (1). If the chunk contains data,
    [length] is the payload length. Otherwise it is the number of children
    that the node has.

    It also exposes {{!AO_stable} AO_stable} which -- as {{!AO} AO} does --
    stores raw contents into chunks of same size. But it also preserves the
    nice properpty that values are addressed by their hash. instead of by
    the hash of the root chunk node as it is the case for {{!AO} AO}. *)

(** This is verbatim:

    {v
   o  o
  /\  /\
  /\  /\
    v}

    This is preformated code:

    {[
let verbatim s =
  s |> String.split_lines |> List.map ~f:String.strip
  |> fun s -> list s "@," Fmt.str
    ]} *)

(** Lists:

    list with short lines:

    - x
    - y
    - z

    list with long lines:

    - xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx
      xxx xxx xxx xxx xxx xxx xxx
    - yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy
      yyy yyy yyy yyy yyy yyy yyy
    - zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz
      zzz zzz zzz zzz zzz zzz zzz

    enumerated list with long lines:

    + xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx
      xxx xxx xxx xxx xxx xxx xxx
    + yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy yyy
      yyy yyy yyy yyy yyy yyy yyy
    + zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz zzz
      zzz zzz zzz zzz zzz zzz zzz

    list with sub lists:

    {ul
     {- xxx

        - a
        - b
        - c
     }
     {- yyy

        + a
        + b
        + c
     }} *)

(** {{:https://github.com/} Github} *)

(** {:https://github.com/} *)

(** An array index offset: [exp1\[exp2\]] *)

(** to extend \{foo syntax *)

(** The different forms of references in \@see tags. *)

(** Printf groff string for the \@before information. *)

(** [a]'c [b]'s [c]'c *)

(** return true if [\gamma(lhs) \subseteq \gamma(rhs)] *)

(** Composition of functions: [(f >> g) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

(** [†] [Struct_rec] is *)

(** for [Global]s *)

(** generic command: ∀xs.[foot]-[post] *)

(** A *)
val foo : int -> unit
(** B *)

(** C *)

(** A *)
val foo : int -> unit
(** B *)

module Foo : sig
  (** A *)
  val foo : int -> unit
  (** B *)

  (** C *)

  (** A *)
  val foo : int -> unit
  (** B *)
end

(** [\[ \] \[\] \]] *)

(** \{ \} \[ \] \@ \@ *)

(** @canonical Foo *)

(** @canonical Module.Foo.Bar *)

(** {v
a
    v} *)

(** {[
b
    ]} *)

(** - Odoc don't parse

    multiple paragraph in a list *)

(** {ul
     {- Abc

        Def
     }
     {- Hij
     }
     {- Klm

        {ul
         {- Nop

            Qrs
         }
         {- Tuv
         }}
     }} *)

(** - {v
    Abc
    def
      v}
    - {[
A
  B
      ]} *)

(** Code block
    {[ Single line ]}
    {[
      Multi
      line
    ]}
    {[
      Multi
      line
        with
          indentation
    ]}
    {[ Single long line HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ]}
    {[
      With empty

      line
    ]}
    {[ First line
       on the same line
       as opening ]}
 *)

module X :
sig

  (** {[ First line
         on the same line
         as opening ]} *)

end

(** {!module:A} {!module:A.B}

    {!module-type:A} {!module-type:A.b}

    {!class:c} {!class:M.c}

    {!class-type:c} {!class-type:M.c}

    {!val:x} {!val:M.x}

    {!type:t} {!type:M.t}

    {!exception:E} {!exception:M.E}

    {!method:m} {!method:c.m}

    {!constructor:C} {!constructor:M.C}

    {!field:f} {!field:t.f} {!field:M.t.f}
 *)

(** {!modules:Foo}

    {!modules:Foo Bar.Baz}

    @canonical Foo

    @canonical Foo.Bar
*)

(** {%html:<p>Raw markup</p>%} {%Without language%} {%other:Other language%} *)

(** [Multi
     Line]

    [ A lot of    spaces ]

    [Very looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong] *)

(** {[
  for i = 1 to 3
  do
    Printf.printf "let x%d = %d\n" i i
  done
]} *)

(** {[
  print_newline ();
  List.iter
    (fun s -> Printf.printf "let ( %s ) = Pervasives.( %s )\n" s s)
    ["+"; "-"; "*"; "/"]
]}  *)

(** {[
  #use "import.cinaps";;

  List.iter all_fields ~f:(fun (name, type_) -> printf "\nexternal get_%s
  : unit -> %s = \"get_%s\"" name type_ name)
]} *)

(** {[


  List.iter all_fields ~f:(fun (name, type_) -> printf "\nexternal get_%s
  : unit -> %s = \"get_%s\"" name type_ name)
]} *)

(** {[
  let x = 1 in

  (* fooooooo *)
  let y = 2 in
  (* foooooooo *)
  z
]} *)

(** {[
   let this  = is_short
]}

{[
  does not parse: verbatim
+/+/+ /+/+/ +/+//+/+/+/+/+/+/+/
+/+/+ /+/+/ +/+//+/+/+/+/+/+/+/
+/+/+ /+/+/ +/+//+/+/+/+/+/+/+/
+/+/+ /+/+/ +/+//+/+
]}

{[
[@@@ocamlformat "break-separators = after"]

let fooooooooooooooooo =
[ foooooooooooooooooooooooooooooooo
; foooooooooooooooooooooooooooooooo
; foooooooooooooooooooooooooooooooo ]

]}

{[
let fooooooooooooooooo =
[ foooooooooooooooooooooooooooooooo
; foooooooooooooooooooooooooooooooo
; foooooooooooooooooooooooooooooooo ]

]} *)



(**
  This is a comment with code inside
  {[
    (** This is a comment with code inside
        [ let code inside = f inside ]
    *)
    let code inside (* comment   *) = f inside
  ]}

  Code block with metadata:
  {@ocaml[ code ]}

  {@ocaml kind=toplevel[ code ]}

  {@ocaml kind=toplevel env=e1[
    (** This is a comment with code inside
        [ let code inside = f inside ]
    *)
    let code inside (* comment   *) = f inside
  ]}
*)

(** {e foooooooo oooooooooo ooooooooo ooooooooo} {i fooooooooooooo oooooooo oooooooooo} {b fooooooooooooo oooooooooooo oooooo ooooooo} *)

(** {e foooooooo oooooooooo ooooooooo ooooooooo} {{!some ref} fooooooooooooo
    oooooooo oooooooooo} {b fooooooooooooo oooooooooooo oooooo ooooooo} *)

(** foooooooooooooooooooooooooooooooooooooooooooooooooo foooooooooooo {b eee + eee eee} *)

(** foooooooooooooooooooooooooooooooooooooooooooooooooo foooooooooooooooo {b + eee + eee eee} *)

val f : int

(***)

val k : int

(**)

(** {e foooooooo oooooooooo ooooooooo ooooooooo
       {i fooooooooooooo oooooooo oooooooooo
          {b fooooooooooooo oooooooooooo oooooo ooooooo}}} *)

(** {e
       {i fooooooooooooo oooooooo oooooooooo
          {b fooooooooooooo oooooooooooo oooooo ooooooo}} foooooooo
       oooooooooo ooooooooo ooooooooo} *)

(** foooooooooo fooooooooooo

    {e foooooooo oooooooooo ooooooooo ooooooooo
       {i fooooooooooooo oooooooo oooooooooo
          {b fooooooooooooo oooooooooooo oooooo ooooooo}} fooooooooooooo
       foooooooooo fooooo
       {i fooooooooooooo oooooooo oooooooooo
          {b fooooooooooooo oooooooooooo oooooo ooooooo}}}

    {e foooooooo oooooooooo ooooooooo ooooooooo
       {i fooooooooooooo oooooooo oooooooooo}}

    fooooooooooooo foooooooooooooo:

    - foo
    - {e foooooooo oooooooooo ooooooooo ooooooooo
         {i fooooooooooooo oooooooo oooooooooo}}
    - {e foooooooo oooooooooo ooooooooo ooooooooo}
      {i fooooooooooooo oooooooo oooooooooo}
    - foo *)

(** Brackets must not be escaped in the first argument of some tags: *)

(** @raise [Invalid_argument] if the argument is [None]. Sometimes [t.[x]]. *)

(** @author [Abc] [def] \[hij\] *)

(** @author {Abc} {def} \{hij\} *)

(** @param [id] [def] \[hij\] *)

(** @raise [exn] [def] \[hij\] *)

(** @since [Abc] [def] \[hij\] *)

(** @before [Abc] [def] \[hij\] *)

(** @version [Abc] [def] \[hij\] *)

(** @see <[Abc]> [def] \[hij\] *)

(** @see '[Abc]' [def] \[hij\] *)

(** @see "[Abc]" [def] \[hij\] *)

(** \[abc\] *)
(** *)
(**  *)

(** [trim "  "] is [""] *)

(** [trms (c × (Σᵢ₌₁ⁿ cᵢ × Πⱼ₌₁ᵐᵢ Xᵢⱼ^pᵢⱼ))]
    is the sequence of terms [Xᵢⱼ] for each [i] and [j]. *)

(**

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi lacinia odio sit amet lobortis fringilla. Mauris diam massa, vulputate sit amet lacus id, vestibulum bibendum lectus. Nullam tristique justo nisi, gravida dapibus mi pulvinar at. Suspendisse pellentesque odio quis ipsum tempor luctus.

Cras ultrices, magna sit amet faucibus molestie, sapien dolor ullamcorper lorem, vel viverra tortor augue vel massa. Suspendisse nunc nisi, consequat et ante nec, efficitur dapibus ipsum. Aenean vitae pellentesque odio. Integer et ornare tellus, at tristique elit.

Phasellus et nisi id neque ultrices vestibulum vitae non tortor. Mauris aliquet at risus sed rhoncus. Ut condimentum rhoncus orci, sit amet eleifend erat tempus quis.

*)

(** {[(* a
         b *)]} *)

val a :
  fooooooooooooooooooooooooooo
  (** {[(* a
           b *)]} *)
  ->
  fooooooooooooooooooooooooo

type x =
  { a : t
  (** {[(* a
           b *)]} *)
  ; b : [` A
  (** {[(* a
           b *)]} *) ]
  }

type x =
  | A of a
  (** {[(* a
           b *)]} *)
  | B of b
  (** {[(* a
           b *)]} *)

(** Set a different language name in the block metadata to not format as OCaml:

    {@sh[ echo "this""is""only""a""single"(echo word)(echo also) ]} *)

(**a*)(**b*)

(** Inline math: {m \infty}

    Inline math elements can wrap as well {m \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty \infty} or {m \f\relax{x} = \int_{-\infty}^\infty \f\hat\xi\,e^{2 \pi i \xi x} \,d\xi}.

    Block math:

    {math \infty}

    {math
    \infty
    }

    {math

    \pi

    }

    {math

    \infty

   \pi

       \pi

    \pi

    }

    {math {m \f\relax{x} = \int_{-\infty}^\infty \f\hat\xi\,e^{2 \pi i \xi x} \,d\xi}}

    {math
    % \f is defined as #1f(#2) using the macro
    \f\relax{x} = \int_{-\infty}^\infty
    \f\hat\xi\,e^{2 \pi i \xi x}
    \,d\xi
    }
*)
