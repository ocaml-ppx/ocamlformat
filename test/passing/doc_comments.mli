(** Manpages. See {!Cmdliner.Manpage}. *)

type block =
  [ `S of string
  | `P of string
  | `Pre of string
  | `I of string * string
  | `Noblank
  | `Blocks of block list ]

val escape : string -> string
(** [escape s] escapes [s] from the doc language. *)

type title = string * int * string * string * string

(** {1 Standard section names} *)

val s_name : string

(** {1 Section maps} Used for handling the merging of metadata doc strings. *)

type smap

val smap_append_block : smap -> sec:string -> block -> smap
(** [smap_append_block smap sec b] appends [b] at the end of section [sec]
    creating it at the right place if needed. *)

(** {1 Content boilerplate} *)

val s_environment_intro : block

(** {1 Output} *)

type format = [`Auto | `Pager | `Plain | `Groff]

val print :
     ?errs:Format.formatter
  -> ?subst:(string -> string option)
  -> format
  -> Format.formatter
  -> t
  -> unit

(** {1 Printers and escapes used by Cmdliner module} *)

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
    @version foo
    @see <foo> foo
    @since foo
    @before foo [foo]
    @deprecated [foo]
    @param foo [foo]
    @raise foo [foo]
    @return [foo]
    @inline
    @canonical foo *)

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
        - c}
     {- yyy
        + a
        + b
        + c}} *)
