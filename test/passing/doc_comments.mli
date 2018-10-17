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
    directives $(markup,...) intact. @raise Invalid_argument in case of
    illegal syntax. *)

val doc_to_plain :
     errs:Format.formatter
  -> subst:(string -> string option)
  -> Buffer.t
  -> string
  -> string
(** [doc_to_plain b ~subst s] using [b], subsitutes in [s] variables by
    their [subst] definition and renders cmdliner directives to plain text.
    @raise Invalid_argument in case of illegal syntax. *)
