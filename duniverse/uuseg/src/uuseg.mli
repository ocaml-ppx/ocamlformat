(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unicode text segmentation.

    [Uuseg] segments Unicode text. It implements the locale
    independent Unicode text segmentation algorithms to detect
    grapheme cluster, word and sentence boundaries and the Unicode
    line breaking algorithm to detect line break opportunities.

    The module is independent from any IO mechanism or Unicode text
    data structure and it can process text without a complete
    in-memory representation.

    The supported Unicode version
    is determined by the {!unicode_version} value.

    Consult the {{!basics}basics}, {{!limits}limitations} and
    {{!examples}examples} of use.

    {3 References}
    {ul
    {-
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}
    {-
    {e {{:http://www.unicode.org/reports/tr29/}UAX #29 Unicode Text
    Segmentation}}. (latest version)}
    {-
    {e {{:http://www.unicode.org/reports/tr14/}UAX #14 Unicode Line Breaking
    Algorithm}}. (latest version)}
    {- Web based {{:http://unicode.org/cldr/utility/breaks.jsp}ICU
       break utility}.}}
*)

(** {1 Segment} *)

val unicode_version : string
(** [unicode_version] is the Unicode version supported by [Uuseg]. *)

type custom
(** The type for custom segmenters. See {!val:custom}. *)

type boundary =
  [ `Grapheme_cluster
       (** {{:http://www.unicode.org/glossary/#extended_grapheme_cluster}
           Extended grapheme clusters} according to
           {{:https://www.unicode.org/reports/tr29/#C1-1}UAX29-C1-1} *)
  | `Word
      (** Words according to
          {{:https://www.unicode.org/reports/tr29/#C2-1}UAX29-C2-1} *)
  | `Sentence
      (** Sentences according to
          {{:https://www.unicode.org/reports/tr29/#C3-1}UAX29-C3-1} *)
  | `Line_break
      (** Line breaks accordings to
          {{:http://www.unicode.org/reports/tr14/}UAX #14} *)
  | `Custom of custom ]
(** The type for boundaries. *)

val pp_boundary : Format.formatter -> boundary -> unit
(** [pp_boundary ppf b] prints an unspecified representation of [b]
    on [ppf]. *)

type t
(** The type for Unicode text segmenters. *)

type ret = [ `Boundary | `Uchar of Uchar.t | `Await | `End ]
(** The type for segmenter results. See {!add}. *)

val create : [< boundary ] -> t
(** [create b] is an Unicode text segmenter for boundaries of type [b]. *)

val boundary : t -> boundary
(** [boundary s] is the type of boundaries detected by [s]. *)

val add : t -> [ `Uchar of Uchar.t | `Await | `End ] -> ret
(** [add s v] is:
    {ul
    {- [`Boundary] if there is a boundary at that point in the sequence of
       characters. The client must then call [add] with [`Await]
       until [`Await] is returned.}
    {- [`Uchar u] if [u] is the next character in the sequence.
       The client must then call [add] with [`Await] until [`Await] is
       returned.}
    {- [`Await] when the segmenter is ready to add a new [`Uchar]
       or [`End].}
    {- [`End] when [`End] was added and all [`Boundary] and [`Uchar] were
       output.}}

    For [v] use [`Uchar u] to add a new character to the sequence to
    segment and [`End] to signal the end of sequence. After adding one
    of these two values always call [add] with [`Await] until [`Await]
    or [`End] is returned.

    @raise Invalid_argument if [`Uchar] or [`End] is added while
    that last add did not return [`Await] or if an [`Uchar] or [`End]
    is added after an [`End] was already added. *)

val mandatory : t -> bool
(** [mandatory s] is [true] if the last [`Boundary] returned by {!add}
    was mandatory. This function only makes sense for [`Line_break]
    segmenters or [`Custom] segmenters that sport that notion. For
    other segmenters or if no [`Boundary] was returned so far, [true]
    is returned. *)

val copy : t -> t
(** [copy s] is a copy of [s] in its current state. Subsequent {!add}s on
    [s] do not affect the copy. *)

val pp_ret : Format.formatter -> [< ret] -> unit
(** [pp_ret ppf v] prints an unspecified representation of [v] on [ppf]. *)

(** {1 Custom segmenters} *)

val custom :
  ?mandatory:('a -> bool) ->
  name:string ->
  create:(unit -> 'a) ->
  copy:('a -> 'a) ->
  add: ('a -> [ `Uchar of Uchar.t | `Await | `End ] -> ret) -> unit -> custom
  (** [create ~mandatory ~name ~create ~copy ~add] is a custom segmenter.
      {ul
      {- [name] is a name to identify the segmenter.}
      {- [create] is called when the segmenter is {{!create}created}
         it should return a custom segmenter value.}
      {- [copy] is called with the segmenter value whenever the
         segmenter is {{!copy}copied}. It should return a copy of the
         segmenter value.}
      {- [mandatory] is called with the segmenter value to define
         the result of the {!mandatory} function. Defaults always
         returns [true].}
      {- [add] is called with the segmenter value to define
         the result of the {!add} value. The returned value
         should respect the semantics of {!add}. Use the functions
         {!err_exp_await} and {!err_ended} to raise [Invalid_argument]
         exception in {!add}s error cases.}} *)

val err_exp_await : [< ret] -> 'a
(** [err_exp_await fnd] should be used by custom segmenters when
    the client tries to {!add} an [`Uchar] or [`End] while the last
    returned value was not an [`Await]. *)

val err_ended : [< ret] -> 'a
(** [err_ended ()] should be used by custom segmenter when the client
    tries to {!add} [`Uchar] or [`End] after [`End] was already added. *)

(** {1:limits Limitations}

    A [`Grapheme_cluster] segmenter will always consume only a small
    bounded amount of memory on any text. Other segmenters will also
    do so on non-degenerate text, but it's possible to feed them with
    input that will make them buffer an arbitrary amount of
    characters. *)

(** {1:basics Basics}

    A segmenter is a stateful filter that inputs a sequence of characters
    and outputs the same sequence except characters are interleaved
    with [`Boundary] values whenever the segmenter detects a boundary.

    The function {!create} returns a new segmenter for a given boundary
    type:
{[
let words = Uuseg.create `Word
]}
    To add characters to the sequence to segment, call {!add} on
    [words] with [`Uchar _]. To end the sequence call {!add} on [words]
    with [`End]. The segmented sequence of characters is returned character
    by character, interleaved with [`Boundary] values at the appropriate
    places, by the successive calls to {!add}.

    The client and the segmenter must wait on each other to limit
    internal buffering: each time the client adds to the sequence
    by calling {!add} with [`Uchar] or [`End] it must continue to
    call {!add} with [`Await] until the segmenter returns [`Await]
    or [`End]. In practice this leads to the following kind of control flow:
{[
let rec add acc v = match Uuseg.add words v with
| `Uchar u -> add (`Uchar u :: acc) `Await
| `Boundary -> add (`B :: acc) `Await
| `Await | `End -> acc
]}
    For example to segment the sequence <[U+0041], [U+0020], [U+0042]>
    (["a b"]) to a list of characters interleaved with [`B] values on word
    boundaries we can write:
{[
let uchar = `Uchar (Uchar.of_int u)
let seq = [uchar 0x0041; uchar 0x0020; uchar 0x0042]
let seq_words = List.rev (add (List.fold_left add [] seq) `End)
]}
*)

(** {1:examples Examples}

[utf_8_segments seg s] is the list of UTF-8 encoded [seg] segments of
the UTF-8 encoded string [s].

{[
let utf_8_segments seg s =
  let flush_segment buf acc =
    let segment = Buffer.contents buf in
    Buffer.clear buf; if segment = "" then acc else segment :: acc
  in
  let rec add buf acc segmenter v = match Uuseg.add segmenter v with
  | `Uchar u -> Buffer.add_utf_8_uchar buf u; add buf acc segmenter `Await
  | `Boundary -> add buf (flush_segment buf acc) segmenter `Await
  | `Await | `End -> acc
  in
  let rec loop buf acc s i max segmenter =
    if i > max then flush_segment buf (add buf acc segmenter `End) else
    let dec = String.get_utf_8_uchar s i in
    let acc = add buf acc segmenter (`Uchar (Uchar.utf_decode_uchar dec)) in
    loop buf acc s (i + Uchar.utf_decode_length dec) max segmenter
  in
  let buf = Buffer.create 42 in
  let segmenter = Uuseg.create seg in
  List.rev (loop buf [] s 0 (String.length s - 1) segmenter)
]}

Note that this function can be derived directly from
{!Uuseg_string.fold_utf_8}. *)
