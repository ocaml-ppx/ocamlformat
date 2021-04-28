(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

    {e Unicode version %%UNICODE_VERSION%%}

    {3 References}
    {ul
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr29/}UAX #29 Unicode Text
    Segmentation}}. (latest version)}
    {- Andy Heninger.
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

type boundary = [ `Grapheme_cluster | `Word | `Sentence | `Line_break
                | `Custom of custom ]
(** The type for boundaries.
    {ul
    {- [`Grapheme_cluster] determines
    {{:http://www.unicode.org/glossary/#extended_grapheme_cluster}
    extended grapheme clusters} boundaries according to UAX 29
    (corresponds, for most scripts, to user-perceived characters).}
    {- [`Word] determines word boundaries according to UAX 29.}
    {- [`Sentence] determines sentence boundaries according to UAX 29.}
    {- [`Line_break] determines {{!mandatory}mandatory} line breaks and
       line break opportunities according to UAX 14.}} *)

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
the UTF-8 encoded string [s]. This example uses {!Uutf} to fold over
the characters of [s] and to encode the characters in a standard
OCaml buffer. Note that this function can be derived directly from
{!Uuseg_string.fold_utf_8}.
{[
let utf_8_segments seg s =
  let b = Buffer.create 42 in
  let flush_segment acc =
    let segment = Buffer.contents b in
    Buffer.clear b; if segment = "" then acc else segment :: acc
  in
  let seg = Uuseg.create (seg :> Uuseg.boundary) in
  let rec add acc v = match Uuseg.add seg v with
  | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add acc `Await
  | `Boundary -> add (flush_segment acc) `Await
  | `Await -> acc
  in
  let rec uchar acc _ = function
  | `Uchar _ as u -> add acc u
  | `Malformed _ -> add acc (`Uchar Uutf.u_rep)
  in
  List.rev (flush_segment (add (Uutf.String.fold_utf_8 uchar [] s) `End))
]}
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
