(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let unicode_version = Uucp.unicode_version

(* Segmenters *)

type 'a segmenter =
  { name : string;
    create : unit -> 'a;
    copy : 'a -> 'a;
    mandatory : 'a -> bool;
    add : 'a -> [ `Uchar of Uchar.t | `Await | `End ] ->
      [ `Boundary | `Uchar of Uchar.t | `Await | `End ] }

type custom = C : 'a segmenter -> custom

type boundary =
  [ `Grapheme_cluster | `Word | `Sentence | `Line_break | `Custom of custom ]

let pp_boundary ppf b = match (b :> boundary) with
| `Grapheme_cluster -> Format.fprintf ppf "`Grapheme_cluster"
| `Word -> Format.fprintf ppf "`Word"
| `Sentence -> Format.fprintf ppf "`Sentence"
| `Line_break -> Format.fprintf ppf "`Line_break"
| `Custom (C s) -> Format.fprintf ppf "`Custom %s" s.name

(* Built-in segmenters *)

let mandatory_default _ = true

let grapheme_cluster =
  C { name = "Uuseg.grapheme_cluster";
      create = Uuseg_grapheme_cluster.create;
      copy = Uuseg_grapheme_cluster.copy;
      mandatory = mandatory_default;
      add = Uuseg_grapheme_cluster.add; }

let word =
  C { name = "Uuseg.word";
      create = Uuseg_word.create;
      copy = Uuseg_word.copy;
      mandatory = mandatory_default;
      add = Uuseg_word.add; }

let sentence =
  C { name = "Uuseg.sentence";
      create = Uuseg_sentence.create;
      copy = Uuseg_sentence.copy;
      mandatory = mandatory_default;
      add = Uuseg_sentence.add; }

let line_break =
  C { name = "Uuseg.line_break";
      create = Uuseg_line_break.create;
      copy = Uuseg_line_break.copy;
      mandatory = Uuseg_line_break.mandatory;
      add = Uuseg_line_break.add; }

(* Generic segmenter inteface *)

type t = Seg : boundary * 'a * 'a segmenter -> t
type ret = Uuseg_base.ret

let create boundary =
  let (C seg) = match boundary with
  | `Grapheme_cluster -> grapheme_cluster
  | `Word -> word
  | `Sentence -> sentence
  | `Line_break -> line_break
  | `Custom c -> c
  in
  Seg ((boundary :> boundary), seg.create (), seg)

let boundary (Seg (boundary, _, _)) = boundary
let add (Seg (_, s, seg)) add = seg.add s add
let mandatory (Seg (_, s, seg)) = seg.mandatory s
let copy (Seg (b, s, seg)) = Seg (b, seg.copy s, seg)
let pp_ret = Uuseg_base.pp_ret

(* Custom segmenters *)

let custom ?(mandatory = mandatory_default) ~name ~create ~copy ~add () =
  C { name; create; copy; mandatory; add }

let err_exp_await = Uuseg_base.err_exp_await
let err_ended = Uuseg_base.err_ended
