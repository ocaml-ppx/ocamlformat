(* This testing infrastructure has been contributed by Gabriel Radanne.
   It is based on regenerate: https://github.com/regex-generate/regenerate *)

let bos = '^'
let eos = '$'
let alphabet = ['a'; 'b'; 'c']

module Char = struct
  include Char
  let hash = Hashtbl.hash
  let foreach f =
    List.iter f (bos :: eos :: alphabet)
  let print c =
    Printf.sprintf "%c" c
  let pp = Format.pp_print_char
end

module B =
  Brzozowski.Make(Char)

open B

let remove l x =
  let rec remove' x acc l = match l with
    | [] -> List.rev acc
    | y :: tail when Char.equal x y -> remove' x acc tail
    | y :: tail -> remove' x (y::acc) tail
  in
  remove' x [] l

let compl l =
  List.fold_left remove alphabet l
let set b l = disjunction (List.map char (if b then l else compl l))


(* Turn a regular expression from Regenerate into a [B] expression. *)
let rec to_re = let open Regenerate.Regex in function
    | One -> B.epsilon
    | Set (b, l) -> set b l
    | Seq (re, re') -> to_re re @@ to_re re'
    | Or (re, re') -> to_re re ||| to_re re'
    | And (re, re') -> to_re re &&& to_re re'
    | Not re -> B.neg (to_re re)
    | Rep (0,None,re) -> B.star (to_re re)
    | Rep (_,_,_re) -> assert false

let is_match re input =
  match B.exec re input with None -> false | Some _ -> true

(* Check positive and negative samples. *)
let check (re, pos, neg) =
  (* 1. Compile the regular expression. *)
  let cre =
    try
      B.dfa (to_re re)
    with _ ->
      (* Discard regular expressions that we do not handle. *)
      QCheck.assume_fail ()
  in
  (* 2. Test! *)
  List.for_all (fun s -> is_match cre s) pos &&
  List.for_all (fun s -> not (is_match cre s)) neg



module WordSeq (C : sig include Set.OrderedType val pp : t Fmt.t end) = struct
  type char = C.t
  type t = C.t Seq.t
  let empty = Seq.empty
  let singleton = OSeq.return
  let length = OSeq.length
  let append = OSeq.append
  let cons = OSeq.cons
  let pp fmt w = Seq.iter (C.pp fmt) w
  let compare = OSeq.compare ~cmp:C.compare
end

(* Ensure that the whole word is matched *)
let add_boundaries =
  let bounds s =
    let (@) = OSeq.append in
    OSeq.(return bos @ s @ return eos)
  in
  let whole_string re =
    Regenerate.Regex.(seq [char bos; re; char eos])
  in
  QCheck.map_same_type
    (fun (re, pos, neg) ->
       whole_string re, List.map bounds pos, List.map bounds neg)

let test =
  let module Word = WordSeq(Char) in
  let module Stream = Segments.ThunkList(Word) in
  let generator =
    Regenerate.arbitrary
      (module Word) (* Datastructure for words *)
      (module Stream) (* Datastructure for streams of words *)
      ~compl:false (* Should we generate complement operations? *)
      ~pp:Fmt.char (* Printer for characters *)
      ~samples:100 (* Average number of samples for each regular expression *)
      alphabet (* Alphabet *)
  in
  let generator = add_boundaries generator in
  QCheck.Test.make generator check

let () =
  QCheck_runner.set_verbose false;
  QCheck_runner.set_long_tests false;
  QCheck_runner.run_tests_main [test]
