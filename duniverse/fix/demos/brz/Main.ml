open Printf
let iter = List.iter
let map = List.map

(* -------------------------------------------------------------------------- *)

(* Instantiate Brzozowski with an alphabet restricted to 'a'-'e' and '$'. *)

module B = Brzozowski.Make(struct
  include Char
  let hash = Hashtbl.hash
  let foreach f =
    for i = Char.code 'a' to Char.code 'e' do f (Char.chr i) done;
    List.iter f [ '$' ]
  let print c =
    Printf.sprintf "%c" c
end)

open B

let a   = char 'a'
let b   = char 'b'
let c   = char 'c'
let eof = char '$'

let rec word (cs : char Seq.t) : regexp =
  match cs() with
  | Seq.Nil ->
      epsilon
  | Seq.Cons (c, cs) ->
      char c @@ word cs

let word (w : string) =
  word (String.to_seq w)

(* -------------------------------------------------------------------------- *)

(* Test routines. *)

type test =
  {
    basename: string;
    regexp: regexp;
    accepted: string list;
    rejected: string list;
  }

let exec mode test =
  Brzozowski.accepting_state_can_have_successors := mode;
  printf "Regular expression: %s\n%!" (print test.regexp);
  printf "Automaton:\n%!";
  let automaton = dfa test.regexp in
  printf "%d states.\n%!" (size automaton);
  let filename = sprintf "%s.%b.dot" test.basename mode in
  let f = open_out filename in
  dump f automaton;
  close_out f;
  printf "Written to %s.\n%!" filename;
  test.accepted |> iter (fun input ->
    if exec automaton (String.to_seq input) = None then begin
      printf "Error: the input \"%s\" should be accepted, but is rejected.\n"
        input
    end
  );
  test.rejected |> iter (fun input ->
    if exec automaton (String.to_seq input) <> None then begin
      printf "Error: the input \"%s\" should be rejected, but is accepted.\n"
        input
    end
  )

let exec test =
  exec true test;
  exec false test

(* -------------------------------------------------------------------------- *)

(* Examples. *)

let tests =
  ref []

let register test =
  tests := test :: !tests

(* [dead] *)

let () =
  register {
    basename = "dead";
    regexp = word "dead";
    accepted = [ "dead"; ];
    rejected = [ "dd"; ];
  }

(* [1dead] *)

let () =
  register {
    basename = "onedead";
    regexp = one @@ word "dead";
    accepted = [ "dadeadda"; ];
    rejected = [ "ddade"; ];
  }

(* [a(a|b)*] *)

let () =
  register {
    basename = "starab";
    regexp = a @@ star (a ||| b);
    accepted = [ "a"; "aa"; "abb"; "aaababababa"; "ac"; ];
    rejected = [ ""; "b"; "bab"; "ca"; ];
  }

(* [a(a|b)*$] *)

let test_rd =
  register {
    basename = "rd";
    regexp = a @@ star (a ||| b) @@ eof;
    accepted = [ "a$"; "aa$"; "abb$"; "aaababababa$"; ];
    rejected = [ "$"; "b$"; "bab$"; "ca$"; "ac$"; ];
  }

(* [a(a|b)*(bc)*] *)

let () =
  register {
    basename = "astarabstarbc";
    regexp = a @@ star (a ||| b) @@ star (b @@ c);
    accepted = [ "a"; "aa"; "abb"; "aaababababa"; "ac"; ] @
               [ "abc"; "abcb"; "abcbc"; "aabbcbcbc"; "aaabbbcbcbc"; "ac"; ];
    rejected = [ "ca" ];
  }

(* [1a(a|b)*(bc)*$] *)

let () =
  register {
    basename = "oneastarabstarbc";
    regexp = one @@ a @@ star (a ||| b) @@ star (b @@ c) @@ eof;
    accepted = [ "ba$"; "eeaa$"; "edabb$"; "eaaaababababa$"; "abc$"; ] @
               [ "abbc$"; "abcbc$"; "aabbcbcbc$"; "aaabbbcbcbc$"; "a$"; ];
    rejected = [ "cc$" ];
  }

(* An automaton that tests whether a word belongs in a dictionary. *)

let keywords =
  [ "cab"; "bed"; "ace"; "add"; "dead"; "dad"; ]

let () =
  register {
    basename = "dict";
    regexp = keywords |> map word |> disjunction;
    accepted = [ "cab"; "added"; "dead"; ];
    rejected = [ "deed"; ];
  }

(* An automaton that searches for a word that belongs in a dictionary. *)

let () =
  register {
    basename = "oneadddead";
    regexp = one @@ ([ "dead"; "add" ] |> map word |> disjunction);
    accepted = [ "dead"; "addead"; ];
    rejected = [ "beca"; ];
  }

(* Main. *)

let () =
  iter exec (List.rev !tests)
