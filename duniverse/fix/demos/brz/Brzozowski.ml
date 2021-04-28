open Fix
open Printf

let exists = List.exists
let flatten = List.flatten
let forall = List.for_all
let map = List.map
let sort = List.sort
let uniq = ListAux.uniq

module Generic = struct
  let compare = compare
end

(* -------------------------------------------------------------------------- *)

(* During the DFA construction, if the DFA is used in first-match mode, where
   one stops as soon as an accepting state is encountered, then a final state
   need not have any outgoing transitions; that would be pointless. However,
   if the DFA is used in all-matches mode, then, when a final state is
   reached, one records that an accepting state was reached and continues
   reading the input. *)

(* The following global flag controls this. Quick and dirty. Not pretty. *)

let accepting_state_can_have_successors =
  ref true

(* -------------------------------------------------------------------------- *)

(* This module is parameterized over an alphabet. *)

module Make (Char : sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val foreach: (t -> unit) -> unit
  val print: t -> string
end) = struct

(* [exists_char f] determines whether there exists a character [a] such that
   [f a] is true. *)

let exists_char (f : Char.t -> bool) : bool =
  let exception Exists in
  try
    Char.foreach (fun c -> if f c then raise Exists);
    false
  with Exists ->
    true

(* -------------------------------------------------------------------------- *)

(* See Scott Owens, John Reppy, Aaron Turon, "Regular-expression derivatives
   re-examined", JFP 19(2):173-190, 2009. *)

(* We wish to hash-cons expressions (that is, assign unique identifiers to
   them) up to the equational theory that appears in Definition 4.1 in Owens
   et al. This leads us to use n-ary forms of disjunction and conjunction, so
   as to be able to efficiently sort the disjuncts (or conjuncts) and remove
   duplicate elements. *)

(* In [EDisj es] and [EConj es], the list [es] is never a singleton list. It
   is sorted and has no duplicate elements. *)

type regexp =
  skeleton HashCons.cell

and skeleton =
  | EEpsilon
  | EChar of Char.t
  | ECat of regexp * regexp
  | EStar of regexp
  | EDisj of regexp list
  | EConj of regexp list
  | ENeg of regexp

(* -------------------------------------------------------------------------- *)

(* Set up hash-consing. *)

let make : skeleton -> regexp =

  let module H = HashCons.ForHashedType(struct

    (* Define equality and hashing of skeletons. The functions [HashCons.equal]
       and [HashCons.hash] can be used, one level down, to test the equality of
       two expressions and to hash an expression. *)

    type t = skeleton

    let equal (s1 : skeleton) (s2 : skeleton) =
      let equal = HashCons.equal in
      match s1, s2 with
      | EEpsilon, EEpsilon ->
          true
      | EChar c1, EChar c2 ->
          Char.equal c1 c2
      | ECat (e1, f1), ECat (e2, f2) ->
          equal e1 e2 && equal f1 f2
      | EStar e1, EStar e2
      | ENeg e1, ENeg e2 ->
          equal e1 e2
      | EDisj es1, EDisj es2
      | EConj es1, EConj es2 ->
          ListAux.equal equal es1 es2
      | _, _ ->
          false

    (* A way of combining two hash codes into one. *)

    let (%%) (h1 : int) (h2 : int) : int =
      Hashtbl.hash (h1, h2)

    let hash (s : skeleton) =
      let hash = HashCons.hash in
      match s with
      | EEpsilon ->
          0
      | EChar c ->
          1 %% Char.hash c
      | ECat (e1, e2) ->
          2 %% hash e1 %% hash e2
      | EStar e ->
          3 %% hash e
      | EDisj es ->
          List.fold_left (fun h e -> h %% hash e) 4 es
      | EConj es ->
          List.fold_left (fun h e -> h %% hash e) 5 es
      | ENeg e ->
          6 %% hash e

  end)
  in H.make

(* Because [HashCons.cell] is an abstract type, the function [make] is the
   only way of constructing an expression. Provided we are careful to apply
   [make] only to an expression that is in normal form with respect to the
   equations in Definition 4.1, we can be certain that every expression is in
   normal form. *)

(* [skeleton] allows inspecting the structure of an expression. *)

let skeleton : regexp -> skeleton =
  HashCons.data

(* -------------------------------------------------------------------------- *)

(* The type [regexp] inherits equality, comparison, and hashing functions from
   [HashCons]. *)

module R = struct
  type t = regexp
  let equal = HashCons.equal
  let compare = HashCons.compare
  let hash = HashCons.hash
end

include R

(* -------------------------------------------------------------------------- *)

(* The empty expression [0] is encoded as 0-ary disjunction. *)

let zero : regexp =
  make (EDisj [])

(* The universal expression [1] is encoded as 0-ary conjunction. *)

let one : regexp =
  make (EConj [])

(* Epsilon. *)

let epsilon : regexp =
  make EEpsilon

(* A character. *)

let char c =
  make (EChar c)

(* -------------------------------------------------------------------------- *)

(* Concatenation. *)

let rec (@@) (e1 : regexp) (e2 : regexp) : regexp =
  match skeleton e1, skeleton e2 with
  | EDisj [], _
  | _, EDisj [] ->
      zero
  | EEpsilon, _ ->
      e2
  | _, EEpsilon ->
      e1
  | ECat (e1, f1), _ ->
      e1 @@ f1 @@ e2
  | _, _ ->
      make (ECat (e1, e2))

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let star (e : regexp) : regexp =
  match skeleton e with
  | EEpsilon
  | EStar _ ->
      (* [epsilon*] is [epsilon]. *)
      (* [e**] is [e*]. *)
      e
  | EDisj [] ->
      (* [0*] is [epsilon]. *)
      epsilon
  | _ ->
      make (EStar e)

(* -------------------------------------------------------------------------- *)

(* Disjunction. *)

let universal e =
  match skeleton e with EConj [] -> true | _ -> false

let undisj (e : regexp) : regexp list =
  match skeleton e with EDisj es -> es | _ -> [e]

let undisj (es : regexp list) : regexp list =
  flatten (map undisj es)

let disjunction (es : regexp list) : regexp =
  (* [1 ||| e] is [1]. *)
  if exists universal es then one else
  (* Disjunction is ACI. *)
  let es = es |> undisj |> uniq compare |> sort compare in
  (* Avoid disjunctions of one element. *)
  match es with [e] -> e | _ -> make (EDisj es)

let (|||) e1 e2 =
  disjunction [e1; e2]

(* -------------------------------------------------------------------------- *)

(* Conjunction. *)

let empty e =
  match skeleton e with EDisj [] -> true | _ -> false

let unconj (e : regexp) : regexp list =
  match skeleton e with EConj es -> es | _ -> [e]

let unconj (es : regexp list) : regexp list =
  flatten (map unconj es)

let conjunction (es : regexp list) : regexp =
  (* [0 &&& e] is [0]. *)
  if exists empty es then zero else
  (* Conjunction is ACI. *)
  let es = es |> unconj |> uniq compare |> sort compare in
  (* Avoid conjunctions of one element. *)
  match es with [e] -> e | _ -> make (EConj es)

let (&&&) e1 e2 =
  conjunction [e1; e2]

(* -------------------------------------------------------------------------- *)

(* Negation. *)

(* One could systematically push negation down through conjunction (turning it
   into a disjunction) and through disjunction (turning into a conjunction). It
   is done here only in the case of empty conjunctions and disjunctions. *)

let neg (e : regexp) : regexp =
  match skeleton e with
  | ENeg e ->
      (* [~~e] is [e]. *)
      e
  | EDisj [] ->
      (* [~0] is [1]. *)
      one
  | EConj [] ->
      (* [~1] is [0]. *)
      zero
  | _ ->
      make (ENeg e)

(* -------------------------------------------------------------------------- *)

(* [nullable e] determines whether [e] accepts the empty string, that
   is, whether the intersection of [e] with [epsilon] is nonempty. *)

(* We memoize this function. (This is optional.) *)

let nullable : regexp -> bool =
  let module M = Memoize.ForHashedType(R) in
  M.fix (fun nullable e ->
    match skeleton e with
    | EChar _ ->
        false
    | EEpsilon
    | EStar _ ->
        true
    | ECat (e1, e2) ->
        nullable e1 && nullable e2
    | EDisj es ->
        exists nullable es
    | EConj es ->
        forall nullable es
    | ENeg e ->
        not (nullable e)
  )

(* -------------------------------------------------------------------------- *)

(* [delta a e] is the Brzozowski derivative of [e] with respect to [a]. It can
   be semantically defined as the set { w | a.w \in e }. It is characterized
   by the equation [e & ( a. 1* ) = a . delta a e]. *)

(* We memoize this function. (This seems required in order to achieve good
   complexity. A naive version of derivation would have exponential cost, due
   to the duplication that takes place in the cases of catenation and Kleene
   star.) More precisely, for every character [a], [delta a] is a function of
   type [regexp -> regexp]: we memoize every function [delta a] separately. *)

let delta : Char.t -> regexp -> regexp =
  let module C = Memoize.ForHashedType(Char) in
  let module M = Memoize.ForHashedType(R) in
  C.memoize (fun a ->
    M.fix (fun delta e ->
      match skeleton e with
      | EEpsilon ->
          zero
      | EChar b ->
          if Char.equal a b then epsilon else zero
      | ECat (e1, e2) ->
          delta e1 @@ e2 ||| if nullable e1 then delta e2 else zero
      | EStar e ->
          delta e @@ star e
      | EDisj es ->
          disjunction (map delta es)
      | EConj es ->
          conjunction (map delta es)
      | ENeg e ->
          neg (delta e)
    )
  )

(* -------------------------------------------------------------------------- *)

(* [nonempty e] determines whether [e] is nonempty, that is, whether [e]
   accepts a nonempty language. *)

(* This test is based on the following fact: [e] is nonempty if either [e] is
   nullable or there exists a character [a] such that [delta a e] is nonempty.
   This is an inductive characterization: [nonempty] is the least fixed point
   of this equation. Thus, it can be defined as an instance of [Fix.lfp]. *)

(* One isolated nonemptiness query can be quite expensive, but (thanks to
   memoization), the cost of a sequence of queries is less than the sum of
   the costs of each query, considered in isolation; there is shared work. *)

let nonempty : regexp -> bool =
  let module F = Fix.ForHashedType(R)(Prop.Boolean) in
  F.lfp (fun e nonempty ->
    nullable e || exists_char (fun a -> nonempty (delta a e))
  )

(* -------------------------------------------------------------------------- *)

(* An internal representation of deterministic finite-state automata (DFAs). *)

(* We choose to represent a state as an integer in the range [0..n), where
   [n] is the number of states of the automaton. *)

type state =
  int

(* An automaton is then described as follows: *)

type dfa = {
  (* The number of states. *)
  n: int;
  (* An optional initial state. If it is absent, then the automaton is
     empty: it rejects every input. *)
  init: state option;
  (* A mapping of every state to the expression that this state accepts.
     This expression is guaranteed to be nonempty. This state is a final
     state if and only if this expression is nullable. *)
  decode: state -> regexp;
  (* The transition function, a mapping of every state and character to
     an optional target state. *)
  transition: state -> Char.t -> state option;
}

(* It is guaranteed that all states are reachable from the initial state: that
   is, there is a path from the initial state to every state. It is guaranteed
   that all states are nonempty: that is, there is a path from every state to
   some final state. *)

(* -------------------------------------------------------------------------- *)

(* Constructing a DFA for an expression [e]. *)

(* We wish to define a mapping of nonempty expressions to states. We must be
   careful to avoid divergence, by recognizing expressions that have been
   encountered already. To do this, we proceed in two steps. First, we
   discover the finite set of expressions that are obtained from [e] by
   repeated derivation. We assign a unique number to each of these reachable
   expressions. Then, we construct a DFA whose states are the reachable
   expressions and whose transitions correspond to derivation. *)

let may_have_successors (e : regexp) : bool =
  !accepting_state_can_have_successors || not (nullable e)

let dfa (e : regexp) : dfa =
  (* Discover and number the nonempty reachable expressions. The most
     nontrivial aspect of this phase is termination. The fact that expressions
     are considered equal modulo a certain equational theory is crucial here.
     The equational theory used by Owens et al. is actually stronger than
     strictly necessary to ensure termination. A stronger theory results in
     more expressions being identified, therefore smaller automata. *)
  let module G = struct
    type t = regexp
    let foreach_successor e yield =
      if may_have_successors e then
        (* The successors of [e] are its derivatives along every character
           [a], provided they are nonempty. *)
        Char.foreach (fun a ->
          let e' = delta a e in
          if nonempty e' then yield e'
        )
    (* The single root is [e], if it is nonempty. *)
    let foreach_root yield =
      if nonempty e then yield e
  end in
  let module N = GraphNumbering.ForHashedType(R)(G) in
  (* We have [n] states which are mapped to nonempty expressions by [decode]. *)
  let n, decode = N.n, N.decode in
  (* A nonempty expression is represented by a state,
     whereas an empty expression is not. *)
  let encode e : state option =
    if nonempty e then Some (N.encode e) else None
  in
  (* The initial state is the encoding of [e]. *)
  let init = encode e in
  (* The transition function is as follows. *)
  let transition q a =
    if may_have_successors (decode q) then
      encode (delta a (decode q))
    else
      None
  in
  (* We are done. *)
  { n; init; decode; transition }

(* The transition function uses [decode], [delta], and [encode]. All of these
   functions are memoized, so taking a transition is a matter of just a few
   table lookups (four of them, I think). In other words, at automaton runtime,
   no Brzozowski derivatives are computed. (That would be a bit expensive.)

   That said, if desired, the number of table lookups involved in a transition
   could be reduced by tabulating the transition function, as follows:

     let transition =
       let module Q = Tabulate.ForIntSegment(N) in
       let module T = Tabulate.ForHashedType(Char)(Char) in
       Q.tabulate (fun q -> T.tabulate (fun a -> transition q a))
     in
     ...

 *)

(* -------------------------------------------------------------------------- *)

(* Running a DFA. *)

(* There seem to be two natural conditions for success. One condition is to
   declare success when a final state is reached, even if the input is not
   exhausted. The other is to declare success when a final state is reached
   and the input is exhausted. Here, we choose the first condition. Upon
   success, we return a pair of the current offset [i] and the remainder of
   the input, so (if desired) it is easy to check a posteriori whether the end
   of the input was reached. *)

(* Beside, if [accepting_state_can_have_successors] is [true], then it makes
   sense to offer a variant of [exec] that returns no just the first match,
   but a list of all matches. This is not done here. *)

type input = Char.t Seq.t

let rec exec (a : dfa) (q : state) (input : input) (i : int) =
  let { decode; transition; _ } = a in
  if nullable (decode q) then
    (* [q] is final. Accept. *)
    Some (input, i)
  else
    match input() with
    | Seq.Nil ->
        (* No more input. Fail. *)
        None
    | Seq.Cons (c, input) ->
        (* The next input character is [c]. Attempt to follow a transition
           out of [q] along [c]. *)
        exec_opt a (transition q c) input (i+1)

and exec_opt a (oq : state option) input i =
  match oq with
  | None ->
      (* There is no transition. Fail. *)
      None
  | Some q ->
      (* There is transition to state [q]. Follow it. *)
      exec a q input i

let exec a input =
  let { init; _ } = a in
  exec_opt a init input 0

(* -------------------------------------------------------------------------- *)

(* An expression printer. Not guaranteed to conform to any standard. *)

let rec print0 b e =
  match skeleton e with
  | EEpsilon ->
      bprintf b ""
  | EChar c ->
      bprintf b "%s" (Char.print c)
  | EDisj [] ->
      (* This notation could be confused with a character. *)
      bprintf b "0"
  | EConj [] ->
      (* This notation could be confused with a character. *)
      bprintf b "1"
  | EStar e ->
      bprintf b "%a*" print0 e
  | _ ->
      bprintf b "(%a)" print e

and print1 b e =
  match skeleton e with
  | ECat (e1, e2) ->
      bprintf b "%a%a" print0 e1 print1 e2
  | _ ->
      print0 b e

and print2 b e =
  match skeleton e with
  | ENeg e ->
      bprintf b "~%a" print2 e
  | _ ->
      print1 b e

and print3 b e =
  match skeleton e with
  | EConj (e :: es) ->
      bprintf b "%a" print2 e;
      List.iter (fun e ->
        bprintf b "&%a" print2 e
      ) es
  | _ ->
      print2 b e

and print4 b e =
  match skeleton e with
  | EDisj (e :: es) ->
      bprintf b "%a" print3 e;
      List.iter (fun e ->
        bprintf b "|%a" print3 e
      ) es
  | _ ->
      print3 b e

and print b e =
  print4 b e

let print e =
  let b = Buffer.create 128 in
  print b e;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* A DFA printer. *)

let size { n; _ } =
  n

let dump f { n; init; decode; transition } =
  let final q = nullable (decode q) in
  (* Header. *)
  fprintf f "digraph G {\n";
  fprintf f "rankdir = LR;\n";
  fprintf f "ratio = auto;\n";
  (* Vertices. *)
  for q = 0 to n - 1 do
    fprintf f "q%02d [ label=\"%s\", style = solid, shape = %s ] ;\n"
      q
      (print (decode q))
      (if final q then "doublecircle" else "circle")
  done;
  (* Indicate the initial state. *)
  fprintf f "\"\" [ shape = none ]";
  begin match init with
  | None ->
      ()
  | Some init ->
      fprintf f "\"\" -> q%02d [ label = \"\" ]" init
  end;
  (* Edges. *)
  for q = 0 to n - 1 do
    (* We group [q]'s outgoing edges per target state. Thus, if several edges
       have the same target state, we draw a single transition, with multiple
       labels, separated with "|". *)
    let edges = ref [] in
    Char.foreach (fun a ->
      match transition q a with
      | None ->
          fprintf f "// No transition out of q%02d along %s\n"
            q (Char.print a)
      | Some q' ->
          edges := (a, q') :: !edges
    );
    let edges = List.rev !edges in
    let cmp = Generic.compare in
    let targets = map snd edges |> sort cmp |> uniq cmp in
    let labels target : Char.t list =
      edges |> List.filter (fun (_, q') -> target = q') |> map fst
    in
    List.iter (fun q' ->
      let label = String.concat "|" (labels q' |> map Char.print) in
      fprintf f "q%02d -> q%02d [ label=\"%s\" ] ;\n"
        q q' label
    ) targets
  done;
  fprintf f "}\n"

(* -------------------------------------------------------------------------- *)

end
