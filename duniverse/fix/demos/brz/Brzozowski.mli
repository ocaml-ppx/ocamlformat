(* This module offers an implementation of regular expressions. *)

(* It is parameterized over an alphabet. *)

module Make (Char : sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val foreach: (t -> unit) -> unit
  val print: t -> string
end) : sig

  (* Several internal memoization tables are created when this functor is
     applied. First, regular expressions are hash-consed, which means that the
     constructors below will never return two physically distinct yet
     logically equal expressions. (This notion of logical equality includes
     the equations in Definition 4.1 in Owens et al.'s paper.) Second, the
     functions [nullable], [nonempty] and [delta] are memoized. *)

  (* The type of regular expressions. *)

  type regexp
  type t = regexp

  val equal: regexp -> regexp -> bool
  val hash: regexp -> int

  (* Constructors. *)

  val epsilon: regexp
  val char: Char.t -> regexp
  val (@@): regexp -> regexp -> regexp
  val star: regexp -> regexp
  val zero: regexp
  val (|||): regexp -> regexp -> regexp
  val disjunction: regexp list -> regexp
  val one: regexp
  val (&&&): regexp -> regexp -> regexp
  val conjunction: regexp list -> regexp
  val neg: regexp -> regexp

  (* Display. *)

  val print: regexp -> string

  (* [nullable e] determines whether [e] accepts the empty string, that
     is, whether the intersection of [e] with [epsilon] is nonempty. *)

  val nullable: regexp -> bool

  (* [nonempty e] determines whether [e] is nonempty, that is, whether [e]
     accepts a nonempty language. *)

  val nonempty: regexp -> bool

  (* [delta a e] is the Brzozowski derivative of [e] with respect to [a].
     It can be semantically defined as the set { w | a.w \in e }. It is
     characterized by the equation [e & ( a. 1* ) = a . delta a e]. *)

  val delta: Char.t -> regexp -> regexp

  (* Converting a regular expression to a deterministic finite automaton. *)

  type dfa
  type input = Char.t Seq.t

  val dfa: regexp -> dfa
  val exec: dfa -> input -> (input * int) option
  val size: dfa -> int
  val dump: out_channel -> dfa -> unit

end

(* Only for this demo. *)

val accepting_state_can_have_successors: bool ref
