open Fix
open Sigs

module Analyze
  (G : GRAMMAR)
  (M : IMPERATIVE_MAPS with type key = G.nonterminal)
  (TSet : Set.S with type elt = G.terminal)
= struct

  open G

  (* Instantiate [Fix] with keys of type [nonterminal] and properties
     of type [bool]. The ordering is [false <= true]. *)

  module F =
    Fix.Make(M)(Prop.Boolean)

  let rec nullable request = function
    | Epsilon ->
	true
    | T _ ->
	false
    | N nt ->
	request nt
    | Seq (prod1, prod2) ->
	nullable request prod1 && nullable request prod2
    | Alt (prod1, prod2) ->
	nullable request prod1 || nullable request prod2

  let nullable_nt : nonterminal -> bool =
    F.lfp (fun nt request ->
      nullable request (productions nt)
    )

  let nullable_prod : production -> bool =
    nullable nullable_nt

  let rec inhabited request = function
    | Epsilon
    | T _ ->
	true
    | N nt ->
	request nt
    | Seq (prod1, prod2) ->
	inhabited request prod1 && inhabited request prod2
    | Alt (prod1, prod2) ->
	inhabited request prod1 || inhabited request prod2

  let inhabited_nt : nonterminal -> bool =
    F.lfp (fun nt request ->
      inhabited request (productions nt)
    )

  let inhabited_prod : production -> bool =
    inhabited inhabited_nt

  (* Instantiate [Fix] with keys of type [nonterminal] and properties
     of type [TSet.t]. The ordering is set inclusion. *)

  module FF =
    Fix.Make(M)(Prop.Set(TSet))

  let delta (b : bool) (s : TSet.t Lazy.t) : TSet.t =
    if b then Lazy.force s else TSet.empty

  let rec first request = function
    | Epsilon ->
        TSet.empty
    | T t ->
        TSet.singleton t
    | N nt ->
	 request nt
    | Seq (prod1, prod2) ->
         TSet.union
  	   (delta (inhabited_prod prod2) (lazy (first request prod1)))
           (delta (nullable_prod prod1) (lazy (first request prod2)))
    | Alt (prod1, prod2) ->
        TSet.union (first request prod1) (first request prod2)

  (* [first (t, nt)] returns [true] iff the nonterminal symbol [nt] can
     expand to a sentence that begins with the terminal symbol [t]. *)
  let first =
    FF.lfp (fun nt request ->
      first request (G.productions nt)
    )

end
