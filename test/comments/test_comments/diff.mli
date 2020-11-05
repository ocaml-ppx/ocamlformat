type index := int

type 'a command =
  | Insert of { expected : index; actual : index }
      (** Insert the element [actual.(actual)] at [expected.(expected)]. *)
  | Delete of { expected : index }
      (** Delete the element at [expected.(expected)]. *)
  | Substitute of { expected : index; actual : index }
      (** Set [expected.(expected)) := actual.(actual)]. *)

val pp_command : Format.formatter -> 'a command -> unit

type ('elt, 'container) typ =
  | Array : ('a, 'a array) typ
  | List : ('a, 'a list) typ
  | String : (char, string) typ

module Edit_script : sig
  type 'a t = 'a command list

  val apply :
    ('elt, 'container) typ ->
    actual:(index -> 'elt) ->
    'elt t ->
    'container ->
    'elt list
end

val levenshtein_script :
  ('a, 'container) typ ->
  equal:('a -> 'a -> bool) ->
  'container ->
  'container ->
  'a Edit_script.t
(** [O(n^2)]-space computation of Levenshtein edit scripts. Guarantees to be
    [O(n)] time in the case that the containers are equal. *)
