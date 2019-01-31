module Cases : sig
  type t =
    { leading_space: Fmt.t
    ; bar: Fmt.t
    ; box_all: Fmt.t -> Fmt.t
    ; box_pattern_arrow: Fmt.t -> Fmt.t
    ; box_pattern_guard: Fmt.t -> Fmt.t
    ; break_before_arrow: Fmt.t
    ; break_after_arrow: Fmt.t
    ; break_after_opening_paren: Fmt.t
    ; box_rhs: Fmt.t -> Fmt.t }

  val get : Conf.t -> first:bool -> indent:int -> parens_here:bool -> t
end
