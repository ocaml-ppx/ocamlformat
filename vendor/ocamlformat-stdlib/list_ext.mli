include module type of Base.List

val partition_map :
  'a list -> f:('a -> ('b, 'c) Base.Either.t) -> 'b list * 'c list
(** [partition_map t ~f] partitions [t] according to [f].

    @since base.v0.14.0 *)
