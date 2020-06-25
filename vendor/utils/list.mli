val partition_map :
  'a list -> f:('a -> [`Fst of 'b | `Snd of 'c]) -> 'b list * 'c list
(** [partition_map t ~f] partitions [t] according to [f].

    @since base.v0.9.0
    @before base.v0.14.0, the type of [f] becomes
    [('a -> ('b, 'c) Either0.t)] in base.v0.14.0 *)
