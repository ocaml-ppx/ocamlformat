(** Extension of Cmdliner supporting lighter-weight option definition *)

include module type of Cmdliner

val mk : default:'a -> 'a Term.t -> 'a ref
(** [mk ~default term] is a ref which, after [parse] is called, contains
    the value of the command line option specified by [term]. *)

val parse : Term.info -> (unit -> 'a Term.ret) -> 'a Term.result
(** [parse info validate] parses the command line according to the options
    declared by calls to [mk], using manual and version [info], and calling
    [validate] to check usage constraints not expressible in the [Term]
    language. *)
