(** Extension of Cmdliner supporting lighter-weight option definition *)

include module type of struct include Cmdliner end

val mk : default:'a -> 'a Term.t -> (unit -> 'a)
(** [mk ~default term] is a ref which, after [parse] is called, contains
    the value of the command line option specified by [term]. *)

val parse : Cmd.info -> (unit -> 'a Term.ret) -> ('a Cmd.eval_ok, Cmd.eval_error) result
(** [parse info validate] parses the command line according to the options
    declared by calls to [mk], using manual and version [info], and calling
    [validate] to check usage constraints not expressible in the [Term]
    language. *)
