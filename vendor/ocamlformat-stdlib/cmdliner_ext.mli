(** Extension of Cmdliner supporting lighter-weight option definition *)

include module type of struct include Cmdliner end

val parse : Cmd.info -> (unit -> 'a Term.ret) -> (unit Term.t) -> ('a Cmd.eval_ok, Cmd.eval_error) result
(** [parse info validate] parses the command line according to the options
    declared by calls to [mk], using manual and version [info], and calling
    [validate] to check usage constraints not expressible in the [Term]
    language. *)
