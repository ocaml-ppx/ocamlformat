(** This module deals with management of hooks that run after specific events
    (e.g. end of build). *)

module type S = sig
  (** Register a hook called every time the event occurs. *)
  val always : (unit -> unit) -> unit

  (** Register a hook that will only be called once when the next event occurs. *)
  val once : (unit -> unit) -> unit

  (** Signalize the event and run all registered hooks. *)
  val run : unit -> unit
end

(** Every time a build ends, which includes every iteration in watch mode,
    including cancellation of build because of file changes. *)
module End_of_build : S
