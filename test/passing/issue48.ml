module X (* : sig val x : unit -> unit end *) = struct
  let x () = print_endline "coucou"
end
