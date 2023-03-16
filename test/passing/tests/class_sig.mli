class c : 'a -> object
  val x : 'b
end

(** Fitting *)

class c : object end

class c : int -> object end

class c : int -> object end[@attr]

class c : int -> object end [@@attr]

class c : int -> object end

class c (* a *) : (* b *) int (* c *) -> (* d *) object (* e *) end (* f *)

class c : object end
