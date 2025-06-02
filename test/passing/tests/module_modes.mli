@@ portable

type t

val portable : t
val nonportable : t @@ nonportable

module T : sig
  val portable : t
  val portable : t @@ nonportable
end

module T : sig @@ nonportable
  val portable : t
  val portable : t @@ nonportable
end

module (T @@ nonportable) : sig @@ portable
  val portable : t
  val nonportable : t @@ nonportable
end
