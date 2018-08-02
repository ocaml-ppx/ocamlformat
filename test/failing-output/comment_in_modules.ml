module M = struct (* comments *) end

module M : sig (* comments *) end = struct (* comments *) end

module type M = sig (* comments *) end

(** Xxxxxxx xxxxxxxx xx xxxxxxx xxxxxxxxxxxxx xxxxxxxxx xx xxxx *)
module Mmmmmmmmmmmmmmmmmmmmmm = Aaaaaaaaaaaaaaaaaaaaaa.
                                Bbbbbbbbbbbbbbbbbbbbbbbb

(** Xxxxxxx xxxxxxxx xx xxxxxxx xxxxxxxxxxxxx xxxxxxxxx xx xxxx *)
module Fffffffffffffff (Yyyyyyyyyyyyyyy : Z.S) =
Gggggggggg (Wwwwwwwwww.Make (Yyyyyyyyyy))
