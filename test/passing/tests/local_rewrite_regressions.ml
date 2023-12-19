module With_length : sig
  type 'a t = private
    { tree : 'a
             [@global]
    (* a *)
    ; length : int [@global]
    }
end = struct end
