[@@@ocamlformat "break-struct=natural"]

module M =
  X
    (Y)
    (struct
      let x = k
    end)

module Hash = struct
  include Hash
  module type S = S.HASH
end

module Hash = struct
  include Hash
  include Hash
  module type S = S.HASH
  module type S = S.HASH
end

module Hash = struct
  include Hash
  include Hash
  include Hash
  module type S = S.HASH
  module type S = S.HASH
  module type S = S.HASH
end

module Hash = struct
  let z = zzzzzzzzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzz zzzzzzzzzz
  let z = zzzzzzzzzzzzzz zzzzzzzzzzzzzzz zzzzzzzzzzzzzzz zzzzzzzzzzzzzzz zzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzzzz zzzzzzzzzzzzzzzzzz
  include Hash
  module type S = S.HASH
end
