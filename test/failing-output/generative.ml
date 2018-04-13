module Generative () = struct
  
end

module M = Generative (struct
  
end)

module M = String_id (M) ()
