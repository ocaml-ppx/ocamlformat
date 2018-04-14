module Generative () = struct
  
end
 
module M = Generative ()

module M = String_id (M) ()
