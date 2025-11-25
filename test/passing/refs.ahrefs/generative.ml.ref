module Generative () = struct end

module M = Generative ()
module M = String_id (M) ()

module F2 : functor () -> sig end = F1
module F2 : functor () () -> sig end = F1
module F2 : (*xx*) ( (*yy*) ) (*zz*) -> sig end = F1
module F2 : () -> functor [@attr] () () -> sig end = F1
module F2 : () -> functor () () -> () -> sig end = F1
module F2 : () -> () -> () -> functor () () -> () -> sig end = F1
