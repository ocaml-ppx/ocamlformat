module M : S with M = struct end

module M :
  S with Fooooooooooooooooooooooooooo(Foooooooooo.Foo)(Fooooooooooooo)
           (Fooooooooooooo) = struct end

module M : S (* foo *) with M = struct end

module M : S with (* fooo *) M = struct end

module rec M : S with M = struct end

module type S = sig
  module rec M : S with M
end

module type S = (S with M [@foo])
