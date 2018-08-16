include (functor [@warning "item"] (M : S) -> N) [@@warning "structure"]

include struct
  type t
end [@warning "item"] [@@warning "structure"]

include M [@warning "item"] [@@warning "structure"]

include (M : S) [@warning "item"] [@@warning "structure"]

include M (N) [@warning "item"] [@@warning "structure"]

include [%ext] [@warning "item"] [@@warning "structure"]

include (val M) [@warning "item"] [@@warning "structure"]

include ( val Aaaaaaaaaaaaaaaa.Bbbbbbbbbbbbbbbb.Cccccccccccccccc
              .Dddddddddddddddd ) [@warning "item"] [@@warning "structure"]
