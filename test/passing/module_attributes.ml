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

include (
  List :
    module type of Foo with module A := A [@warning "-3"] with module B := B )

include (
  List :
    (module type of Foo
    with module A := A [@warning "-3"] [@warning "-3"]
    with module B := B [@warning "-3"]) )

include (
  List :
    (module type of Pervasives
    with module A := A [@warning "-3"] [@warning "-3"]
    with module B := B [@warning "-3"] [@warning "-3"]) ) [@warning "-3"]
