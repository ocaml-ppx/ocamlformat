external ident : a -> b -> c -> d = "something"

external ident : a -> b -> c -> d = "something" "else"

val ident : a -> b -> c -> d

val ident :
  arg1_is_loooooooooooooooooooooooooooooooong -> arg2 -> arg3 -> arg4

external ident :
  arg1_is_loooooooooooooooooooooooooooooooong -> arg2 -> arg3 -> arg4
  = "something" "else"

type t = {f1: a -> b -> c; f2: int; f3: a -> b -> c -> d -> e}

type t =
  { field1: a -> b -> c
  ; field2: int
  ; field3:
         a_is_loooooooooooooooooooooooooooooooong
      -> b_is_loooooooooooooooooooooooooooooooong
      -> c
      -> d
      -> e
  ; field4:
      a_is_loooooooooooooooooooong -> b_is_loooooooooong -> c -> d -> e
  ; field5:
      a loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
      typ }
