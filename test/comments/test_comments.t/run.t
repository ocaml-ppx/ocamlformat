For every tokens in the test file, insert a comment at that position, format
then compute the diff at the token level.

File "test.ml" should contain most syntaxes.

  $ test_comments test.ml
  insertion offset = 0
  before: (* toto *) module M
   after: (* toto *) <NL> <NL> module M
  insertion offset = 13
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 14
  before: (* toto *) module M
   after: (* toto *) <NL> <NL> module M
  insertion offset = 23
  before: M (* toto *) : sig <NL> type t <NL> <NL> val f : t -> t <NL> end = struct <NL>
   after: M : sig <NL> type t <NL> <NL> val f : t -> t <NL> end = (* toto *) struct <NL>
  insertion offset = 28
  before: sig (* toto *) <NL>
   after: sig <NL> (* toto *) <NL>
  insertion offset = 31
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 36
  before: <NL> type (* toto *) t <NL>
   after: <NL> (* toto *) <NL> type t <NL>
  insertion offset = 38
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 41
  before: (* toto *) val f
   after: (* toto *) <NL> <NL> val f
  insertion offset = 56
  before: <NL> (* toto *) end =
   after: <NL> <NL> (* toto *) <NL> end =
  insertion offset = 68
  before: struct (* toto *) <NL>
   after: struct <NL> (* toto *) <NL>
  insertion offset = 71
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 76
  before: <NL> type (* toto *) t =
   after: <NL> (* toto *) <NL> type t =
  insertion offset = 78
  before: t (* toto *) = A | B of int * int | C of { a : int ; b : int } <NL> <NL>
   after: t = A | B of int * int | C of { a : int ; b : int } <NL> <NL> (* toto *) <NL> <NL>
  insertion offset = 82
  before: = A (* toto *) | B of int * int | C
   after: = <NL> | A <NL> (* toto *) <NL> | B of int * int <NL> | C
  insertion offset = 99
  before: = A | B of int * int (* toto *) | C
   after: = <NL> | A <NL> | B of int * int <NL> (* toto *) <NL> | C
  insertion offset = 106
  before: of (* toto *) { a :
   after: of { (* toto *) a :
  insertion offset = 123
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 126
  before: (* toto *) let f
   after: (* toto *) <NL> <NL> let f
  insertion offset = 137
  before: x (* toto *) <NL>
   after: x <NL> <NL> (* toto *) <NL>
  insertion offset = 138
  before: <NL> (* toto *) end <NL>
   after: <NL> <NL> (* toto *) <NL> end <NL>
  insertion offset = 141
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 142
  before: <NL> (* toto *) 
   after: <NL> <NL> (* toto *) <NL> 
