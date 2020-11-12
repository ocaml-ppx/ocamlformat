For every tokens in the test file, insert a comment at that position, format
then compute the diff at the token level.

File "test.ml" should contain most syntaxes.

  $ test_comments test.ml
  insertion offset = 0
  before: (* toto *) module M
   after: (* toto *) <NL> <NL> module M
  insertion offset = 9
  before: M (* toto *) : sig <NL> (** M *) <NL> <NL> type " a t <NL> <NL> class c : <NL> " a t <NL> -> l : " a t <NL> -> ?k: " a t <NL> -> object <NL> [@@@ attr ] <NL> end <NL> <NL> module type S = sig <NL> include module type of struct end <NL> end <NL> <NL> module M : functor ( X : module type of N with type t = t ) ( ) -> <NL> S with type t = t and module N = N <NL> end = struct <NL>
   after: M : sig <NL> (** M *) <NL> <NL> type " a t <NL> <NL> class c : <NL> " a t <NL> -> l : " a t <NL> -> ?k: " a t <NL> -> object <NL> [@@@ attr ] <NL> end <NL> <NL> module type S = sig <NL> include module type of struct end <NL> end <NL> <NL> module M : functor ( X : module type of N with type t = t ) ( ) -> <NL> S with type t = t and module N = N <NL> end = (* toto *) struct <NL>
  insertion offset = 14
  before: sig (* toto *) <NL>
   after: sig <NL> (* toto *) <NL>
  insertion offset = 26
  before: <NL> (** M *) <NL> (* toto *) <NL> type
   after: <NL> (* toto *) <NL> <NL> (** M *) <NL> type
  insertion offset = 29
  before: (* toto *) type "
   after: (* toto *) <NL> <NL> type "
  insertion offset = 35
  before: <NL> type " (* toto *) a
   after: <NL> (* toto *) <NL> type " a
  insertion offset = 37
  before: <NL> type " a (* toto *) t
   after: <NL> (* toto *) <NL> type " a t
  insertion offset = 39
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 42
  before: (* toto *) class c
   after: (* toto *) <NL> <NL> class c
  insertion offset = 51
  before: : (* toto *) <NL>
   after: : <NL> (* toto *) <NL>
  insertion offset = 57
  before: " (* toto *) a t <NL>
   after: " a (* toto *) t <NL>
  insertion offset = 65
  before: t <NL> (* toto *) -> l
   after: t (* toto *) <NL> -> l
  insertion offset = 68
  before: -> (* toto *) l : " a
   after: -> l : (* toto *) " a
  insertion offset = 69
  before: l (* toto *) : " a
   after: l : (* toto *) " a
  insertion offset = 71
  before: " (* toto *) a t <NL>
   after: " a (* toto *) t <NL>
  insertion offset = 79
  before: t <NL> (* toto *) -> ?k:
   after: t (* toto *) <NL> -> ?k:
  insertion offset = 82
  before: -> (* toto *) ?k: " a
   after: -> ?k: (* toto *) " a
  insertion offset = 86
  before: " (* toto *) a t <NL>
   after: " a (* toto *) t <NL>
  insertion offset = 94
  before: t <NL> (* toto *) -> object
   after: t (* toto *) <NL> -> object
  insertion offset = 97
  before: (* toto *) object <NL>
   after: (* toto *) <NL> object <NL>
  insertion offset = 103
  before: object (* toto *) <NL>
   after: object <NL> (* toto *) <NL>
  insertion offset = 130
  before: ] <NL> (* toto *) end <NL>
   after: ] (* toto *) <NL> end <NL>
  insertion offset = 133
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 134
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 137
  before: (* toto *) module type
   after: (* toto *) <NL> <NL> module type
  insertion offset = 144
  before: module (* toto *) type S =
   after: module type (* toto *) S =
  insertion offset = 156
  before: sig (* toto *) <NL>
   after: sig <NL> (* toto *) <NL>
  insertion offset = 161
  before: (* toto *) include module
   after: (* toto *) <NL> <NL> include module
  insertion offset = 176
  before: module (* toto *) type of struct end
   after: module type of (* toto *) struct end
  insertion offset = 181
  before: type (* toto *) of struct end
   after: type of (* toto *) struct end
  insertion offset = 191
  before: struct (* toto *) end <NL>
   after: struct <NL> (* toto *) <NL> end <NL>
  insertion offset = 197
  before: <NL> (* toto *) end <NL>
   after: <NL> <NL> (* toto *) <NL> end <NL>
  insertion offset = 200
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 201
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 204
  before: (* toto *) module M
   after: (* toto *) <NL> <NL> module M
  insertion offset = 215
  before: : (* toto *) functor ( X : module type of N with type t = t ) ( ) -> <NL> S with type
   after: : functor ( X : module type of N with type t = t ) ( ) -> <NL> S (* toto *) with type
  insertion offset = 223
  before: functor (* toto *) ( X :
   after: functor ( (* toto *) X :
  insertion offset = 228
  before: : (* toto *) module type of N with type
   after: : module type of N (* toto *) with type
  insertion offset = 235
  before: module (* toto *) type of N with
   after: module type of (* toto *) N with
  insertion offset = 240
  before: type (* toto *) of N with
   after: type of (* toto *) N with
  insertion offset = 262
  before: ) (* toto *) ( ) -> <NL> S with type
   after: ) ( ) -> <NL> S (* toto *) with type
  insertion offset = 263
  before: ( (* toto *) ) -> <NL> S with type
   after: ( ) -> <NL> S (* toto *) with type
  insertion offset = 265
  before: ) (* toto *) -> <NL> S with type
   after: ) -> <NL> S (* toto *) with type
  insertion offset = 267
  before: -> (* toto *) <NL> S with type
   after: -> <NL> S (* toto *) with type
  insertion offset = 272
  before: <NL> (* toto *) S with type
   after: <NL> S (* toto *) with type
  insertion offset = 294
  before: and (* toto *) module N =
   after: and module (* toto *) N =
  insertion offset = 306
  before: N (* toto *) <NL>
   after: N <NL> <NL> (* toto *) <NL>
  insertion offset = 307
  before: <NL> (* toto *) end =
   after: <NL> <NL> (* toto *) <NL> end =
  insertion offset = 319
  before: struct (* toto *) <NL>
   after: struct <NL> (* toto *) <NL>
  insertion offset = 322
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 327
  before: <NL> type (* toto *) t =
   after: <NL> (* toto *) <NL> type t =
  insertion offset = 329
  before: t (* toto *) = <NL> | A (** A *) <NL> | B : int * int -> t <NL> | C of { a : int (** a *) ; b : int (** b *) } <NL> constraint <NL> " a = <NL> [> ` A | b ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a <NL> <NL>
   after: t = <NL> | A (** A *) <NL> | B : int * int -> t <NL> | C of { a : int (** a *) ; b : int (** b *) } <NL> constraint <NL> " a = <NL> [> ` A | b ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a <NL> <NL> (* toto *) <NL> <NL>
  insertion offset = 330
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 335
  before: (* toto *) | A
   after: (* toto *) <NL> | A
  insertion offset = 348
  before: (** A *) (* toto *) <NL>
   after: (** A *) <NL> (* toto *) <NL>
  insertion offset = 353
  before: (* toto *) | B
   after: (* toto *) <NL> | B
  insertion offset = 378
  before: (* toto *) | C
   after: (* toto *) <NL> | C
  insertion offset = 385
  before: of (* toto *) { a :
   after: of { (* toto *) a :
  insertion offset = 402
  before: int (** a *) (* toto *) ; b
   after: int (* toto *) (** a *) ; b
  insertion offset = 426
  before: } <NL> (* toto *) constraint <NL>
   after: } (* toto *) <NL> constraint <NL>
  insertion offset = 436
  before: } <NL> constraint (* toto *) <NL>
   after: } (* toto *) <NL> constraint <NL>
  insertion offset = 443
  before: } <NL> constraint <NL> (* toto *) "
   after: } (* toto *) <NL> constraint <NL> "
  insertion offset = 444
  before: " (* toto *) a = <NL>
   after: " a <NL> (* toto *) = <NL>
  insertion offset = 446
  before: a (* toto *) =
   after: a <NL> (* toto *) =
  insertion offset = 447
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 454
  before: (* toto *) [> `
   after: (* toto *) <NL> [> `
  insertion offset = 457
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 458
  before: ` (* toto *) A | b ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ` A (* toto *) | b ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 460
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 462
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 463
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 465
  before: ] (* toto *) * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> (* toto *) <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 467
  before: ] * (* toto *) [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * (* toto *) <NL> [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 470
  before: ] * [< (* toto *) ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< (* toto *) ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 471
  before: ] * [< ` (* toto *) A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 473
  before: ] * [< ` A (* toto *) > ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 475
  before: ] * [< ` A > (* toto *) ` B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 476
  before: ] * [< ` A > ` (* toto *) B ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 478
  before: ] * [< ` A > ` B (* toto *) ` C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 479
  before: ] * [< ` A > ` B ` (* toto *) C ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 480
  before: ] * [< ` A > ` B ` C (* toto *) ] * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A (* toto *) > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 482
  before: ] * [< ` A > ` B ` C ] (* toto *) * < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> (* toto *) <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 484
  before: ] * [< ` A > ` B ` C ] * (* toto *) < m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * (* toto *) <NL> < m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 486
  before: ] * [< ` A > ` B ` C ] * < (* toto *) m : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < (* toto *) m : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 487
  before: ] * [< ` A > ` B ` C ] * < m (* toto *) : t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m (* toto *) : t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 489
  before: ] * [< ` A > ` B ` C ] * < m : (* toto *) t ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : (* toto *) t ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 491
  before: ] * [< ` A > ` B ` C ] * < m : t (* toto *) ; .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t (* toto *) ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 493
  before: ] * [< ` A > ` B ` C ] * < m : t ; (* toto *) .. > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t (* toto *) ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 496
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. (* toto *) > * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t (* toto *) ; .. > <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 498
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > (* toto *) * ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> (* toto *) <NL> * ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 500
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * (* toto *) ( module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * (* toto *) <NL> ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 501
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( (* toto *) module S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * (* toto *) <NL> ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 508
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module (* toto *) S ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * (* toto *) <NL> ( module S ) <NL> * t # u <NL> as <NL> " a
  insertion offset = 509
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S (* toto *) ) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> (* toto *) <NL> * t # u <NL> as <NL> " a
  insertion offset = 511
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) (* toto *) * t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> (* toto *) <NL> * t # u <NL> as <NL> " a
  insertion offset = 513
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * (* toto *) t # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * (* toto *) <NL> t # u <NL> as <NL> " a
  insertion offset = 515
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t (* toto *) # u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t (* toto *) # u <NL> as <NL> " a
  insertion offset = 516
  before: ] * [< ` A > ` B ` C ] * < m : t ; .. > * ( module S ) * t # (* toto *) u as " a
   after: ] <NL> * [< ` A > ` B ` C ] <NL> * < m : t ; .. > <NL> * ( module S ) <NL> * t (* toto *) # u <NL> as <NL> " a
  insertion offset = 518
  before: u (* toto *) as " a
   after: u <NL> (* toto *) <NL> as <NL> " a
  insertion offset = 521
  before: u as (* toto *) " a
   after: u <NL> (* toto *) <NL> as <NL> " a
  insertion offset = 522
  before: u as " (* toto *) a
   after: u <NL> (* toto *) <NL> as <NL> " a
  insertion offset = 523
  before: a (* toto *) <NL>
   after: a <NL> <NL> (* toto *) <NL>
  insertion offset = 524
  insertion offset = 535
  before: <NL> (** f *) (* toto *) <NL> let
   after: <NL> (* toto *) <NL> <NL> (** f *) <NL> let
  insertion offset = 538
  before: <NL> (** f *) <NL> (* toto *) let f
   after: <NL> (* toto *) <NL> <NL> (** f *) <NL> let f
  insertion offset = 547
  test_comments: Cannot process "test.ml".
    Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
    BUG: comments dropped.
  insertion offset = 548
  test_comments: Cannot process "test.ml".
    Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
    BUG: comments dropped.
  insertion offset = 558
  before: f : " a . [% id ] t (* toto *) =
   after: f (* toto *) : " a . [% id ] t =
  insertion offset = 563
  before: ( (* toto *) fun X ->
   after: ( fun (* toto *) X ->
  insertion offset = 586
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 589
  before: (* toto *) module M
   after: (* toto *) <NL> <NL> module M
  insertion offset = 607
  before: ( (* toto *) ) = struct end
   after: ( ) = (* toto *) struct end
  insertion offset = 609
  before: ) (* toto *) = struct end
   after: ) = (* toto *) struct end
  insertion offset = 618
  before: struct (* toto *) end <NL>
   after: struct <NL> (* toto *) <NL> end <NL>
  insertion offset = 622
  before: <NL> (* toto *) end <NL>
   after: <NL> <NL> (* toto *) <NL> end <NL>
  insertion offset = 625
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 626
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 627
  before: (* toto *) let _
   after: (* toto *) <NL> <NL> let _
  insertion offset = 634
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 684
  before: (* Insert every expressions in sequence here *) (* toto *) <NL>
   after: (* Insert every expressions in sequence here *) <NL> (* toto *) <NL>
  insertion offset = 687
  before: (* toto *) ( module
   after: (* toto *) <NL> ( module
  insertion offset = 688
  before: ( (* toto *) module M .
   after: ( module (* toto *) M .
  insertion offset = 696
  before: M (* toto *) . N ( X
   after: M . N (* toto *) ( X
  insertion offset = 697
  before: . (* toto *) N ( X
   after: . N (* toto *) ( X
  insertion offset = 701
  before: X (* toto *) . Y ) :
   after: X . Y (* toto *) ) :
  insertion offset = 702
  before: . (* toto *) Y ) :
   after: . Y (* toto *) ) :
  insertion offset = 711
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 714
  before: (* toto *) let rec
   after: (* toto *) <NL> let rec
  insertion offset = 718
  before: let (* toto *) rec x =
   after: let rec (* toto *) x =
  insertion offset = 733
  before: ( (* toto *) lazy _ )
   after: ( lazy (* toto *) _ )
  insertion offset = 747
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 750
  before: (* toto *) f <NL>
   after: (* toto *) <NL> f <NL>
  insertion offset = 757
  before: ( (* toto *) function <NL> (* Insert every patterns here *)
   after: ( function <NL> (* toto *) <NL> (* Insert every patterns here *)
  insertion offset = 765
  before: function (* toto *) <NL>
   after: function <NL> (* toto *) <NL>
  insertion offset = 804
  before: (* Insert every patterns here *) (* toto *) <NL>
   after: (* Insert every patterns here *) <NL> (* toto *) <NL>
  insertion offset = 811
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 813
  before: <NL> | (* toto *) ( (
   after: <NL> (* toto *) <NL> | ( (
  insertion offset = 815
  before: ( ( (* toto *) ( x
   after: ( (* toto *) ( ( x
  insertion offset = 828
  before: _ | (* toto *) a , b | A , B x | ` A , ` B x | #
   after: _ <NL> (* toto *) <NL> | a , b <NL> | A , B x <NL> | ` A , ` B x <NL> | #
  insertion offset = 835
  before: _ | a , b | (* toto *) A , B x | ` A , ` B x | #
   after: _ <NL> | a , b <NL> (* toto *) <NL> | A , B x <NL> | ` A , ` B x <NL> | #
  insertion offset = 844
  before: _ | a , b | A , B x | (* toto *) ` A , ` B x | #
   after: _ <NL> | a , b <NL> | A , B x <NL> (* toto *) <NL> | ` A , ` B x <NL> | #
  insertion offset = 845
  before: ` (* toto *) A , `
   after: ` A (* toto *) , `
  insertion offset = 849
  before: ` (* toto *) B x |
   after: ` B (* toto *) x |
  insertion offset = 855
  before: _ | a , b | A , B x | ` A , ` B x | (* toto *) # t
   after: _ <NL> | a , b <NL> | A , B x <NL> | ` A , ` B x <NL> (* toto *) <NL> | # t
  insertion offset = 856
  before: _ | a , b | A , B x | ` A , ` B x | # (* toto *) t
   after: _ <NL> | a , b <NL> | A , B x <NL> | ` A , ` B x <NL> (* toto *) <NL> | # t
  insertion offset = 859
  before: t ) (* toto *) as x
   after: t (* toto *) ) as x
  insertion offset = 872
  before: (* toto *) | {
   after: (* toto *) <NL> | {
  insertion offset = 873
  before: <NL> | (* toto *) { a
   after: <NL> (* toto *) <NL> | { a
  insertion offset = 886
  before: _ ; (* toto *) _ }
   after: _ (* toto *) ; _ }
  insertion offset = 896
  before: (* toto *) | [|
   after: (* toto *) <NL> | [|
  insertion offset = 897
  before: <NL> | (* toto *) [| a
   after: <NL> (* toto *) <NL> | [| a
  insertion offset = 913
  before: (* toto *) | A
   after: (* toto *) <NL> | A
  insertion offset = 914
  before: <NL> | (* toto *) A |
   after: <NL> (* toto *) <NL> | A |
  insertion offset = 918
  before: A | (* toto *) B <NL>
   after: A (* toto *) | B <NL>
  insertion offset = 927
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 928
  before: <NL> | (* toto *) ( module
   after: <NL> (* toto *) <NL> | ( module
  insertion offset = 929
  before: ( (* toto *) module M )
   after: ( module (* toto *) M )
  insertion offset = 946
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 947
  before: <NL> | (* toto *) ( module
   after: <NL> (* toto *) <NL> | ( module
  insertion offset = 948
  before: ( (* toto *) module M :
   after: ( module (* toto *) M :
  insertion offset = 961
  before: M : S ) (* toto *) <NL>
   after: M (* toto *) : S ) <NL>
  insertion offset = 969
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 970
  before: <NL> | (* toto *) ( module
   after: <NL> (* toto *) <NL> | ( module
  insertion offset = 971
  before: ( (* toto *) module _ )
   after: ( module (* toto *) _ )
  insertion offset = 988
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 989
  before: <NL> | (* toto *) ( exception
   after: <NL> (* toto *) <NL> | ( exception
  insertion offset = 990
  before: ( (* toto *) exception E )
   after: ( exception (* toto *) E )
  insertion offset = 1010
  before: (* toto *) | [%
   after: (* toto *) <NL> | [%
  insertion offset = 1011
  before: <NL> | (* toto *) [% ppx
   after: <NL> (* toto *) <NL> | [% ppx
  insertion offset = 1025
  before: (* toto *) | M
   after: (* toto *) <NL> | M
  insertion offset = 1026
  before: <NL> | (* toto *) M .
   after: <NL> (* toto *) <NL> | M .
  insertion offset = 1028
  before: . (* toto *) ( A |
   after: . ( (* toto *) A |
  insertion offset = 1033
  before: A | (* toto *) B )
   after: A (* toto *) | B )
  insertion offset = 1043
  before: (* toto *) | {
   after: (* toto *) <NL> | {
  insertion offset = 1044
  before: <NL> | (* toto *) { x
   after: <NL> (* toto *) <NL> | { x
  insertion offset = 1048
  before: = (* toto *) ( module M :
   after: = ( module (* toto *) M :
  insertion offset = 1049
  before: ( (* toto *) module M :
   after: ( module (* toto *) M :
  insertion offset = 1062
  before: M : S ) (* toto *) }
   after: M (* toto *) : S ) }
  insertion offset = 1071
  before: (* toto *) | {
   after: (* toto *) <NL> | {
  insertion offset = 1072
  before: <NL> | (* toto *) { x
   after: <NL> (* toto *) <NL> | { x
  insertion offset = 1076
  before: { x = (* toto *) (
   after: { (* toto *) x = (
  insertion offset = 1093
  before: (* toto *) | {
   after: (* toto *) <NL> | {
  insertion offset = 1094
  before: <NL> | (* toto *) { x
   after: <NL> (* toto *) <NL> | { x
  insertion offset = 1098
  before: { x = (* toto *) (
   after: { (* toto *) x = (
  insertion offset = 1114
  before: (* toto *) | {
   after: (* toto *) <NL> | {
  insertion offset = 1115
  before: <NL> | (* toto *) { x
   after: <NL> (* toto *) <NL> | { x
  insertion offset = 1120
  before: ( (* toto *) ( x' :
   after: ( ( (* toto *) x' :
  insertion offset = 1140
  before: -> (* toto *) <NL>
   after: -> <NL> (* toto *) <NL>
  insertion offset = 1160
  before: ( (* toto *) fun ( type t )
   after: ( fun ( type (* toto *) t )
  insertion offset = 1164
  before: fun (* toto *) ( type t )
   after: fun ( type (* toto *) t )
  insertion offset = 1165
  before: ( (* toto *) type t )
   after: ( type (* toto *) t )
  insertion offset = 1175
  before: X (* toto *) ~ a ~b:
   after: X ~ (* toto *) a ~b:
  insertion offset = 1178
  before: a (* toto *) ~b: Y ?
   after: a ~b: (* toto *) Y ?
  insertion offset = 1183
  before: Y (* toto *) ? c ?c:
   after: Y ? (* toto *) c ?c:
  insertion offset = 1186
  before: c (* toto *) ?c: ( Z =
   after: c ?c: ( (* toto *) Z =
  insertion offset = 1189
  before: ?c: (* toto *) ( Z =
   after: ?c: ( (* toto *) Z =
  insertion offset = 1201
  before: a ; (* toto *) _ }
   after: a (* toto *) ; _ }
  insertion offset = 1218
  before: X ~ (* toto *) a ~b:
   after: X (* toto *) ~ a ~b:
  insertion offset = 1224
  before: a ~b: ( (* toto *) x y ) ? c
   after: a <NL> ~b: ( (* toto *) x y ) <NL> ? c
  insertion offset = 1226
  before: a ~b: ( x (* toto *) y ) ? c
   after: a <NL> ~b: ( x (* toto *) y ) <NL> ? c
  insertion offset = 1227
  before: a ~b: ( x y (* toto *) ) ? c
   after: a <NL> ~b: ( x y (* toto *) ) <NL> ? c
  insertion offset = 1230
  before: ) ? (* toto *) c ?c:
   after: ) (* toto *) ? c ?c:
  insertion offset = 1275
  before: ( (* toto *) module M :
   after: ( module (* toto *) M :
  insertion offset = 1295
  before: <NL> ( (* toto *) try a
   after: <NL> (* toto *) ( try a
  insertion offset = 1356
  before: |] (* toto *) ;
   after: |] <NL> (* toto *) ;
  insertion offset = 1357
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 1365
  before: . (* toto *) { a ,
   after: . { (* toto *) a ,
  insertion offset = 1380
  before: .* (* toto *) ( 0 )
   after: .* ( (* toto *) 0 )
  insertion offset = 1384
  before: ( 0 ) (* toto *) else
   after: ( (* toto *) 0 ) else
  insertion offset = 1392
  before: .* (* toto *) ( a ;
   after: .* ( (* toto *) a ;
  insertion offset = 1399
  before: ( a ; b ) (* toto *) ;
   after: ( (* toto *) a ; b ) ;
  insertion offset = 1400
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 1403
  before: (* toto *) for i
   after: (* toto *) <NL> for i
  insertion offset = 1424
  before: do (* toto *) <NL>
   after: do <NL> (* toto *) <NL>
  insertion offset = 1436
  before: . (* toto *) ( i )
   after: . ( (* toto *) i )
  insertion offset = 1442
  before: ) <NL> (* toto *) done ;
   after: ) (* toto *) <NL> done ;
  insertion offset = 1447
  before: done (* toto *) ;
   after: done <NL> (* toto *) ;
  insertion offset = 1448
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 1460
  before: m ( (* toto *) new M
   after: m (* toto *) ( new M
  insertion offset = 1464
  before: m ( new (* toto *) M
   after: m (* toto *) ( new M
  insertion offset = 1465
  before: M (* toto *) . c ) {< x
   after: M . c ) (* toto *) {< x
  insertion offset = 1466
  before: . (* toto *) c ) {< x
   after: . c ) (* toto *) {< x
  insertion offset = 1467
  before: c (* toto *) ) {< x
   after: c ) (* toto *) {< x
  insertion offset = 1504
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 1507
  before: (* toto *) let module
   after: (* toto *) <NL> let module
  insertion offset = 1511
  before: let (* toto *) module M =
   after: let module (* toto *) M =
  insertion offset = 1523
  before: ( (* toto *) val S .
   after: ( val (* toto *) S .
  insertion offset = 1529
  before: . (* toto *) ( M .
   after: . ( (* toto *) M .
  insertion offset = 1531
  before: M (* toto *) . N X .
   after: M . N (* toto *) X .
  insertion offset = 1532
  before: . (* toto *) N X .
   after: . N (* toto *) X .
  insertion offset = 1535
  before: X (* toto *) . Y ) .
   after: X . Y (* toto *) ) .
  insertion offset = 1536
  before: . (* toto *) Y ) .
   after: . Y (* toto *) ) .
  insertion offset = 1544
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1547
  before: (* toto *) let exception
   after: (* toto *) <NL> let exception
  insertion offset = 1551
  before: let (* toto *) exception E of
   after: let exception (* toto *) E of
  insertion offset = 1570
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1573
  before: (* toto *) let open
   after: (* toto *) <NL> let open
  insertion offset = 1581
  before: open (* toto *) ! M in
   after: open ! (* toto *) M in
  insertion offset = 1587
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1590
  before: (* toto *) let* x
   after: (* toto *) <NL> let* x
  insertion offset = 1595
  test_comments: Cannot process "test.ml".
    Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
    BUG: comments dropped.
  insertion offset = 1603
  test_comments: Cannot process "test.ml".
    Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
    BUG: comments dropped.
  insertion offset = 1613
  before: = ( (* toto *) w [@
   after: = (* toto *) ( w [@
  insertion offset = 1626
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1629
  before: (* toto *) lazy (
   after: (* toto *) <NL> lazy (
  insertion offset = 1637
  before: ( (* toto *) let* ) (
   after: ( let* (* toto *) ) (
  insertion offset = 1645
  before: ( (* toto *) function X ->
   after: ( function (* toto *) X ->
  insertion offset = 1659
  before: -> (* toto *) ( + ) )
   after: -> ( + (* toto *) ) )
  insertion offset = 1661
  before: ( (* toto *) + ) )
   after: ( + (* toto *) ) )
  insertion offset = 1664
  before: + ) (* toto *) ) (
   after: + (* toto *) ) ) (
  insertion offset = 1667
  before: ) ( (* toto *) ( *
   after: ) (* toto *) ( ( *
  insertion offset = 1669
  before: ( (* toto *) * ) [@ attr ] ) ) ;
   after: ( * ) [@ attr ] ) (* toto *) ) ;
  insertion offset = 1671
  before: * (* toto *) ) [@ attr ] ) ) ;
   after: * ) [@ attr ] ) (* toto *) ) ;
  insertion offset = 1684
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 1689
  before: <NL> 1 (* toto *) :: ~-
   after: <NL> (* toto *) 1 :: ~-
  insertion offset = 1697
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 1706
  before: ] (* toto *) <NL>
   after: ] <NL> <NL> (* toto *) <NL>
  insertion offset = 1707
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1708
  before: (* toto *) class virtual
   after: (* toto *) <NL> <NL> class virtual
  insertion offset = 1714
  before: class (* toto *) virtual c x
   after: class virtual (* toto *) c x
  insertion offset = 1726
  before: x (* toto *) ~ y ?z:
   after: x ~ (* toto *) y ?z:
  insertion offset = 1729
  before: y (* toto *) ?z: ( z' =
   after: y ?z: ( (* toto *) z' =
  insertion offset = 1732
  before: ?z: (* toto *) ( z' =
   after: ?z: ( (* toto *) z' =
  insertion offset = 1741
  before: ) (* toto *) = <NL> let
   after: ) = <NL> (* toto *) <NL> let
  insertion offset = 1742
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 1745
  before: (* toto *) let open
   after: (* toto *) <NL> let open
  insertion offset = 1749
  before: let (* toto *) open M in
   after: let open (* toto *) M in
  insertion offset = 1758
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1761
  before: (* toto *) object (
   after: (* toto *) <NL> object (
  insertion offset = 1768
  before: object (* toto *) ( self )
   after: object ( (* toto *) self )
  insertion offset = 1773
  before: self (* toto *) ) <NL> inherit
   after: self ) <NL> (* toto *) <NL> inherit
  insertion offset = 1774
  before: ) (* toto *) <NL>
   after: ) <NL> (* toto *) <NL>
  insertion offset = 1779
  before: (* toto *) inherit M
   after: (* toto *) <NL> inherit M
  insertion offset = 1788
  before: M (* toto *) . c x <NL>
   after: M . c (* toto *) x <NL>
  insertion offset = 1789
  before: . (* toto *) c x <NL>
   after: . c (* toto *) x <NL>
  insertion offset = 1792
  before: x (* toto *) <NL> <NL> val
   after: x <NL> <NL> (* toto *) <NL> val
  insertion offset = 1793
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1798
  before: (* toto *) val mutable
   after: (* toto *) <NL> val mutable
  insertion offset = 1802
  before: val (* toto *) mutable y =
   after: val mutable (* toto *) y =
  insertion offset = 1815
  before: 0 (* toto *) <NL> <NL> initializer
   after: 0 <NL> <NL> (* toto *) <NL> initializer
  insertion offset = 1816
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1821
  before: (* toto *) initializer y
   after: (* toto *) <NL> initializer y
  insertion offset = 1839
  before: x (* toto *) <NL> <NL> method
   after: x <NL> <NL> (* toto *) <NL> method
  insertion offset = 1840
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1845
  before: (* toto *) method m
   after: (* toto *) <NL> method m
  insertion offset = 1863
  before: y (* toto *) <NL> <NL> method
   after: y <NL> <NL> (* toto *) <NL> method
  insertion offset = 1864
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1869
  before: (* toto *) method n
   after: (* toto *) <NL> method n
  insertion offset = 1891
  before: >} (* toto *) <NL> <NL> method
   after: >} <NL> <NL> (* toto *) <NL> method
  insertion offset = 1892
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1897
  before: (* toto *) method virtual
   after: (* toto *) <NL> method virtual
  insertion offset = 1904
  before: method (* toto *) virtual o :
   after: method virtual (* toto *) o :
  insertion offset = 1917
  before: : # (* toto *) ct ->
   after: : (* toto *) # ct ->
  insertion offset = 1926
  before: int (* toto *) <NL>
   after: int <NL> (* toto *) <NL>
  insertion offset = 1929
  before: (* toto *) end <NL>
   after: (* toto *) <NL> end <NL>
  insertion offset = 1932
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 1933
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1934
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 1939
  before: <NL> type (* toto *) t =
   after: <NL> (* toto *) <NL> type t =
  insertion offset = 1941
  before: t (* toto *) = .. <NL> <NL>
   after: t = .. <NL> <NL> (* toto *) <NL> <NL>
  insertion offset = 1943
  before: = (* toto *) .. <NL> <NL>
   after: = .. <NL> <NL> (* toto *) <NL> <NL>
  insertion offset = 1946
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1947
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 1968
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1969
  before: (* toto *) exception E
   after: (* toto *) <NL> <NL> exception E
  insertion offset = 1985
  before: t (* toto *) <NL>
   after: t <NL> <NL> (* toto *) <NL>
  insertion offset = 1986
  before: <NL> (* toto *) 
   after: <NL> <NL> (* toto *) <NL> 
