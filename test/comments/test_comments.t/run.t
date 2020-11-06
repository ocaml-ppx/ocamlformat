For every tokens in the test file, insert a comment at that position, format
then compute the diff at the token level.

File "test.ml" should contain most syntaxes.

  $ test_comments test.ml
  insertion offset = 0
  before: (* toto *) module M
   after: (* toto *) <NL> <NL> module M
  insertion offset = 9
  before: M (* toto *) : sig <NL> (** M *) <NL> <NL> type t <NL> <NL> class c : <NL> " a t <NL> -> l : " a t <NL> -> ?k: " a t <NL> -> object <NL> [@@@ attr ] <NL> end <NL> end = struct <NL>
   after: M : sig <NL> (** M *) <NL> <NL> type t <NL> <NL> class c : <NL> " a t <NL> -> l : " a t <NL> -> ?k: " a t <NL> -> object <NL> [@@@ attr ] <NL> end <NL> end = (* toto *) struct <NL>
  insertion offset = 14
  before: sig (* toto *) <NL>
   after: sig <NL> (* toto *) <NL>
  insertion offset = 26
  before: <NL> (** M *) <NL> (* toto *) <NL> type
   after: <NL> (* toto *) <NL> <NL> (** M *) <NL> type
  insertion offset = 29
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 34
  before: <NL> type (* toto *) t <NL>
   after: <NL> (* toto *) <NL> type t <NL>
  insertion offset = 36
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 39
  before: (* toto *) class c
   after: (* toto *) <NL> <NL> class c
  insertion offset = 48
  before: : (* toto *) <NL>
   after: : <NL> (* toto *) <NL>
  insertion offset = 54
  before: " (* toto *) a t <NL>
   after: " a (* toto *) t <NL>
  insertion offset = 62
  before: t <NL> (* toto *) -> l
   after: t (* toto *) <NL> -> l
  insertion offset = 65
  before: -> (* toto *) l : " a
   after: -> l : (* toto *) " a
  insertion offset = 66
  before: l (* toto *) : " a
   after: l : (* toto *) " a
  insertion offset = 68
  before: " (* toto *) a t <NL>
   after: " a (* toto *) t <NL>
  insertion offset = 76
  before: t <NL> (* toto *) -> ?k:
   after: t (* toto *) <NL> -> ?k:
  insertion offset = 79
  before: -> (* toto *) ?k: " a
   after: -> ?k: (* toto *) " a
  insertion offset = 83
  before: " (* toto *) a t <NL>
   after: " a (* toto *) t <NL>
  insertion offset = 91
  before: t <NL> (* toto *) -> object
   after: t (* toto *) <NL> -> object
  insertion offset = 94
  before: (* toto *) object <NL>
   after: (* toto *) <NL> object <NL>
  insertion offset = 100
  before: object (* toto *) <NL>
   after: object <NL> (* toto *) <NL>
  insertion offset = 127
  before: ] <NL> (* toto *) end <NL>
   after: ] (* toto *) <NL> end <NL>
  insertion offset = 130
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 131
  before: <NL> (* toto *) end =
   after: <NL> <NL> (* toto *) <NL> end =
  insertion offset = 143
  before: struct (* toto *) <NL>
   after: struct <NL> (* toto *) <NL>
  insertion offset = 146
  before: (* toto *) type t
   after: (* toto *) <NL> <NL> type t
  insertion offset = 151
  before: <NL> type (* toto *) t =
   after: <NL> (* toto *) <NL> type t =
  insertion offset = 153
  before: t (* toto *) = <NL> | A (** A *) <NL> | B : int * int -> t <NL> | C of { a : int (** a *) ; b : int (** b *) } <NL> <NL>
   after: t = <NL> | A (** A *) <NL> | B : int * int -> t <NL> | C of { a : int (** a *) ; b : int (** b *) } <NL> <NL> (* toto *) <NL> <NL>
  insertion offset = 154
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 159
  before: (* toto *) | A
   after: (* toto *) <NL> | A
  insertion offset = 172
  before: (** A *) (* toto *) <NL>
   after: (** A *) <NL> (* toto *) <NL>
  insertion offset = 177
  before: (* toto *) | B
   after: (* toto *) <NL> | B
  insertion offset = 202
  before: (* toto *) | C
   after: (* toto *) <NL> | C
  insertion offset = 209
  before: of (* toto *) { a :
   after: of { (* toto *) a :
  insertion offset = 226
  before: int (** a *) (* toto *) ; b
   after: int (* toto *) (** a *) ; b
  insertion offset = 245
  before: } (* toto *) <NL>
   after: } <NL> <NL> (* toto *) <NL>
  insertion offset = 246
  before: } <NL> (* toto *) <NL> (** f *) <NL>
   after: } (** f *) <NL> <NL> (* toto *) <NL> <NL>
  insertion offset = 257
  before: <NL> (** f *) (* toto *) <NL> let
   after: <NL> (* toto *) <NL> <NL> (** f *) <NL> let
  insertion offset = 260
  before: <NL> (** f *) <NL> (* toto *) let f
   after: <NL> (* toto *) <NL> <NL> (** f *) <NL> let f
  insertion offset = 279
  before: <NL> (* toto *) end <NL>
   after: <NL> <NL> (* toto *) <NL> end <NL>
  insertion offset = 282
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 283
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 284
  before: (* toto *) let _
   after: (* toto *) <NL> <NL> let _
  insertion offset = 291
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 341
  before: (* Insert every expressions in sequence here *) (* toto *) <NL>
   after: (* Insert every expressions in sequence here *) <NL> (* toto *) <NL>
  insertion offset = 344
  before: (* toto *) x ;
   after: (* toto *) <NL> x ;
  insertion offset = 347
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 351
  before: ( (* toto *) module M .
   after: ( module (* toto *) M .
  insertion offset = 359
  before: M (* toto *) . N ( X
   after: M . N (* toto *) ( X
  insertion offset = 360
  before: . (* toto *) N ( X
   after: . N (* toto *) ( X
  insertion offset = 364
  before: X (* toto *) . Y ) :
   after: X . Y (* toto *) ) :
  insertion offset = 365
  before: . (* toto *) Y ) :
   after: . Y (* toto *) ) :
  insertion offset = 374
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 377
  before: (* toto *) let rec
   after: (* toto *) <NL> let rec
  insertion offset = 381
  before: let (* toto *) rec x =
   after: let rec (* toto *) x =
  insertion offset = 393
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 396
  before: (* toto *) let _
   after: (* toto *) <NL> let _
  insertion offset = 411
  before: ( (* toto *) lazy _ )
   after: ( lazy (* toto *) _ )
  insertion offset = 425
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 428
  before: (* toto *) ( function
   after: (* toto *) <NL> ( function
  insertion offset = 429
  before: ( (* toto *) function <NL> (* Insert every patterns here *)
   after: ( function <NL> (* toto *) <NL> (* Insert every patterns here *)
  insertion offset = 437
  before: function (* toto *) <NL>
   after: function <NL> (* toto *) <NL>
  insertion offset = 472
  before: (* Insert every patterns here *) (* toto *) <NL>
   after: (* Insert every patterns here *) <NL> (* toto *) <NL>
  insertion offset = 475
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 477
  before: <NL> | (* toto *) ( (
   after: <NL> (* toto *) <NL> | ( (
  insertion offset = 479
  before: ( ( (* toto *) ( x
   after: ( (* toto *) ( ( x
  insertion offset = 492
  before: _ | (* toto *) a , b | A , B x | ` A , ` B x | #
   after: _ <NL> (* toto *) <NL> | a , b <NL> | A , B x <NL> | ` A , ` B x <NL> | #
  insertion offset = 499
  before: _ | a , b | (* toto *) A , B x | ` A , ` B x | #
   after: _ <NL> | a , b <NL> (* toto *) <NL> | A , B x <NL> | ` A , ` B x <NL> | #
  insertion offset = 508
  before: _ | a , b | A , B x | (* toto *) ` A , ` B x | #
   after: _ <NL> | a , b <NL> | A , B x <NL> (* toto *) <NL> | ` A , ` B x <NL> | #
  insertion offset = 509
  before: ` (* toto *) A , `
   after: ` A (* toto *) , `
  insertion offset = 513
  before: ` (* toto *) B x |
   after: ` B (* toto *) x |
  insertion offset = 519
  before: _ | a , b | A , B x | ` A , ` B x | (* toto *) # t
   after: _ <NL> | a , b <NL> | A , B x <NL> | ` A , ` B x <NL> (* toto *) <NL> | # t
  insertion offset = 520
  before: _ | a , b | A , B x | ` A , ` B x | # (* toto *) t
   after: _ <NL> | a , b <NL> | A , B x <NL> | ` A , ` B x <NL> (* toto *) <NL> | # t
  insertion offset = 523
  before: t ) (* toto *) as x
   after: t (* toto *) ) as x
  insertion offset = 532
  before: (* toto *) | {
   after: (* toto *) <NL> | {
  insertion offset = 533
  before: <NL> | (* toto *) { a
   after: <NL> (* toto *) <NL> | { a
  insertion offset = 546
  before: _ ; (* toto *) _ }
   after: _ (* toto *) ; _ }
  insertion offset = 552
  before: (* toto *) | [|
   after: (* toto *) <NL> | [|
  insertion offset = 553
  before: <NL> | (* toto *) [| a
   after: <NL> (* toto *) <NL> | [| a
  insertion offset = 565
  before: (* toto *) | A
   after: (* toto *) <NL> | A
  insertion offset = 566
  before: <NL> | (* toto *) A |
   after: <NL> (* toto *) <NL> | A |
  insertion offset = 570
  before: A | (* toto *) B <NL>
   after: A (* toto *) | B <NL>
  insertion offset = 575
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 576
  before: <NL> | (* toto *) ( module
   after: <NL> (* toto *) <NL> | ( module
  insertion offset = 577
  before: ( (* toto *) module M )
   after: ( module (* toto *) M )
  insertion offset = 590
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 591
  before: <NL> | (* toto *) ( module
   after: <NL> (* toto *) <NL> | ( module
  insertion offset = 592
  before: ( (* toto *) module M :
   after: ( module (* toto *) M :
  insertion offset = 605
  before: M : S ) (* toto *) <NL>
   after: M (* toto *) : S ) <NL>
  insertion offset = 609
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 610
  before: <NL> | (* toto *) ( module
   after: <NL> (* toto *) <NL> | ( module
  insertion offset = 611
  before: ( (* toto *) module _ )
   after: ( module (* toto *) _ )
  insertion offset = 624
  before: (* toto *) | (
   after: (* toto *) <NL> | (
  insertion offset = 625
  before: <NL> | (* toto *) ( exception
   after: <NL> (* toto *) <NL> | ( exception
  insertion offset = 626
  before: ( (* toto *) exception E )
   after: ( exception (* toto *) E )
  insertion offset = 642
  before: (* toto *) | [%
   after: (* toto *) <NL> | [%
  insertion offset = 643
  before: <NL> | (* toto *) [% ppx
   after: <NL> (* toto *) <NL> | [% ppx
  insertion offset = 653
  before: (* toto *) | M
   after: (* toto *) <NL> | M
  insertion offset = 654
  before: <NL> | (* toto *) M .
   after: <NL> (* toto *) <NL> | M .
  insertion offset = 656
  before: . (* toto *) ( A |
   after: . ( (* toto *) A |
  insertion offset = 661
  before: A | (* toto *) B )
   after: A (* toto *) | B )
  insertion offset = 666
  before: -> (* toto *) <NL>
   after: -> <NL> (* toto *) <NL>
  insertion offset = 677
  before: ) (* toto *) ;
   after: ) <NL> (* toto *) ;
  insertion offset = 678
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 681
  before: (* toto *) ( fun
   after: (* toto *) <NL> ( fun
  insertion offset = 682
  before: ( (* toto *) fun ( type t )
   after: ( fun ( type (* toto *) t )
  insertion offset = 686
  before: fun (* toto *) ( type t )
   after: fun ( type (* toto *) t )
  insertion offset = 687
  before: ( (* toto *) type t )
   after: ( type (* toto *) t )
  insertion offset = 697
  before: X (* toto *) ~ a ~b:
   after: X ~ (* toto *) a ~b:
  insertion offset = 700
  before: a (* toto *) ~b: Y ?
   after: a ~b: (* toto *) Y ?
  insertion offset = 705
  before: Y (* toto *) ? c ?c:
   after: Y ? (* toto *) c ?c:
  insertion offset = 708
  before: c (* toto *) ?c: ( Z =
   after: c ?c: ( (* toto *) Z =
  insertion offset = 711
  before: ?c: (* toto *) ( Z =
   after: ?c: ( (* toto *) Z =
  insertion offset = 723
  before: a ; (* toto *) _ }
   after: a (* toto *) ; _ }
  insertion offset = 740
  before: X ~ (* toto *) a ~b:
   after: X (* toto *) ~ a ~b:
  insertion offset = 746
  before: a ~b: ( (* toto *) x y ) ? c
   after: a <NL> ~b: ( (* toto *) x y ) <NL> ? c
  insertion offset = 748
  before: a ~b: ( x (* toto *) y ) ? c
   after: a <NL> ~b: ( x (* toto *) y ) <NL> ? c
  insertion offset = 749
  before: a ~b: ( x y (* toto *) ) ? c
   after: a <NL> ~b: ( x y (* toto *) ) <NL> ? c
  insertion offset = 752
  before: ) ? (* toto *) c ?c:
   after: ) (* toto *) ? c ?c:
  insertion offset = 792
  before: <NL> ( (* toto *) try a
   after: <NL> (* toto *) ( try a
  insertion offset = 848
  before: c ) (* toto *) |] ;
   after: c (* toto *) ) |] ;
  insertion offset = 851
  before: |] (* toto *) ;
   after: |] <NL> (* toto *) ;
  insertion offset = 852
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 875
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 878
  before: (* toto *) for i
   after: (* toto *) <NL> for i
  insertion offset = 899
  before: do (* toto *) <NL>
   after: do <NL> (* toto *) <NL>
  insertion offset = 912
  before: i <NL> (* toto *) done ;
   after: i (* toto *) <NL> done ;
  insertion offset = 917
  before: done (* toto *) ;
   after: done <NL> (* toto *) ;
  insertion offset = 918
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 930
  before: m ( (* toto *) new M
   after: m (* toto *) ( new M
  insertion offset = 934
  before: m ( new (* toto *) M
   after: m (* toto *) ( new M
  insertion offset = 935
  before: M (* toto *) . c ) {< x
   after: M . c ) (* toto *) {< x
  insertion offset = 936
  before: . (* toto *) c ) {< x
   after: . c ) (* toto *) {< x
  insertion offset = 937
  before: c (* toto *) ) {< x
   after: c ) (* toto *) {< x
  insertion offset = 952
  before: ; (* toto *) <NL>
   after: ; <NL> (* toto *) <NL>
  insertion offset = 955
  before: (* toto *) let module
   after: (* toto *) <NL> let module
  insertion offset = 959
  before: let (* toto *) module M =
   after: let module (* toto *) M =
  insertion offset = 971
  before: ( (* toto *) val S .
   after: ( val (* toto *) S .
  insertion offset = 977
  before: . (* toto *) ( M .
   after: . ( (* toto *) M .
  insertion offset = 979
  before: M (* toto *) . N X .
   after: M . N (* toto *) X .
  insertion offset = 980
  before: . (* toto *) N X .
   after: . N (* toto *) X .
  insertion offset = 983
  before: X (* toto *) . Y ) .
   after: X . Y (* toto *) ) .
  insertion offset = 984
  before: . (* toto *) Y ) .
   after: . Y (* toto *) ) .
  insertion offset = 992
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 995
  before: (* toto *) let exception
   after: (* toto *) <NL> let exception
  insertion offset = 999
  before: let (* toto *) exception E of
   after: let exception (* toto *) E of
  insertion offset = 1018
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1021
  before: (* toto *) let open
   after: (* toto *) <NL> let open
  insertion offset = 1029
  before: open (* toto *) ! M in
   after: open ! (* toto *) M in
  insertion offset = 1035
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1038
  before: (* toto *) let* x
   after: (* toto *) <NL> let* x
  insertion offset = 1043
  test_comments: Cannot process "test.ml".
    Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
    BUG: comments dropped.
  insertion offset = 1049
  test_comments: Cannot process "test.ml".
    Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
    BUG: comments dropped.
  insertion offset = 1062
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1071
  before: x (* toto *) <NL>
   after: x <NL> <NL> (* toto *) <NL>
  insertion offset = 1072
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1073
  before: (* toto *) class virtual
   after: (* toto *) <NL> <NL> class virtual
  insertion offset = 1079
  before: class (* toto *) virtual c x
   after: class virtual (* toto *) c x
  insertion offset = 1091
  before: x (* toto *) ~ y ?z:
   after: x ~ (* toto *) y ?z:
  insertion offset = 1094
  before: y (* toto *) ?z: ( z' =
   after: y ?z: ( (* toto *) z' =
  insertion offset = 1097
  before: ?z: (* toto *) ( z' =
   after: ?z: ( (* toto *) z' =
  insertion offset = 1106
  before: ) (* toto *) = <NL> let
   after: ) = <NL> (* toto *) <NL> let
  insertion offset = 1107
  before: = (* toto *) <NL>
   after: = <NL> (* toto *) <NL>
  insertion offset = 1110
  before: (* toto *) let open
   after: (* toto *) <NL> let open
  insertion offset = 1114
  before: let (* toto *) open M in
   after: let open (* toto *) M in
  insertion offset = 1123
  before: in (* toto *) <NL>
   after: in <NL> (* toto *) <NL>
  insertion offset = 1126
  before: (* toto *) object (
   after: (* toto *) <NL> object (
  insertion offset = 1133
  before: object (* toto *) ( self )
   after: object ( (* toto *) self )
  insertion offset = 1138
  before: self (* toto *) ) <NL> inherit
   after: self ) <NL> (* toto *) <NL> inherit
  insertion offset = 1139
  before: ) (* toto *) <NL>
   after: ) <NL> (* toto *) <NL>
  insertion offset = 1144
  before: (* toto *) inherit M
   after: (* toto *) <NL> inherit M
  insertion offset = 1153
  before: M (* toto *) . c x <NL>
   after: M . c (* toto *) x <NL>
  insertion offset = 1154
  before: . (* toto *) c x <NL>
   after: . c (* toto *) x <NL>
  insertion offset = 1157
  before: x (* toto *) <NL> <NL> val
   after: x <NL> <NL> (* toto *) <NL> val
  insertion offset = 1158
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1163
  before: (* toto *) val mutable
   after: (* toto *) <NL> val mutable
  insertion offset = 1167
  before: val (* toto *) mutable y =
   after: val mutable (* toto *) y =
  insertion offset = 1180
  before: 0 (* toto *) <NL> <NL> initializer
   after: 0 <NL> <NL> (* toto *) <NL> initializer
  insertion offset = 1181
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1186
  before: (* toto *) initializer y
   after: (* toto *) <NL> initializer y
  insertion offset = 1204
  before: x (* toto *) <NL> <NL> method
   after: x <NL> <NL> (* toto *) <NL> method
  insertion offset = 1205
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1210
  before: (* toto *) method m
   after: (* toto *) <NL> method m
  insertion offset = 1228
  before: y (* toto *) <NL> <NL> method
   after: y <NL> <NL> (* toto *) <NL> method
  insertion offset = 1229
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1234
  before: (* toto *) method n
   after: (* toto *) <NL> method n
  insertion offset = 1251
  before: m (* toto *) <NL> <NL> method
   after: m <NL> <NL> (* toto *) <NL> method
  insertion offset = 1252
  before: <NL> (* toto *) <NL>
   after: <NL> <NL> (* toto *) <NL>
  insertion offset = 1257
  before: (* toto *) method virtual
   after: (* toto *) <NL> method virtual
  insertion offset = 1264
  before: method (* toto *) virtual o :
   after: method virtual (* toto *) o :
  insertion offset = 1277
  before: : # (* toto *) ct ->
   after: : (* toto *) # ct ->
  insertion offset = 1286
  before: int (* toto *) <NL>
   after: int <NL> (* toto *) <NL>
  insertion offset = 1289
  before: (* toto *) end <NL>
   after: (* toto *) <NL> end <NL>
  insertion offset = 1292
  before: end (* toto *) <NL>
   after: end <NL> <NL> (* toto *) <NL>
  insertion offset = 1293
  before: <NL> (* toto *) 
   after: <NL> <NL> (* toto *) <NL> 
