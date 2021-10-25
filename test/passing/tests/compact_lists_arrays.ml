[@@@ocamlformat "break-collection-expressions=wrap"]

let length =
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13
   ; 13; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15; 16; 16; 16; 16; 16; 16; 16
   ; 16; 17; 17; 17; 17; 17; 17; 17; 17; 18; 18; 18; 18; 18; 18; 18; 18; 19
   ; 19; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20
   ; 20; 20; 20; 20; 20; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21
   ; 21; 21; 21; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22
   ; 22; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24
   ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; 28 |]

let length =
  [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13
  ; 13; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15; 16; 16; 16; 16; 16; 16; 16
  ; 16; 17; 17; 17; 17; 17; 17; 17; 17; 18; 18; 18; 18; 18; 18; 18; 18; 19
  ; 19; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20; 20
  ; 20; 20; 20; 20; 20; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21
  ; 21; 21; 21; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22
  ; 22; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 24
  ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24
  ; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25; 25; 25; 25
  ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25
  ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26
  ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26
  ; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
  ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
  ; 28 ]

(* this is an array *)
let length =
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26
   ; 269999999999999999999999999999999999999999999999999; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; (* foo *) 27 (* foo *); 27; 27; 27; 27; 27; 27; 27; 27; 27; 28 |]
  [@foo]

(* this is a list *)
let length =
  [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13
  ; 13; 13; 13; 14; 14; 14; (* foo *) 14; 15; 15; 15; 15; 16; 16; 16; 16; 16
  ; 16; 16; 16; 17; 17; 17; 17 (* foo *); 17; 17; 17; 17; 18; 18; 18; 18; 18
  ; 18; 18; 18; 19; 19; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20
  ; 20; 20; 20; 20; 26; 26; 26; 26; 26; 27; 27; 27; 27
  ; 2777777777777777777777777777777777; 27; 27; 27; 27; 27; 27; 27; 27; 27
  ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 28 ]
  [@foo]
