let _ =
  [
    a;
    b (* foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo *)
  ]

let
  [ fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* before end of the list *) ] =
  [ fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* after all elements *)
  (* after all elements as well *) ]

let
  [| fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* before end of the array *) |] =
  [| fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* after all elements *)
  (* after all elements as well *) |]

let { fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    _
    (* xxx *) } =
  { fooooooooooooooooooooooooooooooo= x;
    fooooooooooooooooooooooooooooooo= y;
    fooooooooooooooooooooooooooooooo= z;
  (* after all fields *) }

let length =
  [| 0; 269999999999999999999999999999999999999999999999999; 26
   ; (* foo *) 27 (* foo *); 27; 27 |]
  [@foo]

let length =
  [ 0; 14; (* foo *) 14; 17 (* foo *); 17; 2777777777777777777777777777777777
  ; 27 ]
  [@foo]

let length =
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13
   ; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26
   ; 269999999999999999999999999999999999999999999999999; 26; 26; 26; 26; 26
   ; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27
   ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27
   ; (* foo *) 27 (* foo *); 27; 27; 27; 27; 27; 27; 27; 27; 27; 28 |]
  [@foo]

let length =
  [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 12; 12; 13; 13
  ; 13; 13; 14; 14; 14; (* foo *) 14; 15; 15; 15; 15; 16; 16; 16; 16; 16; 16
  ; 16; 16; 17; 17; 17; 17 (* foo *); 17; 17; 17; 17; 18; 18; 18; 18; 18; 18
  ; 18; 18; 19; 19; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20
  ; 20; 20; 20; 26; 26; 26; 26; 26; 27; 27; 27; 27
  ; 2777777777777777777777777777777777; 27; 27; 27; 27; 27; 27; 27; 27; 27
  ; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 28 ]
  [@foo]
