let f = function
  | A a -> f a
  | B a -> f a [@nontail]
  | C a -> f a [@nontail] ; f a [@nontail]
  | D a ->
      let x = f a [@nontail] in
      f x [@nontail]
  | E a -> (
      if f a [@nontail] then f a [@nontail] ;
      if f a [@nontail] then f a [@nontail] else f a [@nontail] ;
      try f a [@nontail] with e -> f a [@nontail] )
  | F a -> new c a [@nontail]
  | G a ->
      1 + (f a [@nontail]) ;
      1 ^ (f a [@nontail]) ;
      g (f a [@nontail]) ;
      1 :: (f a [@nontail])
  | H a ->
      let exception E of string in
      f a [@nontail]
  | I a ->
      let open M in
      f a [@nontail]
  | J a ->
      let module M = N in
      f a [@nontail]
  | K a ->
      let f x = g b [@nontail] in
      let f x (type a) = g b [@nontail] in
      42
  | L a -> (f a [@nontail] : t)
  | M a -> (f a [@nontail] :> t)

let f x = g b [@nontail]

let f x (type a) = g b [@nontail] ;;

f a [@nontail]
