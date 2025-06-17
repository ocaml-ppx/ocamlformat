type t : immutable_data with (type : value mod portable) u

type t = (type : value mod portable)

type u =
  ( type :
    value
    mod portable
    with t
    with u
    with ( type :
           value
           mod contended many
           with ('a :
                value mod portable with (type : (bits8 & bits8) mod external_)
                )
                u )
         v )
  y

let f (x : (type : value mod portable) @ contended) :
    [`A of (type : value mod unyielding with 'a @@ contended) | `Z] =
  ()

type t = T : u t -> (type : value mod stateless)

let x :
    foo:(type : value mod contended stateless)
    * bar:( type :
            (bits8 & bits8 & bits8)
            mod stateless unyielding
            with 'a @@ portable ) =
  y

module type X =
  Y
    with type a =
      ( type :
        (b & c & d & e) mod adsf asd asd asdf fasd with asdf @@ falskdf fo )

let a (a : (type : a with (type : a) a with (type : a with a) a * (type : a)))
    : (type : (a & a & a) mod a with a) @ a =
  a ~a:(a :> (type : a))

(* Comment attachment *)

type u =
  ( type :
    (* comment *)
    value
    (* a *)
    mod (* b *) portable
    with (* c d *) t
    with u
    with ( type :
           (* f f *)
           value
           mod contended many
           with ('a :
                value mod portable with (type : (bits8 & bits8) mod external_)
                )
                u )
         v )
  y

(* Attributes *)

type u =
  (( type :
     value
     mod portable
     with t
     with (u[@foo])
     with (( type :
             (* f f *)
             value
             mod contended many
             with (('a :
                   value
                   mod portable
                   with (type : (bits8 & bits8) mod external_) )
                   u
                  [@bar] ) )
           v
          [@baz] ) )
   y
  [@fnord] )
