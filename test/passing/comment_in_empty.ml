module M = struct (* this module is empty *) end

module type M = sig (* this module type is empty *) end

class type m = object (* this class type is empty *) end

let x = object (* this object is empty *) end

let _ = [ (* this list is empty *) ]

let _ = (* this list is empty2 *) []

let _ = (* this list is empty2 *) []

let _ = [| (* this array is empty *) |]

let _ = f ( (* comment in unit *) )

let _ = f "asd" (* te""st *) 3

let x = function
  | [ (* empty list pat *) ]
   |[| (* empty array pat *) |]
   |( (* unit pat *) ) | "" (* comment *) ->
      ()

let x =
  object
    method x () = {< (* this override is empty *) >}
  end

type t = private [> (*this variant is empty *) ]

type t = < (* this object type is empty *) >

type t = < .. (* this object type is empty *) >

let x =
  ( (* Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non
       risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec,
       ultricies sed, dolor. *) )

let x =
  [ (* Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non
       risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec,
       ultricies sed, dolor. *) ]
