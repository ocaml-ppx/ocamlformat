class t =
  object
    method meth aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
        ddddddddddddddddddddd eeeeeeeeeeeeeee =
      body
  end

let func aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc ddddddddddddddddddddd
    eeeeeeeeeeeeeee =
  body

let rec func aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
    ddddddddddddddddddddd eeeeeeeeeeeeeee =
  body

let ffffffffffffffffffff aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb = g

let ffffffffffffffffffff aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb
    cccccccccccccccccccccc =
  g

let ffffffffffffffffffff aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb
    cccccccccccccccccccccc dddddddddddddddddddddd =
  g

class ffffffffffffffffffff aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb = g

class ffffffffffffffffffff aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb
  cccccccccccccccccccccc = g

class ffffffffffffffffffff aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb
  cccccccccccccccccccccc dddddddddddddddddddddd = g

let ffffffffffffffffffff : aaaaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbb =
  g

let ffffffffffffffffffff :
       aaaaaaaaaaaaaaaaaaaaaa
    -> bbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc =
  g

let ffffffffffffffffffff :
       aaaaaaaaaaaaaaaaaaaaaa
    -> bbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc
    -> dddddddddddddddddddddd =
  g

let fffffffffffffffffffffffffffffffffff x yyyyyyyyyyyyyyyyyyyyyyyyyyy = ()

let fffffffffffffffffffffffffffffffffff x yyyyyyyyyyyyyyyyyyyyyyyyyyy
    yyyyyyyyyyyyyyyyyyyyyyyyyyy =
  ()

class ffffffffffffffffffff =
  object
    method ffffffffffffffffffff :
           aaaaaaaaaaaaaaaaaaaaaa
        -> bbbbbbbbbbbbbbbbbbbbbb
        -> cccccccccccccccccccccc
        -> dddddddddddddddddddddd =
      g

    val ffffffffffffffffffff
        :    aaaaaaaaaaaaaaaaaaaaaa
          -> bbbbbbbbbbbbbbbbbbbbbb
          -> cccccccccccccccccccccc
          -> dddddddddddddddddddddd =
      g
  end

class type ffffffffffffffffffff = object
  method ffffffffffffffffffff :
       aaaaaaaaaaaaaaaaaaaaaa
    -> bbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc
    -> dddddddddddddddddddddd

  val ffffffffffffffffffff :
       aaaaaaaaaaaaaaaaaaaaaa
    -> bbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc
    -> dddddddddddddddddddddd

  val ffffffffffffffffffff :
       (   aaaaaaaaaaaaaaaaaaaaaa
        -> bbbbbbbbbbbbbbbbbbbbbb
        -> cccccccccccccccccccccc
        -> dddddddddddddddddddddd )
    -> bbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc
    -> dddddddddddddddddddddd
end

let _ =
 fun (module Store : Irmin.Generic_key.S with type repo = repo)
     (module Store : Irmin.Generic_key.S with type repo = repo) ->
  body

let _ =
  f
    (fun
      (module Store : Irmin.Generic_key.S with type repo = repo)
      (module Store : Irmin.Generic_key.S with type repo = repo)
    -> body )

let f (module Store : Irmin.Generic_key.S with type repo = repo)
    (module Store : Irmin.Generic_key.S with type repo = repo) =
  body
