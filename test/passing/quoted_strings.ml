let foo = {%foo | foooooooooooooo |}

let foo =
  (* A *)
  ( {%foo | foooooooooooooo | (* B *) } (* C *)

let foo = {%foo| foooooooooooooo |}
  [@attr] )

let foo = (* A *) ({%foo | foooooooooooooo |} (* B *) [@attr]) (* C *)

let foo = (* A *) {%foo sep| foooooooooooooo |sep}

let foo = {%foo | foooooooooooooo |} [@@attr]

let foo =
  (* A *)
  ( {%foo | foooooooooooooo | (* B *) } (* C *) [@@attr] (* D *)

let foo = {%foo| foooooooooooooo |}
  [@attr] )
  [@@attr]

let foo = ({%foo | foooooooooooooo |} (* A *) [@attr]) (* B *) [@@attr]

let foo = (* A *) ({%foo | foooooooooooooo |} [@attr]) (* B *) [@@attr]

let foo = (* A *) {%foo sep| foooooooooooooo |sep} (* B *) [@@attr]
