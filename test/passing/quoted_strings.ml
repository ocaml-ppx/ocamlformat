let foo = {%foo | foooooooooooooo |}

let foo =
  ( {%foo | foooooooooooooo | (* B *) } (* C *))

let foo = ({%foo| foooooooooooooo |}
  [@attr] )

let foo = (* A *) ({%foo | foooooooooooooo |} (* B *) [@attr]) (* C *)

let foo = (* A *) {%foo sep| foooooooooooooo |sep}

let foo = {%foo | foooooooooooooo |} [@@attr]

let foo =
  ( {%foo | foooooooooooooo | (* B *) } (* C *) [@@attr] (* D *))

let foo = ({%foo| foooooooooooooo |}
  [@attr] )
  [@@attr]

let foo = ({%foo | foooooooooooooo |} (* A *) [@attr]) (* B *) [@@attr]

let foo = (* A *) ({%foo | foooooooooooooo |} [@attr]) (* B *) [@@attr]

let foo = (* A *) {%foo sep| foooooooooooooo |sep} (* B *) [@@attr]

{%%foo | foooooooooooooo |}

{%%foo | foooooooooooooo | (* A *) } (* B *)

;; {%%foo| foooooooooooooo |}
[@@attr]

{%%foo | foooooooooooooo |} (* A *) [@@attr]

{%%foo sep| foooooooooooooo |sep}

{%%foo sep| foooooooooooooo |sep} (* A *) [@@attr]
