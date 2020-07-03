let foo = {%foo | foooooooooooooo |}

let foo = (* A *) ({%foo | foooooooooooooo |} (* B *) [@attr]) (* C *)

let foo = (* A *) {%foo sep| foooooooooooooo |sep}

let foo = {%foo | foooooooooooooo |} [@@attr]

let foo =
  ( {%foo | foooooooooooooo | } (* A *) [@@attr] (* B *))

let foo = ({%foo| foooooooooooooo |}
  [@attr] )
  [@@attr]

let foo = ({%foo | foooooooooooooo |} (* A *) [@attr]) (* B *) [@@attr]

let foo = (* A *) ({%foo | foooooooooooooo |} [@attr]) (* B *) [@@attr]

let foo = (* A *) {%foo sep| foooooooooooooo |sep} (* B *) [@@attr]

{%%foo | foooooooooooooo |}

{%%foo | foooooooooooooo |} (* A *) [@@attr] (* B *)

{%%foo sep| foooooooooooooo |sep}

{%%foo sep| foooooooooooooo |sep} (* A *) [@@attr]

(* Structures *)
{%%M.foo | <hello>{x} |}

{%%M.foo bar| <hello>{|x|} |bar}

(* Signatures *)
module type S = sig
  {%%M.foo | <hello>{x} |}

  {%%M.foo bar| <hello>{|x|} |bar}
end

(* Expressions/Pattern/Types *)
let ({%M.foo | <hello>{x} |} : {%M.foo | <hello>{x} |}) =
  {%M.foo | <hello>{x} |}

let ({%M.foo bar| <hello>{|x|} |bar} : {%M.foo bar| <hello>{|x|} |bar}) =
  {%M.foo bar| <hello>{|x|} |bar}

(* Multiline *)
{%%M.foo |
 <hello>
   {x}
 </hello>
|}
