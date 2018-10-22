[@@@ocamlformat "break-before-func=false"]

;;
List.map (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (* toto *)
  (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
  aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (* toto *)
  (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

let myfunc : int -> int = fun x ->
  let y = x + 1 in
  y

let myfunc : int -> int = function
  | x ->
      let y = x + 1 in
      y
