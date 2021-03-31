module Compact = struct
  [@@@ocamlformat "let-binding-spacing=compact"]

  (* doc *)
  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  (** doc *)
  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  let f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd

  and f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd

  let f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd
end

module Nl = struct
  [@@@ocamlformat "let-binding-spacing=sparse"]

  (* doc *)
  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  (** doc *)
  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  let f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd


  and f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd


  let f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd
end

module Double = struct
  [@@@ocamlformat "let-binding-spacing=double-semicolon"]

  (* doc *)
  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  (** doc *)
  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  let f x = dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd

  let f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd

  and f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd
  ;;

  let f x =
    dddd dddddddddd dddddddddd dddddddddddd ddddddddd dddddddddd
      dddddddddddddddddddd dddddddddddd
  ;;
end
