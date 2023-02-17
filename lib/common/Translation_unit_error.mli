type t =
  | Invalid_source of {exn: exn; input_name: string}
  | Unstable of
      {iteration: int; prev: string; next: string; input_name: string}
  | Ocamlformat_bug of {exn: exn; input_name: string}
  | User_error of string

val user_error : string -> t

val equal : t -> t -> bool
