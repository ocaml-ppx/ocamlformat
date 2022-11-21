let compare = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < ) [@attr]
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )

let raise fmt =
  Fmt.kstr
    (fun error_message (result : _ result) ->
      match result with
      | Ok v -> v
      | Error `Oh_no -> invalid_arg error_message)
    fmt

let raise fmt =
  Fmt.kstr
    (fun error_message aaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbb ->
      invalid_arg error_message)
    fmt

let raise fmt =
  Fmt.kstr
    (fun error_message (result : _ result) ->
      match result with
      | Ok v -> v
      | Error `Oh_no -> invalid_arg error_message)
    fmt

let raise fmt =
  Fmt.kstr
    (fun error_message aaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbb ->
      invalid_arg error_message)
    fmt

let contrived =
  List.map
    ~f:(fun aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ->
      f aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
    l
