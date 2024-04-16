type ('a, 'b) t = Left of 'a | Right of 'b

include (
  struct
    let left v = Left v
    let right v = Right v
    let is_left = function Left _ -> true | Right _ -> false
    let is_right = function Left _ -> false | Right _ -> true
    let find_left = function Left v -> Some v | Right _ -> None
    let find_right = function Left _ -> None | Right v -> Some v
    let map_left f = function Left v -> Left (f v) | Right _ as e -> e
    let map_right f = function Left _ as e -> e | Right v -> Right (f v)

    let map ~left ~right = function
      | Left v -> Left (left v)
      | Right v -> Right (right v)

    let fold ~left ~right = function Left v -> left v | Right v -> right v
    let iter = fold
    let for_all = fold

    let equal ~left ~right e1 e2 =
      match (e1, e2) with
      | Left v1, Left v2 -> left v1 v2
      | Right v1, Right v2 -> right v1 v2
      | Left _, Right _ | Right _, Left _ -> false

    let compare ~left ~right e1 e2 =
      match (e1, e2) with
      | Left v1, Left v2 -> left v1 v2
      | Right v1, Right v2 -> right v1 v2
      | Left _, Right _ -> -1
      | Right _, Left _ -> 1
  end :
    Either_intf.S with type ('a, 'b) t := ('a, 'b) t )
