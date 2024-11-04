module List = struct
  include List

  let rec find_map f = function
    | [] -> None
    | x :: l ->
        begin match f x with
        | Some _ as result -> result
        | None -> find_map f l
        end
end

module Int = struct
  include Int

  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
end

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b
end
