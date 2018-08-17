[@@@ocamlformat "disable"]

let this_won't_be_formatted =
               1

[@@@ocamlformat "enable"]

let x = function
  | A       , B        -> 1
  | BBB     , _        -> 2
  | CCCcccc , CCCCCCCC -> 3
[@@ocamlformat "disable"]

let x = function A, B -> 1 | BBB, _ -> 2 | CCCcccc, CCCCCCCC -> 3

module S = struct
  let x = function
      | A       , B        -> 1
             | BBB     , _        -> 2
  | CCCcccc , CCCCCCCC -> (* cmt about 3 *) 3
                [@@ocamlformat "disable"]
end

module S = struct
  let x = function
    | A, B -> 1
    | BBB, _ -> 2
    | CCCcccc, CCCCCCCC -> (* cmt about 3 *) 3

  let x = function
    | A, B -> 1
    | BBB, _ -> 2
    | CCCcccc, CCCCCCCC -> (* cmt about 3 *) 3

  [@@@ocamlformat "disable"]

  let x = function
  | A       , B        -> 1
  | BBB     , _        -> 2
  | CCCcccc , CCCCCCCC -> (* cmt about 3 *) 3

  [@@@ocamlformat "enable"]

  let x = function
    | A, B -> 1
    | BBB, _ -> 2
    | CCCcccc, CCCCCCCC -> (* cmt about 3 *) 3

  let _ =
    let x = 3 in
    match[@ocamlformat "disable"] x,y with
    | Some _, None   -> test
    | None  , Some _ -> test
    | Some _, Some _ -> test
    | None  , None   -> test
end

let x = function
  | A, B -> 1
  | BBB, _ -> 2
  | CCCcccc, CCCCCCCC -> (* cmt about 3 *) 3

module type S = sig
  type t    = int * int
                      [@@ocamlformat "disable"]

  [@@@ocamlformat "disable"]

  val x :  a : t        -> b: t
               -> c : t        -> unit
end

let x = fun fc ->
  let x = 3 in
  match x,y with
  | Some _, None   -> test
  | None  , Some _ -> test
  | Some _, Some _ -> test
  | None  , None   -> test
  [@@ocamlformat "disable"]

let x =
 fun[@ocamlformat "disable"] fc ->
 let x = 3 in
 match x,y with
 | Some _, None   -> test
 | None  , Some _ -> test
 | Some _, Some _ -> test
 | None  , None   -> test

let _ = (x [@ocamlformat "disable"]  [@test? _ when e [@test 2]]) 3

let _ =
  let module X = struct
    let x = 4
  end in
  X.x

let _ =
  let module X =
    struct
            let x    =   4
    end   [@ocamlformat "disable"] in
  X.x

let _ =
  let module X = struct
    module S =
      struct
          let x    =   4
      end   [@ocamlformat "disable"]
  end in
  X.x
