(** Thanks @craigfe, https://github.com/CraigFe/diff *)

type index = int

type 'a command =
  | Insert of { expected : index; actual : index }
      (** Insert the element [actual.(actual)] at [expected.(expected)]. *)
  | Delete of { expected : index }
      (** Delete the element at [expected.(expected)]. *)
  | Substitute of { expected : index; actual : index }
      (** Set [expected.(expected)) := actual.(actual)]. *)

let pp_command ppf = function
  | Insert { expected; actual } ->
      Format.fprintf ppf "Insert { expected = %d; actual = %d }" expected actual
  | Delete { expected } ->
      Format.fprintf ppf "Delete { expected = %d }" expected
  | Substitute { expected; actual } ->
      Format.fprintf ppf "Substitute { expected = %d; actual = %d }" expected
        actual

let map_expected f = function
  | Insert i -> Insert { i with expected = f i.expected }
  | Delete d -> Delete { expected = f d.expected }
  | Substitute s -> Substitute { s with expected = f s.expected }

let map_actual f = function
  | Insert i -> Insert { i with actual = f i.actual }
  | Substitute s -> Substitute { s with actual = f s.actual }
  | Delete _ as d -> d

let insert expected actual = Insert { expected; actual }
let delete expected = Delete { expected }
let substitute expected actual = Substitute { expected; actual }

type ('a, _) typ =
  | Array : ('a, 'a array) typ
  | List : ('a, 'a list) typ
  | String : (char, string) typ

module Subarray : sig
  type 'a t
  (** Read-only wrapper around an array or a string. Can be {!truncate}d in
      [O(1)] time. *)

  val truncate : origin:int -> length:int -> 'a t -> 'a t
  (** Return a new subarray with smaller bounds than the previous one. *)

  val empty : _ t
  val get : 'a t -> int -> 'a
  val length : 'a t -> int
  val of_container : ('a, 'container) typ -> 'container -> 'a t
end = struct
  type 'a t = { get : int -> 'a; origin : int; length : int }

  let truncate ~origin ~length
        {get; origin = prev_origin; length = prev_length} =
    if origin < prev_origin || length > prev_length then
      failwith
        (Format.sprintf
           "Cannot expand array during truncation ({ origin = %d; length = %d \
            } -> { origin = %d; length = %d })"
           prev_origin prev_length origin length ) ;
    {get; origin; length}

  let index_oob = Format.ksprintf invalid_arg "index out of bounds: %d"
  let empty = { get = index_oob; origin = 0; length = 0 }

  let get { get; origin; length } i =
    if i >= length then index_oob i;
    get (i + origin)

  let length { length; _ } = length
  let of_array a = { get = Array.get a; origin = 0; length = Array.length a }
  let of_list l = Array.of_list l |> of_array
  let of_string s = { get = String.get s; origin = 0; length = String.length s }

  let of_container (type a container) : (a, container) typ -> container -> a t =
    function
    | Array -> of_array
    | List -> of_list
    | String -> of_string
end

module Edit_script = struct
  type 'a t = 'a command list

  let insert n v t =
    let rec inner acc n t =
      match (n, t) with
      | 0, t -> List.rev_append acc (v :: t)
      | _, [] -> List.rev (v :: acc)
      | n, x :: xs -> inner (x :: acc) (n - 1) xs
    in
    inner [] n t

  let delete n t =
    let rec inner acc n t =
      match (n, t) with
      | 0, _ :: xs -> List.rev_append acc xs
      | n, x :: xs -> inner (x :: acc) (n - 1) xs
      | _ -> assert false
    in
    inner [] n t

  let substitute n v t =
    let rec inner acc n t =
      match (n, t) with
      | 0, _ :: xs -> List.rev acc @ (v :: xs)
      | n, x :: xs -> inner (x :: acc) (n - 1) xs
      | _ -> assert false
    in
    inner [] n t

  let apply (type a container) (typ : (a, container) typ)
      ~actual:(t_actual : int -> a) (script : a t) (initial : container) :
      a list =
    let initial : a list =
      match typ with
      | List -> initial
      | Array -> Array.to_list initial
      | String -> List.init (String.length initial) (fun i -> initial.[i])
    in
    List.fold_left
      (fun (i, acc) -> function
        | Insert { expected; actual } ->
            (i + 1, insert (expected + i) (t_actual actual) acc)
        | Delete { expected } -> (i - 1, delete (expected + i) acc)
        | Substitute { expected; actual } ->
            (i, substitute (expected + i) (t_actual actual) acc))
      (0, initial) script
    |> snd
end

let triple_minimum (a, b, c) =
  min (min a b) c

let triple_minimum_on f (a, b, c) =
  let ab = if f a > f b then b else a in
  if f ab > f c then c else ab

(** Standard dynamic programming algorithm for Levenshtein edit scripts. This
    works in two phases:

    1. construct a {i cost matrix} of edit distances for each _prefix_ of the
    two strings;

    2. reconstruct an edit script from the cost matrix.

    The standard algorithm uses a cost matrix of size [n * m]. If we only care
    about edit scripts up to some maximum length [b], the space and time
    complexity can be reduced to [O(max (n, m) * b)] (assuming an [O(1)]
    equality function). *)

(** Phase 1: compute the cost matrix, bottom-up. *)
let construct_grid (type a) ~(equal : a -> a -> bool) (a : a Subarray.t)
    (b : a Subarray.t) : int array array =
  let grid_x_length = Subarray.length a + 1
  and grid_y_length = Subarray.length b + 1 in
  let grid = Array.make_matrix grid_x_length grid_y_length 0 in
  let get_grid (x, y) = grid.(x).(y) in

  for i = 0 to grid_x_length - 1 do
    for j = 0 to grid_y_length - 1 do
      let cost =
        if min i j = 0 then max i j
        else if equal (Subarray.get a (i - 1)) (Subarray.get b (j - 1)) then
          get_grid (i - 1, j - 1)
        else
          triple_minimum
            (get_grid (i - 1, j), get_grid (i, j - 1), get_grid (i - 1, j - 1))
          + 1
      in
      grid.(i).(j) <- cost
    done
  done;
  grid

(** Phase 2: reconstruct the edit script from the cost matrix. *)
let reconstruct_edit_script a b grid =
  let get_grid (x, y) = grid.(x).(y) in

  (* Reverse-engineer the direction and action towards a given cell *)
  let backtrack (i, j) =
    let p_sub = (i - 1, j - 1) and p_ins = (i, j - 1) and p_del = (i - 1, j) in
    if Subarray.get a (i - 1) = Subarray.get b (j - 1) then (p_sub, [])
    else
      ( (`Substitute, get_grid p_sub + 1),
        (`Insert, get_grid p_ins),
        (`Delete, get_grid p_del) )
      |> triple_minimum_on snd
      |> function
      | `Substitute, _ -> (p_sub, [ substitute (fst p_sub) (snd p_sub) ])
      | `Insert, _ -> (p_ins, [ insert (fst p_ins) (snd p_ins) ])
      | `Delete, _ -> (p_del, [ delete (fst p_del) ])
  in

  let rec aux acc (x, y) =
    match (x, y) with
    | 0, 0 -> acc
    | i, 0 -> List.init i delete @ acc
    | 0, j -> List.init j (insert 0) @ acc
    | pos ->
        let next_coord, action = backtrack pos in
        (aux [@tailcall]) (action @ acc) next_coord
  in
  let x_len, y_len = Array.length grid, Array.length grid.(0) in
  aux [] (x_len - 1, y_len - 1)

let ( >> ) f g x = g (f x)

let levenshtein_dp ~equal (a_origin, b_origin) a b =
  let grid = construct_grid ~equal a b in
  reconstruct_edit_script a b grid
  (* Map the command indices to the true coordinates *)
  |> List.map (map_expected (( + ) a_origin) >> map_actual (( + ) b_origin))

(** Strip common prefixes and suffixes of the input sequences can be stripped
    (in linear time) to avoid running the full dynamic programming algorithm on
    them. *)
let strip_common_outer (type a) ~equal ((a : a Subarray.t), (b : a Subarray.t))
    =
  let len_a = Subarray.length a and len_b = Subarray.length b in

  (* Shift the lower indices upwards until they point to non-equal values in the
     arrays (or we scan an entire array). *)
  let rec raise_lower_bound (i, j) =
    match (i >= len_a, j >= len_b) with
    | true, true -> `Equal
    | false, false when equal (Subarray.get a i) (Subarray.get b j) ->
        raise_lower_bound (i + 1, j + 1)
    | a_oob, b_oob ->
        let i = if a_oob then None else Some i in
        let j = if b_oob then None else Some j in
        `Non_equal (i, j)
  in
  match raise_lower_bound (0, 0) with
  | `Equal -> `Equal (* The arrays are equal *)
  (* One array is a prefix of the other *)
  | `Non_equal (None, None) ->
      `Non_equal ((0, 0), (Subarray.empty, Subarray.empty))
  | `Non_equal (None, Some j) ->
      `Non_equal
        ( (j, j),
          (Subarray.empty, Subarray.truncate ~origin:j ~length:(len_b - j) b) )
  | `Non_equal (Some i, None) ->
      `Non_equal
        ( (i, i),
          (Subarray.truncate ~origin:i ~length:(len_a - i) a, Subarray.empty) )
  | `Non_equal (Some i_origin, Some j_origin) -> (
      let rec lower_upper_bound (i, j) =
        match (i < i_origin, j < j_origin) with
        | true, true -> `Equal
        | false, false when equal (Subarray.get a i) (Subarray.get b j) ->
            lower_upper_bound (i - 1, j - 1)
        | _ -> `Non_equal (i, j)
      in
      match lower_upper_bound (len_a - 1, len_b - 1) with
      | `Equal ->
          assert false (* We already decided that the arrays are non-equal *)
      | `Non_equal (i_last, j_last) ->
          `Non_equal
            ( (i_origin, j_origin),
              ( Subarray.truncate ~origin:i_origin
                  ~length:(i_last - i_origin + 1)
                  a,
                Subarray.truncate ~origin:j_origin
                  ~length:(j_last - j_origin + 1)
                  b ) ) )

let levenshtein_script (type a container) (typ : (a, container) typ)
    ~(equal : a -> a -> bool) (a : container) (b : container) : a Edit_script.t
    =
  let a, b = (Subarray.of_container typ a, Subarray.of_container typ b) in
  match strip_common_outer ~equal (a, b) with
  | `Equal -> []
  | `Non_equal (origin, (a, b)) -> levenshtein_dp ~equal origin a b
