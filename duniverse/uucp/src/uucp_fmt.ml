(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pf = Format.fprintf
let strf = Format.asprintf
let string = Format.pp_print_string
let string_X ppf s =
  Format.pp_open_vbox ppf 1; string ppf "\"";
  for i = 0 to String.length s - 1 do
    if i mod 16 = 0 && i > 0 then pf ppf "\\@\n";
    pf ppf "\\x%02x" (Char.code s.[i])
  done;
  string ppf "\""; Format.pp_close_box ppf ()

let string_XN ppf = function "" -> string ppf "snil" | x -> string_X ppf x
let bool = Format.pp_print_bool
let sp = Format.pp_print_space
let semi ppf () = string ppf ";"; sp ppf ()
let int = Format.pp_print_int
let iter i ?(sep = sp) pp ppf x =
  let fst = ref true in
  i (fun v -> (if !fst then fst := false else sep ppf ()); pp ppf v) x

let as_array i pp ppf = pf ppf "@[<2>[|%a|]@]" (iter i ~sep:semi pp)
let array pp = as_array Array.iter pp
let array_N pp ppf = function [||] -> string ppf "nil" | x -> array pp ppf x

module R = struct
  type _ record =
  | [] : unit record
  | (::) :
      (string * (Format.formatter -> 'a -> unit)) * 'b record ->
      ('a -> 'b) record
end

let record record ppf =
  let field name pp_v ppf v = pf ppf "@[<1>%s =@ %a@]" name pp_v v in
  let open R in (* 4.03 compat *)
  let rec go : type a. (unit -> unit) -> a R.record -> a = fun k -> function
  | [] -> pf ppf "@[<2>{ %a }@]" (fun _ -> k) ()
  | [name, pp_v] ->
      fun v -> go (fun () -> k (); field name pp_v ppf v) []
  | (name, pp_v) :: record ->
      fun v -> go (fun () -> k (); field name pp_v ppf v; semi ppf ()) record
  in
  go ignore record
