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

module Uchar = struct
  include Uchar

  let rep_ = 0xFFFD
  let rep = unsafe_of_int rep_
  let valid_bit = 27
  let decode_bits = 24
  let[@inline] utf_decode_is_valid d = (d lsr valid_bit) = 1
  let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111
  let[@inline] utf_decode_uchar d = unsafe_of_int (d land 0xFFFFFF)
  let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor (to_int u)
  let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor rep_
end

module Bytes = struct
  include Bytes

  external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
  let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10
  let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101
  let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100
  let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b
  let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8
  let dec_invalid = Uchar.utf_decode_invalid
  let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)

  let[@inline] utf_8_uchar_2 b0 b1 =
    ((b0 land 0x1F) lsl 6) lor
    ((b1 land 0x3F))

  let[@inline] utf_8_uchar_3 b0 b1 b2 =
    ((b0 land 0x0F) lsl 12) lor
    ((b1 land 0x3F) lsl 6) lor
    ((b2 land 0x3F))

  let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18) lor
    ((b1 land 0x3F) lsl 12) lor
    ((b2 land 0x3F) lsl 6) lor
    ((b3 land 0x3F))

  let get_utf_8_uchar b i =
    let b0 = get_uint8 b i in (* raises if [i] is not a valid index. *)
    let get = unsafe_get_uint8 in
    let max = length b - 1 in
    match Char.unsafe_chr b0 with (* See The Unicode Standard, Table 3.7 *)
    | '\x00' .. '\x7F' -> dec_ret 1 b0
    | '\xC2' .. '\xDF' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
        dec_ret 2 (utf_8_uchar_2 b0 b1)
    | '\xE0' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xED' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_x9F b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xF0' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x90_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        let i = i + 1 in if i > max then dec_invalid 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF1' .. '\xF3' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        let i = i + 1 in if i > max then dec_invalid 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF4' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_x8F b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        let i = i + 1 in if i > max then dec_invalid 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | _ -> dec_invalid 1

  let for_all p s =
    let n = length s in
    let rec loop i =
      if i = n then true
      else if p (unsafe_get s i) then loop (succ i)
      else false in
    loop 0
end

module String = struct
  include String
  module B = Bytes

  let bos = B.unsafe_of_string
  let get_utf_8_uchar s i = B.get_utf_8_uchar (bos s) i

  let for_all f s =
    B.for_all f (bos s)
end
