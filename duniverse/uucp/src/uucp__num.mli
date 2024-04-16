(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Numeric properties. *)

(** {1:hexprop Hex digits} *)

val is_ascii_hex_digit : Uchar.t -> bool
(** [is_ascii_hex_digit u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#ASCII_Hex_Digit}ASCII_Hex_Digit}
    property. *)

val is_hex_digit : Uchar.t -> bool
(** [is_hex_digit u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Hex_Digit}Hex_Digit}
    property. *)

(** {1:numtypeprop Numeric type} *)

type numeric_type = [ `De | `Di | `None | `Nu ]
(** The type for numeric types. *)

val pp_numeric_type : Format.formatter -> numeric_type -> unit
(** [pp_numeric_type ppf n] prints an unspecified representation of
    [n] on [ppf]. *)

val numeric_type : Uchar.t -> numeric_type
(** [numeric_type u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Numeric_Type}
    Numeric_Type} property. *)

(** {1:numvalueprop Numeric value} *)

type numeric_value =
  [ `NaN | `Nums of [`Frac of int * int | `Num of int64 ] list ]
(** The type for numeric values. *)

val pp_numeric_value : Format.formatter -> numeric_value -> unit
(** [pp_numeric_value ppf n] prints an unspecified representation of
      [n] on [ppf]. *)

val numeric_value : Uchar.t -> numeric_value
(** [numeric_type u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Numeric_Value}
    Numeric_Value} property. *)
