(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2005 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val include_dirs : string list ref
val debug : bool ref
val unsafe : bool ref
val open_modules : string list ref
val absname : bool ref
val use_threads : bool ref
val principal : bool ref
val recursive_types : bool ref
val applicative_functors : bool ref
val for_package : string option ref
val transparent_modules : bool ref
val locations : bool ref
val unsafe_string : bool ref
val color : Misc.Color.setting option ref
val error_style : Misc.Error_style.setting option ref
val unboxed_types : bool ref
