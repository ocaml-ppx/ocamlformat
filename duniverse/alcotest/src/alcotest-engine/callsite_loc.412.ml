(*
 * Copyright (c) 2022 Antonin Décimo <antonin@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Best-effort reporting of the location of the call to Alcotest.check. *)

let check_caller caller entry =
  match Printexc.backtrace_slots_of_raw_entry entry with
  | Some [| slot |] -> (
      match Printexc.Slot.name slot with
      | Some name when caller = name -> true
      | _ -> false)
  | _ -> false

let get ?(__FUNCTION__ = "Alcotest_engine__Test.check") () =
  let caller = __FUNCTION__ in
  let open Printexc in
  let callstack_depth = 3 (* this function, caller, bound of test *) in
  let raw_backtrace = get_callstack callstack_depth in
  let entries = raw_backtrace_entries raw_backtrace in
  if
    Array.length entries >= callstack_depth
    && check_caller caller (Array.unsafe_get entries 1)
  then
    match backtrace_slots_of_raw_entry (Array.unsafe_get entries 2) with
    | Some [| slot |] -> (
        match Slot.name slot with
        | Some bound
          when Alcotest_stdlib_ext.String.(
                 is_prefix ~affix:"Alcotest_engine__Core." bound
                 || is_prefix ~affix:"Alcotest_lwt." bound
                 || is_prefix ~affix:"Alcotest_async." bound
                 || is_prefix ~affix:"Alcotest_mirage." bound) ->
            None
        | Some _ ->
            Option.map
              (fun { filename; line_number; start_char; end_char = _ } ->
                {
                  Lexing.pos_fname = filename;
                  pos_lnum = line_number;
                  pos_bol = 0;
                  pos_cnum = start_char;
                })
              (Slot.location slot)
        | None -> None)
    | _ -> None
  else None

let get ?__FUNCTION__ () =
  let guess =
    match Sys.getenv "ALCOTEST_SOURCE_CODE_POSITION" with
    | "" | "false" | "no" | "n" | "0" -> false
    | "true" | "yes" | "y" | "1" -> true
    | _ | (exception _) -> true
  in
  if guess then get ?__FUNCTION__ () else None
