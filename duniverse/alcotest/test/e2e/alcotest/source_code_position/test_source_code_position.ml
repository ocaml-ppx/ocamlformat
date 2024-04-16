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

(* A module with functions to test *)
module To_test = struct
  let capitalise = Astring.String.Ascii.uppercase
  let double_all = List.map (fun a -> a + a)
end

let test_capitalise () =
  To_test.capitalise "b" |> Alcotest.(check string) "strings" "A";
  ()

let test_double_all () =
  To_test.double_all [ 1; 1; 2; 3 ]
  |> Alcotest.(check (list int)) "int lists 1" [ 1 ];
  To_test.double_all [ 1; 1; 2; 3 ]
  |> Alcotest.(check (list int)) "int lists 2" [ 2 ]

let suite1 =
  [
    ( "to_test",
      [
        ("capitalise", `Quick, test_capitalise);
        ("double all", `Slow, test_double_all);
      ] );
  ]

let suite2 =
  [
    ( "Ωèone",
      [
        ("Passing test 1", `Quick, fun () -> ());
        ( "Failing test",
          `Quick,
          fun () -> Alcotest.fail "This was never going to work..." );
        ("Passing test 2", `Quick, fun () -> ());
      ] );
  ]

(* Run both suites completely, even if the first contains failures *)
let () =
  try Alcotest.run ~and_exit:false "First suite" suite1
  with Alcotest.Test_error ->
    Printf.printf "Forging ahead regardless!\n%!";
    Alcotest.run ~and_exit:false "Second suite" suite2;
    Printf.printf "Finally done."
