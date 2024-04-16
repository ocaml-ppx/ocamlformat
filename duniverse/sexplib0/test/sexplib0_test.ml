open! Base
open Expect_test_helpers_base
open Sexplib0

let () = sexp_style := Sexp_style.simple_pretty

module type S = sig
  type t [@@deriving equal, sexp]
end

let test (type a) (module M : S with type t = a) string =
  let sexp = Parsexp.Single.parse_string_exn string in
  let result = Or_error.try_with (fun () -> M.t_of_sexp sexp) in
  print_s [%sexp (result : M.t Or_error.t)]
;;

let%expect_test "simple record" =
  let module M = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving equal, sexp_of]

    let t_of_sexp =
      Sexp_conv_record.record_of_sexp
        ~caller:"M.t"
        ~fields:
          (Field
             { name = "x"
             ; kind = Required
             ; conv = int_of_sexp
             ; rest =
                 Field { name = "y"; kind = Required; conv = int_of_sexp; rest = Empty }
             })
        ~index_of_field:(function
          | "x" -> 0
          | "y" -> 1
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (x, (y, ())) -> { x; y })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((x 1) (y 2))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* reverse order *)
  test "((y 2) (x 1))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* duplicate fields *)
  test "((x 1) (x 2) (y 3) (y 4))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: duplicate fields: x y"
      (invalid_sexp ((x 1) (x 2) (y 3) (y 4))))) |}];
  (* extra fields *)
  test "((a 1) (b 2) (c 3))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: extra fields: a b c"
      (invalid_sexp ((a 1) (b 2) (c 3))))) |}];
  (* missing field *)
  test "((x 1))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: y"
      (invalid_sexp ((x 1))))) |}];
  (* other missing field *)
  test "((y 2))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: x"
      (invalid_sexp ((y 2))))) |}];
  (* multiple missing fields *)
  test "()";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: x y"
      (invalid_sexp ()))) |}];
  ()
;;

let%expect_test "record with extra fields" =
  let module M = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving equal, sexp_of]

    let t_of_sexp =
      Sexp_conv_record.record_of_sexp
        ~caller:"M.t"
        ~fields:
          (Field
             { name = "x"
             ; kind = Required
             ; conv = int_of_sexp
             ; rest =
                 Field { name = "y"; kind = Required; conv = int_of_sexp; rest = Empty }
             })
        ~index_of_field:(function
          | "x" -> 0
          | "y" -> 1
          | _ -> -1)
        ~allow_extra_fields:true
        ~create:(fun (x, (y, ())) -> { x; y })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((x 1) (y 2))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* reversed order *)
  test "((y 2) (x 1))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* extra field *)
  test "((x 1) (y 2) (z 3))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* missing field *)
  test "((x 1))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: y"
      (invalid_sexp ((x 1))))) |}];
  (* other missing field *)
  test "((y 2))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: x"
      (invalid_sexp ((y 2))))) |}];
  (* multiple missing fields *)
  test "()";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: x y"
      (invalid_sexp ()))) |}];
  ()
;;

let%expect_test "record with defaults" =
  let module M = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving equal, sexp_of]

    let t_of_sexp =
      Sexp_conv_record.record_of_sexp
        ~caller:"M.t"
        ~fields:
          (Field
             { name = "x"
             ; kind = Default (fun () -> 0)
             ; conv = int_of_sexp
             ; rest =
                 Field
                   { name = "y"
                   ; kind = Default (fun () -> 0)
                   ; conv = int_of_sexp
                   ; rest = Empty
                   }
             })
        ~index_of_field:(function
          | "x" -> 0
          | "y" -> 1
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (x, (y, ())) -> { x; y })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((x 1) (y 2))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* reverse order *)
  test "((y 2) (x 1))";
  [%expect {| (Ok ((x 1) (y 2))) |}];
  (* extra field *)
  test "((x 1) (y 2) (z 3))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: extra fields: z"
      (invalid_sexp ((x 1) (y 2) (z 3))))) |}];
  (* missing field *)
  test "((x 1))";
  [%expect {| (Ok ((x 1) (y 0))) |}];
  (* other missing field *)
  test "((y 2))";
  [%expect {| (Ok ((x 0) (y 2))) |}];
  (* multiple missing fields *)
  test "()";
  [%expect {| (Ok ((x 0) (y 0))) |}];
  ()
;;

let%expect_test "record with omit nil" =
  let module M = struct
    type t =
      { a : int option
      ; b : int list
      }
    [@@deriving equal, sexp_of]

    let t_of_sexp =
      Sexp_conv_record.record_of_sexp
        ~caller:"M.t"
        ~fields:
          (Field
             { name = "a"
             ; kind = Omit_nil
             ; conv = option_of_sexp int_of_sexp
             ; rest =
                 Field
                   { name = "b"
                   ; kind = Omit_nil
                   ; conv = list_of_sexp int_of_sexp
                   ; rest = Empty
                   }
             })
        ~index_of_field:(function
          | "a" -> 0
          | "b" -> 1
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (a, (b, ())) -> { a; b })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((a (1)) (b (2 3)))";
  [%expect {| (Ok ((a (1)) (b (2 3)))) |}];
  (* reverse order *)
  test "((b ()) (a ()))";
  [%expect {| (Ok ((a ()) (b ()))) |}];
  (* extra field *)
  test "((a (1)) (b (2 3)) (z ()))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: extra fields: z"
      (invalid_sexp ((a (1)) (b (2 3)) (z ()))))) |}];
  (* missing field *)
  test "((a (1)))";
  [%expect {| (Ok ((a (1)) (b ()))) |}];
  (* other missing field *)
  test "((b (2 3)))";
  [%expect {| (Ok ((a ()) (b (2 3)))) |}];
  (* multiple missing fields *)
  test "()";
  [%expect {| (Ok ((a ()) (b ()))) |}];
  ()
;;

let%expect_test "record with sexp types" =
  let module M = struct
    type t =
      { a : int option
      ; b : int list
      ; c : int array
      ; d : bool
      }
    [@@deriving equal, sexp_of]

    let t_of_sexp =
      Sexp_conv_record.record_of_sexp
        ~caller:"M.t"
        ~fields:
          (Field
             { name = "a"
             ; kind = Sexp_option
             ; conv = int_of_sexp
             ; rest =
                 Field
                   { name = "b"
                   ; kind = Sexp_list
                   ; conv = int_of_sexp
                   ; rest =
                       Field
                         { name = "c"
                         ; kind = Sexp_array
                         ; conv = int_of_sexp
                         ; rest =
                             Field
                               { name = "d"; kind = Sexp_bool; conv = (); rest = Empty }
                         }
                   }
             })
        ~index_of_field:(function
          | "a" -> 0
          | "b" -> 1
          | "c" -> 2
          | "d" -> 3
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (a, (b, (c, (d, ())))) -> { a; b; c; d })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((a 1) (b (2 3)) (c (4 5)) (d))";
  [%expect {| (Ok ((a (1)) (b (2 3)) (c (4 5)) (d true))) |}];
  (* reverse order *)
  test "((d) (c ()) (b ()) (a 1))";
  [%expect {| (Ok ((a (1)) (b ()) (c ()) (d true))) |}];
  (* missing field d *)
  test "((a 1) (b (2 3)) (c (4 5)))";
  [%expect {| (Ok ((a (1)) (b (2 3)) (c (4 5)) (d false))) |}];
  (* missing field c *)
  test "((a 1) (b (2 3)) (d))";
  [%expect {| (Ok ((a (1)) (b (2 3)) (c ()) (d true))) |}];
  (* missing field b *)
  test "((a 1) (c (2 3)) (d))";
  [%expect {| (Ok ((a (1)) (b ()) (c (2 3)) (d true))) |}];
  (* missing field a *)
  test "((b (1 2)) (c (3 4)) (d))";
  [%expect {| (Ok ((a ()) (b (1 2)) (c (3 4)) (d true))) |}];
  (* extra field *)
  test "((a 1) (b (2 3)) (c (4 5)) (d) (e (6 7)))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: extra fields: e"
      (invalid_sexp ((a 1) (b (2 3)) (c (4 5)) (d) (e (6 7)))))) |}];
  (* all fields missing *)
  test "()";
  [%expect {| (Ok ((a ()) (b ()) (c ()) (d false))) |}];
  ()
;;

let%expect_test "record with polymorphic fields" =
  let module M = struct
    type t =
      { a : 'a. 'a list
      ; b : 'a 'b. ('a, 'b) Result.t option
      }
    [@@deriving sexp_of]

    let equal = Poly.equal

    let t_of_sexp =
      let open struct
        type a = { a : 'a. 'a list } [@@unboxed]
        type b = { b : 'a 'b. ('a, 'b) Result.t option } [@@unboxed]
      end in
      let caller = "M.t" in
      Sexp_conv_record.record_of_sexp
        ~caller
        ~fields:
          (Field
             { name = "a"
             ; kind = Required
             ; conv =
                 (fun sexp ->
                    { a =
                        list_of_sexp
                          (Sexplib.Conv_error.record_poly_field_value caller)
                          sexp
                    })
             ; rest =
                 Field
                   { name = "b"
                   ; kind = Required
                   ; conv =
                       (fun sexp ->
                          { b =
                              Option.t_of_sexp
                                (Result.t_of_sexp
                                   (Sexplib.Conv_error.record_poly_field_value caller)
                                   (Sexplib.Conv_error.record_poly_field_value caller))
                                sexp
                          })
                   ; rest = Empty
                   }
             })
        ~index_of_field:(function
          | "a" -> 0
          | "b" -> 1
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun ({ a }, ({ b }, ())) -> { a; b })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((a ()) (b ()))";
  [%expect {| (Ok ((a ()) (b ()))) |}];
  (* reverse order *)
  test "((b ()) (a ()))";
  [%expect {| (Ok ((a ()) (b ()))) |}];
  (* attempt to deserialize paramter to [a] *)
  test "((a (_)) (b ()))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: cannot convert values of types resulting from polymorphic record fields"
      (invalid_sexp _))) |}];
  (* attempt to deserialize first parameter to [b] *)
  test "((a ()) (b ((Ok _))))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: cannot convert values of types resulting from polymorphic record fields"
      (invalid_sexp _))) |}];
  (* attempt to deserialize second parameter to [b] *)
  test "((a ()) (b ((Error _))))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: cannot convert values of types resulting from polymorphic record fields"
      (invalid_sexp _))) |}];
  (* multiple missing fields *)
  test "()";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: the following record elements were undefined: a b"
      (invalid_sexp ()))) |}];
  ()
;;
