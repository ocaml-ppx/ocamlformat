open Sexplib0.Sexp_conv

let bench_t_of_sexp ~t_of_sexp string =
  let sexp = Sys.opaque_identity (Parsexp.Single.parse_string_exn string) in
  fun () -> t_of_sexp sexp
;;

type t =
  { a : int
  ; b : int option
  ; c : bool
  ; d : int array
  ; e : int list
  ; f : int option
  ; g : int
  ; h : 'a. 'a list
  }

let t_of_sexp =
  let open struct
    type poly = { h : 'a. 'a list } [@@unboxed]
  end in
  Sexplib0.Sexp_conv_record.record_of_sexp
    ~caller:"Record.t"
    ~fields:
      (Field
         { name = "a"
         ; kind = Required
         ; conv = int_of_sexp
         ; rest =
             Field
               { name = "b"
               ; kind = Omit_nil
               ; conv = option_of_sexp int_of_sexp
               ; rest =
                   Field
                     { name = "c"
                     ; kind = Sexp_bool
                     ; conv = ()
                     ; rest =
                         Field
                           { name = "d"
                           ; kind = Sexp_array
                           ; conv = int_of_sexp
                           ; rest =
                               Field
                                 { name = "e"
                                 ; kind = Sexp_list
                                 ; conv = int_of_sexp
                                 ; rest =
                                     Field
                                       { name = "f"
                                       ; kind = Sexp_option
                                       ; conv = int_of_sexp
                                       ; rest =
                                           Field
                                             { name = "g"
                                             ; kind = Default (fun () -> 0)
                                             ; conv = int_of_sexp
                                             ; rest =
                                                 Field
                                                   { name = "h"
                                                   ; kind = Required
                                                   ; conv =
                                                       (fun sexp ->
                                                          { h =
                                                              list_of_sexp
                                                                (Sexplib0.Sexp_conv_error
                                                                 .record_poly_field_value
                                                                   "Record.t")
                                                                sexp
                                                          })
                                                   ; rest = Empty
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         })
    ~index_of_field:(function
      | "a" -> 0
      | "b" -> 1
      | "c" -> 2
      | "d" -> 3
      | "e" -> 4
      | "f" -> 5
      | "g" -> 6
      | "h" -> 7
      | _ -> -1)
    ~allow_extra_fields:false
    ~create:(fun (a, (b, (c, (d, (e, (f, (g, ({ h }, ())))))))) ->
      { a; b; c; d; e; f; g; h })
;;

let%bench_fun "t_of_sexp, full, in order" =
  bench_t_of_sexp ~t_of_sexp "((a 1) (b (2)) (c) (d (3 4)) (e (5 6)) (f 7) (g 8) (h ()))"
;;

let%bench_fun "t_of_sexp, full, reverse order" =
  bench_t_of_sexp ~t_of_sexp "((h ()) (g 8) (f 7) (e (5 6)) (d (3 4)) (c) (b (2)) (a 1))"
;;

let%bench_fun "t_of_sexp, empty" = bench_t_of_sexp ~t_of_sexp "((a 0) (h ()))"
