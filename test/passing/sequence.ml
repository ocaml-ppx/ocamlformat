let foo x y =
  do_some_setup y ; do_some_setup y ;

  do_some_setup y ; do_some_setup y ;
  important_function x

let foo x y =
  do_some_setup y ;
  important_function x

let foo x y =
  do_some_setup y ;

  important_function x

let foo x y =
  do_some_setup x ;
  do_some_setup y ;

  (* Empty line before *)
  important_function x ;
  another_important_function x y ;

  cleanup x y

let foo x y =
  do_some_setup x ;
  do_some_setup y ;
  (* No empty line *)
  important_function x ;
  another_important_function x y ;
  cleanup x y

let foo x y =
  do_some_setup x ;
  do_some_setup y ;
  (* Empty line after *)

  important_function x ;
  another_important_function x y ;
  cleanup x y

let foo x y =
  do_some_setup x ;
  do_some_setup y ; (* Empty line after, this above *)

  important_function x ;
  another_important_function x y ;
  cleanup x y

let foo x y =
  do_some_setup x ;
  do_some_setup y ;

  (* Empty line before, this under *) important_function x ;
  another_important_function x y ;
  cleanup x y ;

  let fooooooooooooooooooooooo =
    fooooooooooooooooooooo fooooooooooooooooo foooooooooooooooooo
  in

  fooooooooooooooooooooooo ;

  let* foooooooooooooooooooo =
    fooooooooooooooooooooooooo foooooooooooooooooo foooooooooooooooo
  in

  let fooooooooo =
    fooooooooooooooooooo fooooooooooooooooooooooooooooo foooooooooooo
  in

  let* fooooooooooooooooo =
    fooooooooooooooooo fooooooooooooooooooooo fooooooooooooooooooo
  in

  fooooooooooooooooooooooooooooo

[@@@ocamlformat "indicate-multiline-delimiters=closing-on-separate-line"]

let foo x y =
  lazy
    ( fooooooooooooooooooooooo ;
      fooooooooooooooooooooooo ;
      foooooooooooooooooooooooooo ;
      fooooooooooooooooooooooooo
    )
