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

  (* This is the important bit *)
  important_function x ;
  another_important_function x y ;

  cleanup x y

let foo x y =
  do_some_setup x ;
  do_some_setup y ;
  (* This is the important bit *)
  important_function x ;
  another_important_function x y ;
  cleanup x y

let foo x y =
  do_some_setup x ;
  do_some_setup y ;
  (* This is the important bit *)

  important_function x ;
  another_important_function x y ;
  cleanup x y

[@@@ocamlformat "indicate-multiline-delimiters=closing-on-separate-line"]

let foo x y =
  lazy
    ( fooooooooooooooooooooooo ;
      fooooooooooooooooooooooo ;
      foooooooooooooooooooooooooo ;
      fooooooooooooooooooooooooo
    )
