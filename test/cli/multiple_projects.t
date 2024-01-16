First project formatted with the 'janestreet' profile:

  $ mkdir project1
  $ echo profile=janestreet > project1/.ocamlformat
  $ echo 'let _machin ?aaaaaaaaaa:_ ?bbbbbbbbbbb:_ ?cccccccccccc:_ ?ddddddddddddd:_ ?eeeeeeeeeeee:_ () = ()' > project1/main.ml

Second project formatted with the 'ocamlformat' profile:

  $ mkdir project2
  $ echo profile=ocamlformat > project2/.ocamlformat
  $ echo 'let _machin ?aaaaaaaaaa:_ ?bbbbbbbbbbb:_ ?cccccccccccc:_ ?ddddddddddddd:_ ?eeeeeeeeeeee:_ () = ()' > project2/main.ml

  $ ocamlformat -i project1/main.ml project2/main.ml

  $ cat project1/main.ml
  let _machin
        ?aaaaaaaaaa:_
        ?bbbbbbbbbbb:_
        ?cccccccccccc:_
        ?ddddddddddddd:_
        ?eeeeeeeeeeee:_
        ()
    =
    ()
  ;;

  $ cat project2/main.ml
  let _machin ?aaaaaaaaaa:_ ?bbbbbbbbbbb:_ ?cccccccccccc:_ ?ddddddddddddd:_
      ?eeeeeeeeeeee:_ () =
    ()
