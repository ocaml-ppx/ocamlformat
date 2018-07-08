let () =
  foo
    ( for i = 1 to 10 do
        ()
      done )

let () =
  foo
    ( while true do
        ()
      done )

let _ =
  for i = some expr to 1000 do
    test this
  done

let _ =
  for
    something_big = some big expression
    to something biggggggggggggggggggggggggggggggg
  do
    test this
  done

let _ =
  for
    something_big =
      some big expressionnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
    to something biggggggggggggggggggggggggggggggg
         alsooooooooooooooooooooooooooooooooooooooooooo
  do
    test this
  done

let _ =
  for
    something_big =
      some big expressionnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
    downto something biggggggggggggggggggggggggggggggg
             alsooooooooooooooooooooooooooooooooooooooooooo
  do
    test this
  done

let _ =
  while
    some bigggggggggggggggggggggg
      expressionnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
  do
    test this
  done
