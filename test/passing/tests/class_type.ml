class c : x -> y -> z = object end

class c :
  (* fooooooooooooo foooooooooo *)xxxxxxxxxxxxxx ->
  (* fooooooooo foooooooooo *)yyyyyyyyyyyyyy ->
  (* fooooooooooooo fooooooooooo *)zzzzzzzzzzzzzzzzzz =
  object end

class c :
  (* fooooooooooooo foooooooooo *)xxxxxxxxxxxxxx (* fooooooooooo *) ->
  (* fooooooooo foooooooooo *)yyyyyyyyyyyyyy (* foooooooooooooo *) ->
  (* fooooooooooooo fooooooooooo *)zzzzzzzzzzzzzzzzzz (* fooooooooooooooooo *) =
  object end

class c : (a -> b) -> x = object end

class c : x -> (a -> b) -> y = object end
