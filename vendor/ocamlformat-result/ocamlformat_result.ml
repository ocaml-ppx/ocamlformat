open Result

module Let_syntax = struct
  let ( let+ ) = bind
end

module Global_scope = struct
  type 'a or_error = ('a, [`Msg of string]) t

  include Let_syntax
end
