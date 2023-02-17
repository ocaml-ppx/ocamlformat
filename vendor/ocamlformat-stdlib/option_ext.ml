include Base.Option

module O = struct
  let ( let* ) t f = ( bind t ~f )

  let ( let+ ) t f = ( map t ~f )
end