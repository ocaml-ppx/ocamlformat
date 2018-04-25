let _ = f a (*comment*) ~b (*comment*) ~c:(*comment*) c' ?d ?e ()

let _ =
  let _ =
    f
      (*comment*)
      (let open M in
      let x = x in
      e)
  in
  ()
