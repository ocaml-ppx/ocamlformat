let () =
  let module X =
    Map.Make (struct
      type t = t

      let compare = compare
    end)
  in
  foo

let () =
  let module X =
    Map.Make (struct
      type t = t
    end) [@foo]
  in
  let module K = Foooooooooo in
  (* foooooo *)
  let module X =
    Map.Make (struct
      type t = t (* foooooooooo *)
    end) [@foo]
  in
  let module T = X [@foo] in
  let module X =
    Fooo (struct
      type t = t
    end)
  in
  foo

let () =
  let module X =
    Map.Make (struct
        type t = t
      end)
      (* foooooooooooooo *)
      (struct
        type t = t
        type t = t
        type t = t
        type t = t
      end)
      (struct
        type t = t
        type t = t
      end)
  in
  foo

let f () =
  (* before let module *)
  let module M = struct end in
  ()

let f () =
  let module (* before M *)
     M = struct end in
  ()

let f () =
  (* before let module *)
  let module
     (* before M *) M = struct end in
  ()

let f () =
  let module (* before M *) M = struct end in
  ()

let f () =
  (* before let module *)
  let module (* before M *) M = struct end in
  ()
