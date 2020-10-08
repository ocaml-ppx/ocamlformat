open Lwt.Infix

let opaque_identity x =
  #if OCAML_VERSION < (4, 03, 0)
    x
  #else
    Sys.opaque_identity x
  #endif

type 'a event = 'a React.event
type 'a signal = 'a React.signal

module S = struct
  include React.S

  let finalise f _ = f ()

  let with_finaliser f signal =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map
    #if OCAML_VERSION < (4, 03, 0)
      (fun x -> ignore r; x)
    #else
      (fun x -> ignore (Sys.opaque_identity r); x)
    #endif
      signal
end
