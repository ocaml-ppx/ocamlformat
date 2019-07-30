[@@@ocamlformat "break-sequence-separators=before"]

let foo x y =
  lazy
    ( fooooooooooooooooooooooo
    ; fooooooooooooooooooooooo
    ;%ext
      foooooooooooooooooooooooooo
    ; fooooooooooooooooooooooooo )

let _ =
  do_
  ;%ext
    job_1
  ; job_2
  ; job_1
  ; job_2
  ; job_1
  ;%ext
    job_2
  ; job_1
  ; job_2
  ; job_1
  ; job_2
  ; return ()

let _ =
  do_
  ; job_1
  ; job_2
  ;%ext
    f
      ( job_1
      ; job_2
      ; job_1
      ; job_2
      ; job_1
      ;%ext
        job_2
      ;%ext
        job_2
      ; job_1
      ; job_2
      ; job_1
      ; job_2 )
  ;%ext
    return ()
