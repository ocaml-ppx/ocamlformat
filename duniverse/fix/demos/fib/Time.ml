open Unix

let chrono (task : unit -> unit) : float =
  let times1 = times() in
  task();
  let times2 = times() in
  times2.tms_utime -. times1.tms_utime
