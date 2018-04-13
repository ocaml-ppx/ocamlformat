let x = new Objects.one ~hello:true ()

let _ = sprintf "Date: %s" (Js.to_string [%js new Js.date_now] ## toString)
