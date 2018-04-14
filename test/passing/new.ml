let x = new Objects.one ~hello:true ()

let _ = sprintf "Date: %s" (Js.to_string new%js Js.date_now ## toString)
