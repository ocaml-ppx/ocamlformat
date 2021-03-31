type 'a t = [] | ( :: ) of 'a * 'a t

let _ = ( :: ) 5
