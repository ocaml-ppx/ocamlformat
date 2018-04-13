val run :
  unit -> (unit -> ('a, ([> `Msg of string] as 'b)) result)
  -> ('a, 'b) result
