(* test *)
let x = 3

type schoolPerson = Teacher | Director | Student of string

let greeting person =
  match person with
  | Teacher -> "Hey Professor!"
  | Director -> "Hello Director."
  | Student "Richard" -> "Still here Ricky?"
  | Student anyOtherName -> "Hey, " ^ anyOtherName ^ "."

let x = "\xff"

let x = '\xff'

let x = "\255"

let x = '\255'
