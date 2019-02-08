(* test *)

let x = 3

type schoolPerson = Teacher | Director | Student of string

let greeting person =
  match person with
  | Teacher -> ( ("Hey Professor!" [@reason.raw_literal "Hey Professor!"]) )
  | Director -> (
      ("Hello Director." [@reason.raw_literal "Hello Director."]) )
  | ((Student ("Richard"[@reason.raw_literal "Richard"]))[@explicit_arity])
    -> (
      ("Still here Ricky?" [@reason.raw_literal "Still here Ricky?"]) )
  | ((Student anyOtherName)[@explicit_arity]) ->
      ("Hey, " [@reason.raw_literal "Hey, "]) ^ anyOtherName
      ^ ("." [@reason.raw_literal "."])
