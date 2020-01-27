val check_use_file :
  name:string ->
  input:string ->
  expected:((int * int) * (int * int)) list ->
  unit

val check_impl :
  name:string ->
  input:string ->
  expected:((int * int) * (int * int)) list ->
  unit

val check_intf :
  name:string ->
  input:string ->
  expected:((int * int) * (int * int)) list ->
  unit
