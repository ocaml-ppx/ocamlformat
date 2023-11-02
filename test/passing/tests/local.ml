let f a b c = 1

let f (local_ a) ~foo:(local_ b) ?foo:(local_ c = 1) ~(local_ d) = ()

let f ~(local_ x) ~(local_ y : string) ?(local_ z : string) = ()

let xs = [(fun (local_ a) (type b) ~(local_ c) -> local_ 1)]

let xs = [(fun (local_ a) (type b) ~(local_ c) -> exclave_ 1)]

let f () = local_
  let a = [local_ 1] in
  let local_ r = 1 in
  let local_ f : 'a. 'a -> 'a = fun x -> local_ x in
  let local_ g a b c : int = 1 in
  let () = g (local_ fun () -> ()) in
  local_ "asdfasdfasdfasdfasdfasdfasdf"

let f () = exclave_
  let a = [exclave_ 1] in
  let local_ r = 1 in
  let local_ f : 'a. 'a -> 'a = fun x -> exclave_ x in
  let local_ g a b c : int = 1 in
  let () = g (exclave_ (fun () -> ())) in
  exclave_ "asdfasdfasdfasdfasdfasdfasdf"

type 'a r = {mutable a: 'a; b: 'a; global_ c: 'a}

type 'a r =
  | Foo of global_ 'a
  | Bar of 'a * global_ 'a
  | Baz of global_ int * string * global_ 'a

type ('a, 'b) cfn =
  a:local_ 'a -> ?b:local_ b -> local_ 'a -> (int -> local_ 'b)

let _ = local_ ()

let _ = exclave_ ()

let () = local_ x

let () = exclave_ x

let {b} = local_ ()

let {b} = exclave_ ()

let () = local_ r

let () = exclave_ r

let local_ x : string = "hi"
let (x : string) = local_ "hi"

let (x : string) = exclave_ "hi"

let x = local_ ("hi" : string)

let x = exclave_ ("hi" : string)
let x : 'a . 'a -> 'a = local_ "hi"
let x : 'a . 'a -> 'a = exclave_ "hi"
let local_ f : 'a. 'a -> 'a = "hi"

let foo () =
  if true then (local_ ());
  ()

type loc_long_attrs = (string[@ocaml.local]) -> (string[@ocaml.local])

type loc_short_attrs = (string[@local]) -> (string[@local])

type global_long_attrs =
  | Foo of { s : string[@ocaml.global] }
  | Bar of (string[@ocaml.global])

type global_short_attrs =
  | Foo of { s : string[@global] }
  | Bar of (string[@global])

type global_short_attrs =
  | Foo of { s : string[@global] }
  | Bar of (string[@global])

let local_long_ext = [%ocaml.local] ()

let local_short_ext = [%local] ()

let exclave_long_ext = [%ocaml.exclave] ()

let exclave_short_ext = [%exclave] ()

let[@ocaml.local] upstream_local_attr_long x = x

let[@ocaml.local never] upstream_local_attr_never_long x = x

let[@ocaml.local always] upstream_local_attr_always_long x = x

let[@ocaml.local maybe] upstream_local_attr_maybe_long x = x

let[@local] upstream_local_attr_short x = x

let[@local never] upstream_local_attr_never_short x = x

let[@local always] upstream_local_attr_always_short x = x

let[@local maybe] upstream_local_attr_maybe_short x = x

let f x = (* a *) local_
  let y = 1 in
  x + y

let f x = (* a *) exclave_
  let y = 1 in
  x + y

let x = (* a *) local_
  let y = 1 in
  y

let x = (* a *) exclave_
  let y = 1 in
  y
