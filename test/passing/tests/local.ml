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

let[@ocaml.local] upstream_local_attr_long x = x
module type S = S -> S -> S

let[@ocaml.local never] upstream_local_attr_never_long x = x

let[@ocaml.local always] upstream_local_attr_always_long x = x

let[@ocaml.local maybe] upstream_local_attr_maybe_long x = x

let[@local] upstream_local_attr_short x = x

let[@local never] upstream_local_attr_never_short x = x

let[@local always] upstream_local_attr_always_short x = x

let[@local maybe] upstream_local_attr_maybe_short x = x

let f x = (* a *) (* b *) local_ (* c *) (* d *)
  let y = 1 in
  x + y

let f x = (* a *) (* b *) exclave_ (* c *) (* d *)
  let y = 1 in
  x + y

let x = (* a *) (* b *) local_ (* c *) (* d *)
  let y = 1 in
  y

let x = (* a *) (* b *) exclave_ (* c *) (* d *)
  let y = 1 in
  y

module type S = S -> S -> S
(* this is here to make sure we pass the AST equality checks even when the
   extended AST is different *)

let f ((* a *) (* b *)local_ (* c *) (* d *)a) ~foo:((* e *) (* f *)local_(* g *) (* h *) b) ?foo:(local_ c = 1) ~(local_ d) = ()
type 'a r = {mutable a: 'a; b: 'a; (* a *) (* b *)global_ (* c *) (* d *)c: 'a}

type 'a r =
  | Foo of (* a *) (* b *)global_(* c *) (* d *) 'a
  | Bar of 'a * (* e *) (* f *)global_ (* g *) (* h *)'a
  | Baz of global_ int * string * (* i *) (* j *) global_ (* k *) (* l *)'a

let () =
  let foo (local_ _ : int) = 10 in
  let bar (local_ Some x : _) = Some 10 in
  let baz (local_ Some x) = Some 10 in
  ()

type t : value

let _ = (local_ x) + y
let _ = (local_ x) |> f
let _ = local_ x + y
let _ = local_ x |> f

let _ = (exclave_ x) + y
let _ = (exclave_ x) |> f
let _ = exclave_ x + y
let _ = exclave_ x |> f

let _ = (* 1 *) M.x (* 2 *) l (* 3 *) ~f:(* 4 *)((* 5 *) local_ (* 6 *) fun x (* 7 *) ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxx (* 7 *) (module struct type nonrec t = M.t end))

let _ = List.iter l ~f:(local_ fun xxxxx ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))

let _ = (* 1 *) M.x (* 2 *) l (* 3 *) ~f:(* 4 *)((* 5 *) local_ (* 6 *) function x (* 7 *) ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxx (* 8 *) (module struct type nonrec t = M.t end))

let _ = List.iter l ~f:(local_ function xxxxx ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))

let _ = (* 1 *) M.x (* 2 *) l (* 3 *) ~f:(* 4 *)((* 5 *) local_ (* 6 *) function
  | _ -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxx (* 8 *) (module struct type nonrec t = M.t end)
  | _ -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxx (* 9 *) (module struct type nonrec t = M.t end))

let _ = List.iter l ~f:(local_ function
  | _ -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxx (module struct type nonrec t = M.t end)
  | _ -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxx (module struct type nonrec t = M.t end))


let _ =
  List.iter
    l
    (* 1 *)
    ~f:((* 2 *) local_ (* 3 *) fun (* 4 *) xxxxx -> (* 5 *)
      Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx
        (module struct
          type nonrec t = M.t
        end))
    ~g:x
;;

let _ =
  List.iter
    l
    ~f:(local_ fun xxxxx ->
      Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx
        (module struct
          type nonrec t = M.t
        end))
    ~g:x
;;

let _ =
  List.iter
    l
    (* 1 *)
    ~f:((* 2 *) local_ (* 3 *) function (* 4 *) xxxxx -> (* 5 *)
      Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx
        (module struct
          type nonrec t = M.t
        end))
    ~g:x
;;

let _ =
  List.iter
    l
    ~f:(local_ function xxxxx ->
      Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx
        (module struct
          type nonrec t = M.t
        end))
    ~g:x
;;

let _ =
  List.iter
    l
    (* 1 *)
    ~f:((* 2 *) local_ (* 3 *) function (* 4 *)
      | _ (* 5 *) -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx x
      | _ (* 6 *) -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx x)
    ~g:x
;;

let _ =
  List.iter
    l
    ~f:(local_ function
      | _ -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx x
      | _ -> Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx x)
    ~g:x
;;

(* [exclave_] *)
let _ =
  List.iter
    l
    ~f:(local_ function (* x *) xxxxx (* y *) -> (* z *) exclave_ (* w *)
      Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx
        (module struct
          type nonrec t = M.t
        end))
    ~g:x
;;

let _ = List.iter l ~f:(local_ fun (* x *) xxxxx (* y *) -> (* z *) exclave_ (* w *)
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))
;;

let _ =
  List.iter
    l
    ~f:(local_ function xxxxx -> exclave_
      Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx
        (module struct
          type nonrec t = M.t
        end))
    ~g:x
;;

let _ = List.iter l ~f:(local_ fun xxxxx -> exclave_
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))
;;

(* Two funs breaks comments *)
let _ =
  M.f
    ~x:(fun _ -> x)
    ~y:(* c *) (stack_ fun _ -> y)

(* No [local_] (for reference) *)
let _ = List.iter l ~f:(fun xxxxx ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))

let _ = List.iter l ~f:(fun xxxxx ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))
  ~g:x

let _ = List.iter l ~f:(function xxxxx ->
  Xxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxx (module struct type nonrec t = M.t end))
  ~g:x

(* Two arguments *)
let xxxxxxxxxxxxxxxxx xxxxxxx ~xxx =
  xxxxxxx
    (local_ fun () ->
       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
    (local_ fun () ->
       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
;;

(* Comment shouldn't split [local_ fun] *)
let x =
  xxxxxxx
    ~xxxxxxxxxxxxxxxxxxxxxxxxxxx:(fun _ -> x)
    ~xxxxxxxxxxxxxxxxxxxxxxxxxxx:
      (* xxxxxxxxxxxxxxxxxxxxxxxxxx *)
      (local_ fun _ _ -> xxxxxxxxxxxxxxxxxxxxxx)
;;
