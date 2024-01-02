module With_length : sig
  type 'a t = private
    { tree : 'a
             [@global]
    (* a *)
    ; length : int [@global]
    }
end = struct end

val find_last : 'a t -> f:(('a -> bool)(* a *)(* b *)[@local](* c *)) -> 'a option
let find_last : 'a t -> f:(('a -> bool)(* a *)[@local](* b *)) -> 'a option = assert false
type t = (string[@local]) -> (string(* a *)[@local](* b *))


type global_long_attrs =
  | Foo of { s : string(* a *)(* b *)[@ocaml.global](* c *)(* d *); b: int }
  | Bar of (string(* e *)(* f *)[@ocaml.global](* g *)(* h *))

let local_long_ext = (* a *)(* b *)[%ocaml.local](* c *)(* d *) ()

let () =
  let g = (* a *)[%local](* b *) (fun a b c -> 1) in
  ()

let f (x(* a *)[@local](* b *)) = x
