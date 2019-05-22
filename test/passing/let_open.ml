let _ =
  (* a *) let (* b *) open (* c *) struct type t end (* d *) in (* e *)
  (* f *) let (* g *) open (* h *) A (* i *) (B) (* j *) in (* k *)
  ()

(* l *) open (* m *) struct
  type t
end (* n *)
