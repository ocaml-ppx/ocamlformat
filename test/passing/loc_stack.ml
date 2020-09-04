let _ =
  ( (* a *) ( (* b *) 2 ))

let _ = (
  (* before match *)
  match (* after match *) x with
  | _ -> 1
)

let _ = (
  (* before try *)
  try (* after try *) x with
  | _ -> 1
)

let should_inline : Llvm.llvalue -> bool =
 fun llv ->
  match Llvm.use_begin llv with
  | Some use -> (
    match Llvm.use_succ use with
    | Some _ -> (
      (* If we are not in the default context, we can only use the OCAMLPATH
         variable if it is specific to this build context *)
      (* CR-someday diml: maybe we should actually clear OCAMLPATH in other
         build contexts *)
      match Llvm.classify_value llv with
      | Instruction
          ( Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc
          | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast ) ->
          true (* inline casts *)
      | _ -> false (* do not inline if >= 2 uses *) )
    | None -> true )
  | None -> true
