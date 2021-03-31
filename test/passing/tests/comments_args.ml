[@@@ocamlformat "wrap-fun-args=true"]

let emit_wrapper_function =
  Hhas_function.make function_attributes name body
    (Hhas_pos.pos_to_span ast_fun.Ast.f_span)
    false (* is_async *)
    false (* is_generator *)
    false (* is_pair_generator *)
    hoisted true (* no_injection *)
    true (* inout_wrapper *)
    is_interceptable false
    (* is_memoize_impl *)
    Rx.NonRx false

[@@@ocamlformat "wrap-fun-args=false"]

let emit_wrapper_function =
  Hhas_function.make function_attributes name body
    (Hhas_pos.pos_to_span ast_fun.Ast.f_span)
    false (* is_async *)
    false (* is_generator *)
    false (* is_pair_generator *)
    hoisted true (* no_injection *)
    true (* inout_wrapper *)
    is_interceptable false
    (* is_memoize_impl *)
    Rx.NonRx false
