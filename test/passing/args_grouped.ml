let nullsafe_optimistic_third_party_params_in_non_strict =
  CLOpt.mk_bool
    ~long:
      "nullsafe-optimistic-third-party-params-in-non-strict"
      (* Turned on for compatibility reasons. Historically this is because
         there was no actionable way to change third party annotations. Now
         that we have such a support, this behavior should be reconsidered,
         provided our tooling and error reporting is friendly enough to be
         smoothly used by developers. *)
    ~default:true
    "Nullsafe: in this mode we treat non annotated third party method params as if they were \
     annotated as nullable."

let test_file_renamings_from_json =
  let create_test test_input expected_output _ =
    let test_output input =
      DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.from_json input
    in
    foo
  in
  fooooooooooooooo

let eval location exp0 astate =
  let rec eval exp astate =
    match (exp : Exp.t) with
    | Var id ->
        Ok (eval_var (* error in case of missing history? *) [] (Var.of_id id) astate)
    | Lvar pvar ->
        Ok (eval_var [ValueHistory.VariableAccessed (pvar, location)] (Var.of_pvar pvar) astate)
    | Lfield (exp', field, _) ->
        goooooooo
  in
  fooooooooooooooooooooo

let declare_locals_and_ret tenv pdesc (prop_ : Prop.normal Prop.t) =
  let foooooooooooooo =
    BiabductionConfig.run_in_re_execution_mode
      (* no footprint vars for locals *)
      sigma_locals_and_ret ()
  in
  fooooooooooooooooooooooooooo

let bottom_up fooooooooooo =
  let empty = Int.equal 0 !scheduled && Queue.is_empty pending in
  if empty then (
    remaining := 0 ;
    L.progress "Finished call graph scheduling, %d procs remaining (in, or reaching, cycles).@."
      (CallGraph.n_procs syntactic_call_graph) ;
    if Config.debug_level_analysis > 0 then CallGraph.to_dotty syntactic_call_graph "cycles.dot" ;
    foooooooooooooooooo )
  else fooooooooooooooooo

let test_file_renamings_from_json =
  let fooooooooooooo =
    match expected_output with
    | Return exp ->
        assert_equal ~pp_diff
          ~cmp:DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.equal exp
          (test_output test_input)
    | Raise exc ->
        assert_raises exc (fun () -> test_output test_input)
  in
  foooooooooooooooo
