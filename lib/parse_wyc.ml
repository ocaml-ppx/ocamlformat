module P = Parser
module I = P.MenhirInterpreter

module R =
  Merlin_recovery.Make
    (I)
    (struct
      include Parser_recover

      let default_value loc x =
        Default.default_loc := loc;
        default_value x

      let guide _ = false
    end)

type 'a positioned = 'a * Lexing.position * Lexing.position

module type PARSE_INTF = sig
  type 'a parser

  val initial :
    (Lexing.position -> 'a I.checkpoint) -> Lexing.position -> 'a parser

  type 'a step = Intermediate of 'a parser | Success of 'a | Error

  val step : 'a parser -> P.token positioned -> 'a step
end

module type RECOVER_INTF = sig
  include PARSE_INTF

  (* Interface for recovery *)
  val recover : 'a I.checkpoint -> 'a parser

  val recovery_env : 'a parser -> 'a I.env
end

module Without_recovery : RECOVER_INTF = struct
  type 'a parser = 'a I.checkpoint

  let initial entrypoint position = entrypoint position

  type 'a step = Intermediate of 'a parser | Success of 'a | Error

  let rec normalize = function
    | I.InputNeeded _ as cp -> Intermediate cp
    | I.Accepted x -> Success x
    | I.HandlingError _ | I.Rejected -> Error
    | (I.Shifting (_, _, _) | I.AboutToReduce (_, _)) as cp ->
        normalize (I.resume cp)

  let step cp token = normalize (I.offer cp token)

  let recover = function I.InputNeeded _ as cp -> cp | _ -> assert false

  let recovery_env = function I.InputNeeded env -> env | _ -> assert false
end

module With_recovery : PARSE_INTF = struct
  module M = Without_recovery

  type 'a parser = Correct of 'a M.parser | Recovering of 'a R.candidates

  let initial entry_point position = Correct (M.initial entry_point position)

  type 'a step = Intermediate of 'a parser | Success of 'a | Error

  let step parser token =
    match parser with
    | Correct parser -> (
        match M.step parser token with
        | M.Intermediate parser -> Intermediate (Correct parser)
        | M.Success x -> Success x
        | M.Error ->
            let env = M.recovery_env parser in
            Intermediate (Recovering (R.generate env)) )
    | Recovering candidates -> (
        match R.attempt candidates token with
        | `Ok (cp, _) -> Intermediate (Correct (M.recover cp))
        | `Accept x -> Success x
        | `Fail -> (
            match token with
            | Parser.EOF, _, _ -> (
                match candidates.final with
                | None -> Error
                | Some x -> Success x )
            | _ -> Intermediate parser ) )
end

let parse_with_recovery entrypoint tokens =
  let module P = With_recovery in
  let rec step tokens = function
    | P.Error -> failwith "Parsing failed"
    | P.Success x -> x
    | P.Intermediate p -> offer p tokens
  and offer p tokens =
    let token, rest =
      match tokens with
      | [] -> ((Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos), [])
      | token :: rest -> (token, rest)
    in
    step rest (P.step p token)
  in
  offer (P.initial entrypoint Lexing.dummy_pos) tokens

let lex_buf lexbuf =
  Lexer.init ();
  let rec loop acc =
    match Lexer.token lexbuf with
    | exception Lexer.Error _ -> loop acc
    | token -> (
        let acc = (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: acc in
        match token with Parser.EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let to_current =
  Migrate_parsetree.Versions.(
    migrate Migrate_ast.selected_version ocaml_current)

let parse p lexbuf = parse_with_recovery p (lex_buf lexbuf)

let implementation lexbuf =
  to_current.copy_structure (parse P.Incremental.implementation lexbuf)

let interface lexbuf =
  to_current.copy_signature (parse P.Incremental.interface lexbuf)

let copy_usefile uf = List.map to_current.copy_toplevel_phrase uf

let use_file lexbuf = copy_usefile (parse P.Incremental.use_file lexbuf)
