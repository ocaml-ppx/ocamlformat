module P = Parser
module I = P.MenhirInterpreter
open Migrate_ast

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

let merge_adj merge l =
  List.fold_left
    (fun acc x ->
      match acc with
      | h :: t -> (
          match merge h x with Some m -> m :: t | None -> x :: h :: t )
      | [] -> x :: acc)
    [] l
  |> List.rev

let normalize_locs locs =
  List.sort_uniq Location.compare locs |> merge_adj Location.merge

let process p m pprint print lexbuf =
  let ast = parse_with_recovery p (lex_buf lexbuf) in
  let loc_stack = Stack.create () in
  let loc_list = ref [] in
  let make_mapper () =
    let open Parsetree in
    let default = Ast_mapper.default_mapper in
    let expr m e =
      if Annot.Exp.is_generated e then
        loc_list := Stack.top loc_stack :: !loc_list;
      default.expr m e
    in
    let class_expr m x =
      if Annot.Class_exp.is_generated x then
        loc_list := Stack.top loc_stack :: !loc_list;
      default.class_expr m x
    in
    let wrap mapper loc f x =
      if Stack.is_empty loc_stack then (
        Stack.push loc loc_stack;
        let x = f mapper x in
        ignore (Stack.pop loc_stack);
        x )
      else f mapper x
    in
    let structure_item m x = wrap m x.pstr_loc default.structure_item x in
    let signature_item m x = wrap m x.psig_loc default.signature_item x in
    let attribute m x =
      if Annot.Attr.is_generated x then
        loc_list := Stack.top loc_stack :: !loc_list;
      default.attribute m x
    in
    {
      Ast_mapper.default_mapper with
      attribute;
      expr;
      class_expr;
      structure_item;
      signature_item;
    }
  in
  let mapper = make_mapper () in
  let ast' = (m mapper) ast in
  let debug = false in
  if debug then Format.printf "%a\n%!" print ast';
  if debug then Format.printf "%a\n%!" pprint ast';
  normalize_locs !loc_list

let implementation =
  process P.Incremental.implementation Mapper.structure Pprintast.structure
    Printast.implementation

let interface =
  process P.Incremental.interface Mapper.signature Pprintast.signature
    Printast.interface

let pp_use_file fs x =
  List.iter
    (fun tph -> Format.fprintf fs "%a\n" Pprintast.toplevel_phrase tph)
    x

let use_file =
  process P.Incremental.use_file Mapper.use_file pp_use_file Printast.use_file
