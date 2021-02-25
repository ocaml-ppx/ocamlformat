open! Dune_engine
open! Stdune
open Import

(* We redirect the output of all commands, so by default the various tools will
   disable colors. Since we support colors in the output of commands, we force
   it via specific environment variables if stderr supports colors. *)
let setup_env_for_colors env =
  let set env var value =
    Env.update env ~var ~f:(function
      | None -> Some value
      | Some s -> Some s)
  in
  let env = set env "OPAMCOLOR" "always" in
  let env = set env "OCAML_COLOR" "always" in
  env

module Style = struct
  include User_message.Style

  let to_styles = User_message.Print_config.default

  let of_string = function
    | "loc" -> Some Loc
    | "error" -> Some Error
    | "warning" -> Some Warning
    | "kwd" -> Some Kwd
    | "id" -> Some Id
    | "prompt" -> Some Prompt
    | "details" -> Some Details
    | "ok" -> Some Ok
    | "debug" -> Some Debug
    | _ -> None
end

let mark_open_tag s =
  match Style.of_string s with
  | Some style -> Ansi_color.Style.escape_sequence (Style.to_styles style)
  | None ->
    if s <> "" && s.[0] = '\027' then
      s
    else
      ""

let setup_err_formatter_colors () =
  let open Format in
  if Lazy.force Ansi_color.stderr_supports_color then
    List.iter [ err_formatter; Dune_util.Report_error.ppf ] ~f:(fun ppf ->
        let funcs = (pp_get_formatter_tag_functions ppf () [@warning "-3"]) in
        pp_set_mark_tags ppf true;
        pp_set_formatter_tag_functions ppf
          { funcs with
            mark_close_tag = (fun _ -> Ansi_color.Style.escape_sequence [])
          ; mark_open_tag
          } [@warning "-3"])
