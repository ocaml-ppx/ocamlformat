type fmt_code =
     Conf.t
  -> offset:int
  -> set_margin:bool
  -> string
  -> (Fmt.t, [`Msg of string]) Result.t

(** Render a [Fmt.t] to a string with a given margin. *)
let render_fmt ~margin t =
  let buffer = Buffer.create 256 in
  let fs = Format_.formatter_of_buffer buffer in
  Fmt.eval fs (Fmt.( $ ) (Fmt.set_margin margin) t) ;
  Format_.pp_print_flush fs () ;
  Buffer.contents buffer

(** Detect and strip delimiters from an OCaml code block. Returns
    [(inner_code, prefix_str, suffix_str, delimiter_len)]. *)
let strip_delimiters s =
  let len = String.length s in
  if
    len >= 4
    && Char.equal s.[0] '%'
    && Char.equal s.[1] '{'
    && Char.equal s.[len - 2] '%'
    && Char.equal s.[len - 1] '}'
  then
    (* %{ ... %} *)
    (String.sub s ~pos:2 ~len:(len - 4), "%{", "%}", 2)
  else if len >= 2 && Char.equal s.[0] '{' && Char.equal s.[len - 1] '}' then
    (* { ... } *)
    (String.sub s ~pos:1 ~len:(len - 2), "{", "}", 1)
  else
    (* No delimiters (e.g. mly trailer) *)
    (s, "", "", 0)

(** A code block with its source text and position info. *)
type block =
  { value: string
  ; loc: Location.t
  ; block_level: bool }

(** Find the column of the first non-whitespace character on the line
    starting at byte offset [bol] in [source]. *)
let line_indent source bol =
  let len = String.length source in
  let rec loop i =
    if i >= len then i - bol
    else
      match source.[i] with
      | ' ' | '\t' -> loop (i + 1)
      | _ -> i - bol
  in
  loop bol

(** Format a single OCaml code block and return the replacement string,
    or [None] if formatting fails. *)
let format_block ~source ~fmt_code ~(conf : Conf.t) ~menhir_mode
    (block : block) =
  let inner, open_delim, close_delim, _delim_len =
    strip_delimiters block.value
  in
  let trimmed = String.strip inner in
  if String.is_empty trimmed then None
  else
    let line_ind = line_indent source block.loc.loc_start.pos_bol in
    let offset =
      if block.block_level then 0 else line_ind + 2
    in
    let _ = menhir_mode in
    match fmt_code conf ~offset ~set_margin:false trimmed
    with
    | Ok fmt_t ->
        let margin = max 2 (conf.fmt_opts.margin.v - offset) in
        let formatted = render_fmt ~margin fmt_t in
        let formatted = String.rstrip formatted in
        let result =
          if String.is_empty open_delim then
            (* No delimiters: trailer — preserve leading newline *)
            let prefix =
              if String.length block.value > 0
                 && Char.equal block.value.[0] '\n'
              then "\n"
              else ""
            in
            prefix ^ formatted
          else if block.block_level then
            (* Header block (%{ %} or { } at top level) — no indentation *)
            open_delim ^ "\n" ^ formatted ^ "\n" ^ close_delim
          else if String.mem formatted '\n' then
            (* Multi-line action: indent based on first token of the line *)
            let indent = String.make (line_ind + 2) ' ' in
            let close_indent = String.make line_ind ' ' in
            let lines = String.split_lines formatted in
            let indented = String.concat ~sep:("\n" ^ indent) lines in
            open_delim ^ "\n" ^ indent ^ indented ^ "\n" ^ close_indent
            ^ close_delim
          else
            (* Single-line action: inline style *)
            open_delim ^ " " ^ formatted ^ " " ^ close_delim
        in
        Some result
    | Error _ -> None

let format_inplace ~source ~fmt_code ~conf ~menhir_mode ~cmts blocks =
  let source = Source.text source in
  let drop_comments_inside = Cmts.drop_inside cmts in
  let sorted =
    List.sort blocks ~compare:(fun (b1 : block) (b2 : block) ->
        Int.compare b1.loc.loc_start.pos_cnum b2.loc.loc_start.pos_cnum )
  in
  let mk_loc pos_start pos_end =
    let mk pos_cnum =
      {Lexing.pos_fname= ""; pos_lnum= 0; pos_bol= 0; pos_cnum}
    in
    {Location.loc_start= mk pos_start; loc_end= mk pos_end; loc_ghost= true}
  in
  let buf = Buffer.create (String.length source) in
  let pos =
    List.fold sorted ~init:0 ~f:(fun pos block ->
        let start = block.loc.loc_start.pos_cnum in
        let stop = block.loc.loc_end.pos_cnum in
        (* Copy verbatim text before this block and drop its comments *)
        Buffer.add_string buf (String.sub source ~pos ~len:(start - pos)) ;
        drop_comments_inside (mk_loc pos start) ;
        ( match
            format_block ~source ~fmt_code ~conf ~menhir_mode block
          with
        | Some replacement -> Buffer.add_string buf replacement
        | None ->
            Buffer.add_string buf
              (String.sub source ~pos:start ~len:(stop - start)) ) ;
        stop )
  in
  (* Copy remaining text after the last block *)
  let len = String.length source in
  Buffer.add_string buf (String.sub source ~pos ~len:(len - pos)) ;
  drop_comments_inside (mk_loc pos len) ;
  Buffer.contents buf
