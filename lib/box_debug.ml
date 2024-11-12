open Format_

let css =
  {|
  .box {
    border: 2px solid black;
    display: inline-block;
    font-family: courier;
    margin: 0;
    padding: 4px;
  }
  .box:hover {
    border-color: red;
  }
  .name {
    font-family: arial;
    font-style: italic;
    font-weight: lighter;
    font-size: 10px;
    padding: 1px;
    margin: 0 0 4px 0;
  }
  .break {
    background-color: black;
    color: white;
    display: inline-block;
    font-size: 10px;
    padding: 2px;
  }
  .cbreak {
    background-color: purple;
  }
  .keyword {
    background-color: yellow;
  }
  .if_newline {
    background-color: green;
  }
  .break_unless_newline {
    background-color: blue;
  }
  .fits_or_breaks {
    background-color: red;
  }
  .string_with_whitespaces {
    background-color: yellow;
    white-space: pre;
  }

  .tooltiptext {
    visibility: hidden;
    width: min-content;
    white-space: pre;
    background-color: black;
    color: #fff;
    text-align: left;
    padding: 5px 5px;
    border-radius: 6px;
    position: absolute;
    z-index: 1;
    font-size: 10px;
  }

  div:hover>.tooltiptext, span:hover>.tooltiptext {
    visibility: visible;
  }
|}

let debug = ref false

let enable_stacktraces = ref false

let get_stack () =
  if !enable_stacktraces then
    Stdlib.Printexc.(30 |> get_callstack |> raw_backtrace_to_string)
  else ""

let fprintf_as_0 fs fmt = Format_.kasprintf (Format_.pp_print_as fs 0) fmt

let debugf fs fmt =
  (* Print the debug as length 0 to not disturb complex breaks. *)
  if !debug then fprintf_as_0 fs fmt else Format_.ifprintf fs fmt

let with_box k fs =
  let g = !debug in
  debug := true ;
  fprintf_as_0 fs
    {|
<html>
  <head>
    <style>
      %s
    </style>
  </head>
  <body>
|}
    css ;
  k fs ;
  fprintf_as_0 fs {|
  </body>
</html>
|} ;
  debug := g

let pp_box_name fs = function
  | Some n -> Format_.fprintf fs ":%s" n
  | None -> ()

let pp_box_indent fs = function 0 -> () | i -> Format_.fprintf fs "(%d)" i

let stack_tooltip fs stack =
  match stack with
  | Some stack -> debugf fs "<span class=\"tooltiptext\">%s</span>" stack
  | None -> ()

let box_open ?name ?stack box_kind n fs =
  debugf fs "<div class=\"box\"><p class=\"name\"><span>%s%a%a</span>%a</p>"
    box_kind pp_box_name name stack_tooltip stack pp_box_indent n

let box_close fs = debugf fs "</div>"

let break fs n o ~stack =
  debugf fs
    "<div class=\"break\">(%i,%i)<span class=\"tooltiptext\">break %i %i\n\
     %s</span></div>"
    n o n o stack

let space_break ?stack fs =
  debugf fs "<div class=\"break space_break\">space_break%a</div>"
    stack_tooltip stack

let cut_break ?stack fs =
  debugf fs "<div class=\"break cut_break\">cut_break%a</div>" stack_tooltip
    stack

let force_newline ?stack fs =
  debugf fs "<div class=\"break force_newline\">force_newline%a</div>"
    stack_tooltip stack

let start_str fs s =
  let extra_class =
    match String.lfindi s ~f:(fun _ c -> Char.is_whitespace c) with
    | Some _ ->
        (* String contains whitespaces, color it *)
        " string_with_whitespaces"
    | None -> ""
  in
  debugf fs "<span class='string%s'>" extra_class

let end_str ?stack fs = debugf fs "%a</span>" stack_tooltip stack

let pp_keyword fs s = fprintf_as_0 fs "<span class=\"keyword\">%s</span>" s

let _pp_format_lit fs =
  let open CamlinternalFormatBasics in
  function
  | Close_box -> pp_keyword fs "@]"
  | Close_tag -> pp_keyword fs "@}"
  | Break (str, _, _) -> pp_keyword fs str
  | FFlush -> pp_keyword fs "@?"
  | Force_newline -> pp_keyword fs "@\\n"
  | Flush_newline -> pp_keyword fs "@."
  | Magic_size (str, _) -> pp_keyword fs str
  | Escaped_at -> fprintf fs "@@"
  | Escaped_percent -> fprintf fs "@@%%"
  | Scan_indic c -> pp_keyword fs ("@" ^ String.make 1 c)

let rec _format_string : type a b c d e f.
    _ -> (a, b, c, d, e, f) CamlinternalFormatBasics.fmt -> unit =
  let open CamlinternalFormatBasics in
  fun fs -> function
    | String_literal (s, tl) -> fprintf fs "%s%a" s _format_string tl
    | Char_literal (c, tl) -> fprintf fs "%c%a" c _format_string tl
    | Formatting_lit (lit, tl) ->
        fprintf fs "%a%a" _pp_format_lit lit _format_string tl
    | Formatting_gen (Open_box (Format (n, _)), tl) ->
        pp_keyword fs "@[" ; _format_string fs n ; _format_string fs tl
    | Formatting_gen (Open_tag (Format (n, _)), tl) ->
        pp_keyword fs "@{" ; _format_string fs n ; _format_string fs tl
    | End_of_format -> ()
    | _ -> pp_keyword fs "??"

(** Returns a boolean to signal the caller not to render the format string
    again. *)
let fmt fs f =
  let open CamlinternalFormatBasics in
  if !debug then (
    let (Format (fmt, _)) = f in
    _format_string fs fmt ; true )
  else false

let cbreak fs ~stack ~fits:(s1, i, s2) ~breaks:(s3, j, s4) =
  debugf fs
    "<div class=\"break cbreak\">(%s,%i,%s) (%s,%i,%s)<span \
     class=\"tooltiptext\">cbreak ~fits:(%S, %i, %S) ~breaks:(%S, %i, %S)\n\
     %s</span></div>"
    s1 i s2 s3 j s4 s1 i s2 s3 j s4 stack

let if_newline fs ~stack s =
  debugf fs
    "<div class=\"break if_newline\">(%s)<span \
     class=\"tooltiptext\">if_newline %S\n\
     %s</span></div>"
    s s stack

let break_unless_newline fs ~stack n o =
  debugf fs
    "<div class=\"break break_unless_newline\">(%i,%i)<span \
     class=\"tooltiptext\">break_unless_newline %i %i\n\
     %s</span></div>"
    n o n o stack

let fits_or_breaks fs ~stack fits n o breaks =
  debugf fs
    "<div class=\"break fits_or_breaks\">(%s,%i,%i,%s)<span \
     class=\"tooltiptext\">fits_or_breaks %S %i %i %S\n\
     %s</span></div>"
    fits n o breaks fits n o breaks stack
