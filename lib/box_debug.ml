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
  .tooltiptext {
    visibility: hidden;
    width: 120px;
    background-color: black;
    color: #fff;
    text-align: center;
    padding: 5px 0;
    border-radius: 6px;
    position: absolute;
    z-index: 1;
  }
  .break:hover .tooltiptext {
    visibility: visible;
  }
|}

let debug = ref false

let with_box k fs =
  let g = !debug in
  debug := true ;
  fprintf fs
    {|
<html>
  <head>
    <style>
      %s
    </style>
  </head>
  <body>
    %t
  </body>
</html>
|}
    css k ;
  debug := g

let box_open ?name box_kind n fs =
  if !debug then (
    let name =
      match name with
      | Some s -> sprintf "%s:%s" box_kind s
      | None -> box_kind
    in
    let name = if n = 0 then name else sprintf "%s(%d)" name n in
    open_vbox 0 ;
    fprintf fs "<div class=\"box\">" ;
    pp_print_break fs 1 2 ;
    fprintf fs "<p class=\"name\">%s</p>" name ;
    pp_print_break fs 1 2 )

let box_close fs =
  if !debug then (
    pp_close_box fs () ; pp_print_break fs 0 0 ; fprintf fs "</div>" )

let break fs n o =
  if !debug then
    fprintf fs
      "<div class=\"break\">(%i,%i)<span class=\"tooltiptext\">break %i \
       %i</span></div>"
      n o n o

let pp_keyword fs s = fprintf fs "<span class=\"keyword\">%s</span>" s

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

let rec _format_string :
    type a b c d e f.
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

let cbreak fs ~fits:(s1, i, s2) ~breaks:(s3, j, s4) =
  if !debug then
    fprintf fs
      "<div class=\"break cbreak\">(%s,%i,%s) (%s,%i,%s)<span \
       class=\"tooltiptext\">cbreak ~fits:(%S, %i, %S) ~breaks:(%S, %i, \
       %S)</span></div>"
      s1 i s2 s3 j s4 s1 i s2 s3 j s4

let if_newline fs s =
  if !debug then
    fprintf fs
      "<div class=\"break if_newline\">(%s)<span \
       class=\"tooltiptext\">if_newline %S</span></div>"
      s s

let break_unless_newline fs n o =
  if !debug then
    fprintf fs
      "<div class=\"break break_unless_newline\">(%i,%i)<span \
       class=\"tooltiptext\">break_unless_newline %i %i</span></div>"
      n o n o

let fits_or_breaks fs fits n o breaks =
  if !debug then
    fprintf fs
      "<div class=\"break fits_or_breaks\">(%s,%i,%i,%s)<span \
       class=\"tooltiptext\">fits_or_breaks %S %i %i %S</span></div>"
      fits n o breaks fits n o breaks
