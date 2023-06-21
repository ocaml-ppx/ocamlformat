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
  Format_.fprintf fs
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
      | Some s -> Format_.sprintf "%s:%s" box_kind s
      | None -> box_kind
    in
    let name = if n = 0 then name else Format_.sprintf "%s(%d)" name n in
    Format_.open_vbox 0 ;
    Format_.fprintf fs "<div class=\"box\">" ;
    Format_.pp_print_break fs 1 2 ;
    Format_.fprintf fs "<p class=\"name\">%s</p>" name ;
    Format_.pp_print_break fs 1 2 )

let box_close fs =
  if !debug then (
    Format_.pp_close_box fs () ;
    Format_.pp_print_break fs 0 0 ;
    Format_.fprintf fs "</div>" )

let break fs n o =
  if !debug then
    Format_.fprintf fs
      "<div class=\"break\">(%i,%i)<span class=\"tooltiptext\">break %i \
       %i</span></div>"
      n o n o

let fmt fs f =
  if !debug then
    let sub s ~after:i =
      String.sub s ~pos:(i + 1) ~len:(String.length s - i - 1)
    in
    let rec loop = function
      | "" -> ()
      | s -> (
        match String.index s '@' with
        | Some i ->
            if i > 0 && Char.(String.get s (i - 1) <> '\\') then
              if i < String.length s - 1 then
                match String.get s (i + 1) with
                | ' ' ->
                    break fs 1 0 ;
                    loop (sub s ~after:i)
                | ',' ->
                    break fs 0 0 ;
                    loop (sub s ~after:i)
                | ';' -> (
                  match String.index s '>' with
                  | Some e ->
                      let subs = String.sub s ~pos:i ~len:(e - i + 1) in
                      let n, o =
                        try
                          Caml.Scanf.sscanf subs "@;<%i %i>" (fun x y ->
                              (x, y) )
                        with _ -> (1, 0)
                      in
                      break fs n o ;
                      loop (sub s ~after:e)
                  | None ->
                      break fs 1 0 ;
                      loop (sub s ~after:i) )
                | _ -> loop (sub s ~after:i)
              else ()
            else loop (sub s ~after:i)
        | None -> () )
    in
    loop (Caml.string_of_format f)

let cbreak fs ~fits:(s1, i, s2) ~breaks:(s3, j, s4) =
  if !debug then
    Format_.fprintf fs
      "<div class=\"break cbreak\">(%s,%i,%s) (%s,%i,%s)<span \
       class=\"tooltiptext\">cbreak ~fits:(%S, %i, %S) ~breaks:(%S, %i, \
       %S)</span></div>"
      s1 i s2 s3 j s4 s1 i s2 s3 j s4

let if_newline fs s =
  if !debug then
    Format_.fprintf fs
      "<div class=\"break if_newline\">(%s)<span \
       class=\"tooltiptext\">if_newline %S</span></div>"
      s s

let break_unless_newline fs n o =
  if !debug then
    Format_.fprintf fs
      "<div class=\"break break_unless_newline\">(%i,%i)<span \
       class=\"tooltiptext\">break_unless_newline %i %i</span></div>"
      n o n o

let fits_or_breaks fs fits n o breaks =
  if !debug then
    Format_.fprintf fs
      "<div class=\"break fits_or_breaks\">(%s,%i,%i,%s)<span \
       class=\"tooltiptext\">fits_or_breaks %S %i %i %S</span></div>"
      fits n o breaks fits n o breaks
