type t = k
(** VALID BLOCKS:

    Block delimiters should be on their own line:
    {[ let x = 1 ]}

    As of odoc 2.1, a block can carry metadata:
    {@ocaml[
      let x = 2
    ]}

    An OCaml block that should break:
    {[
      let x = 2 in x + x
    ]}

    A toplevel phrase with no output:
    {[
    # let x = 2 and y = 3 in x+y;;
    ]}

    A toplevel phrase with output:
    {@ocaml[
    # let x = 2;;
    val x : int = 2
    ]}

    Many toplevel phrases without output:
    {[
    # let x = 2;;
    # x + 2;;
    # let x = 2 and y = 3 in x+y;;
    ]}

    Many toplevel phrases with output:
    {[
    # let x = 2;;
    val x : int = 2
    # x + 2;;
    - : int = 4
    # let x = 2 and y = 3 in x+y;;
    ]}

    Output are printed after a newline:
    {[
    # let x = 2;; val x : int = 2
    # let x = 3;;
    # let x = 4;; val x : int = 4
    ]}

    Excessive linebreaks are removed:
    {[

      # let x = 2 in x+1;;

      output

      # let y = 3 in y+1;;

    ]}

    Linebreak after `#`:
    {[
    #
      let x = 2 in x+1;;
    ]}
*)

(** INVALID BLOCKS: The formatting of invalid blocks is preserved.

    Invalid toplevel phrase/ocaml block:
    {[
    - : int =
     4
    ]}

    Output before a toplevel phrase:
    {[
    - : int = 4
    # 2+2;;
    ]}

    No `;;` at the end of the phrase, no output:
    {[
    # let x = 2 in x+1
    ]}

    No `;;` at the end of the phrase, with output:
    {[
    # let x = 2 in x+1
    some output
    ]}

    Multiple phrases without `;;` at the end:
    {[
    # let x = 2 in x+1
    # let x = 4 in x+1
    ]}
*)
