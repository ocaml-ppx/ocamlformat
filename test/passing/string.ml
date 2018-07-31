let f = function
  | () ->
      raise_s
        [%sexp
          "Xxxx \036 \036 \036 \036 \036 \036 \036 xxx xxxx xx xxxxxx xx \
           xxx xxxxxxx xxxxxx, xxxxxxx xxxxxxxxxx xx xxxx.  Xxxx."
          , 0]

let _ = "\010\xFFa\o123\n\
         \\\u{12345}aa🐪🐪🐪🐪🐪\n"

let _ =
  "aaaaaaaaaaaaaaaaaaaaaaaaa\n	\
   bbbbbbbbbbbbbbbbbbbbbbbbbb\n	\
   cccccccccccccccccc\n	\
   "

let _ = ('\xff', '\255', '\n')

let f = function '\xff'..'\255' -> ()


let f ("test" [@test "test"]) = 2
