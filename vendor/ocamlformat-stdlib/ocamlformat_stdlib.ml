include Base
include Stdio

module Cmdliner = Cmdliner_ext
module Fpath = Fpath_ext
module List = List_ext
module String = String_ext
module Warning = Warning
module Format = Caml.Format
module Filename = Caml.Filename

let ( >> ) f g x = g (f x)

let impossible msg = failwith msg

let check f x =
  assert (
    ignore (f x) ;
    true ) ;
  x
