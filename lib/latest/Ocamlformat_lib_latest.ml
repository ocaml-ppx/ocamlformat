(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

include Translation_unit

module Private = struct
  module Assoc = Assoc
  module Ast = Ast
  module Chunk = Chunk
  module Cmt = Cmt
  module Cmts = Cmts
  module Docstring = Docstring
  module Eol_compat = Eol_compat
  module Exposed = Exposed
  module Extended_ast = Extended_ast
  module Fmt_ast = Fmt_ast
  module Fmt_odoc = Fmt_odoc
  module Fmt = Fmt
  module Indent = Indent
  module Literal_lexer = Literal_lexer
  module Loc_tree = Loc_tree
  module Multimap = Multimap
  module Non_overlapping_interval_tree = Non_overlapping_interval_tree
  module Normalize_std_ast = Normalize_std_ast
  module Params = Params
  module Parse_with_comments = Parse_with_comments
  module Prec = Prec
  module Source = Source
  module Std_ast = Std_ast
  module Std_longident = Std_longident
  module Sugar = Sugar
  module Toplevel_lexer = Toplevel_lexer
  module Translation_unit = Translation_unit
end
