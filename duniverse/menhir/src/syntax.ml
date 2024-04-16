(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The type [partial_grammar] describes the abstract syntax that is produced
   by the parsers (yacc-parser and fancy-parser).

   The type [grammar] describes the abstract syntax that is obtained after one
   or more partial grammars are joined (see [PartialGrammar]). It differs in
   that declarations are organized in a more useful way and a number of
   well-formedness checks have been performed. *)

type 'a located =
  'a Positions.located

(* ------------------------------------------------------------------------ *)

(* Terminals and nonterminal symbols are strings. *)

type terminal =
  string

type nonterminal =
  string

type symbol =
  string

type symbols =
  symbol list

(* In a somewhat fragile convention, in a partial grammar, a reference to a
   terminal symbol either is a normal identifier [LID], in which case it is
   the name of the terminal symbol, or is a quoted identifier [QID], in which
   case it is a token alias.

   Token aliases are eliminated by replacing them with the corresponding
   terminal symbols very early on during the joining of the partial grammars;
   see the module [ExpandTokenAliases].

   In a complete grammar, there are no token aliases any longer. That is,
   we keep track of the aliases that have been declared (they can be found
   via the field [tk_alias]), but we never use them, since they have been
   eliminated up front. *)

type alias =
  string option

(* Identifiers (which are used to refer to a symbol's semantic value) are
   strings. *)

type identifier =
  string

(* A file name is a string. *)

type filename =
  string

(* ------------------------------------------------------------------------ *)

(* A postlude is a source file fragment. *)

type postlude =
  Stretch.t

(* ------------------------------------------------------------------------ *)

(* OCaml semantic actions are represented as stretches. *)

type action =
  Action.t

(* ------------------------------------------------------------------------ *)

open Attribute

(* Attributes allow the user to annotate the grammar with information that is
   ignored by Menhir, but can be exploited by other tools, via the SDK. *)

(* Attributes can be attached in the following places:

   - with the grammar:         %[@bar ...]
   - with a terminal symbol:   %token FOO [@bar ...]
   - with a rule:              foo(X) [@bar ...]: ...
   - with a production:        e = expr { e } [@bar ...]
   - with a producer:          e = foo(quux) [@bar ...]
   - with an arbitrary symbol: %attribute FOO foo(quux) [@bar ...]

   After expanding away parameterized nonterminal symbols, things become
   a bit simpler, as %attribute declarations are desugared away. *)

(* ------------------------------------------------------------------------ *)

(* Information about tokens. (Only after joining.) *)

(* Associativity. *)

type token_associativity =
  | LeftAssoc
  | RightAssoc
  | NonAssoc
  | UndefinedAssoc

(* Precedence levels. *)

(* The special value [UndefinedPrecedence] is used for the special tokens
   [error] and [#]. It is also used for pseudo-tokens (which are not declared,
   but appear in a [%prec] annotation). A normal token receives a precedence
   level of the form [PrecedenceLevel (file, level, pos1, pos2)], where [file]
   is an input file and [level] is an integer level. Within each file, later
   declarations receive greater levels, which represent higher priority. Two
   declarations that originate in different files are incomparable. The
   positions [pos1] and [pos2] allow locating certain warnings. *)

type precedence_level =
  | UndefinedPrecedence
  | PrecedenceLevel of
      InputFile.input_file * int *
      Lexing.position * Lexing.position

type token_properties =
  {
             tk_filename      : filename;
             tk_ocamltype     : Stretch.ocamltype option;
             tk_position      : Positions.t;
             tk_alias         : alias;
             tk_attributes    : attributes;
    mutable  tk_associativity : token_associativity;
    mutable  tk_precedence    : precedence_level;
    mutable  tk_is_declared   : bool;
  }

(* ------------------------------------------------------------------------ *)

(* A [%prec] annotation is optional. A production can carry at most one.
   If there is one, it is a symbol name. See [ParserAux]. *)

type prec_annotation =
  symbol located option

(* ------------------------------------------------------------------------ *)

(* A "production level" is used to solve reduce/reduce conflicts. It reflects
   which production appears first in the grammar. See [ParserAux]. *)

type production_level =
  | ProductionLevel of InputFile.input_file * int

(* ------------------------------------------------------------------------ *)

(* A level is attached to every [%on_error_reduce] declaration. It is used
   to decide what to do when several such declarations are applicable in a
   single state. *)

type on_error_reduce_level =
  production_level (* we re-use the above type, to save code *)

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* The old rule syntax. Although old, still used internally. The new syntax
   is translated down to it. *)

(* A parameter is either just a symbol or an application of a symbol to a
   nonempty tuple of parameters. Before anonymous rules have been eliminated,
   it can also be an anonymous rule, represented as a list of branches. *)

type parameter =
  | ParameterVar of symbol located
  | ParameterApp of symbol located * parameters
  | ParameterAnonymous of parameterized_branch list located

and parameters =
  parameter list

(* ------------------------------------------------------------------------ *)

(* A producer is a pair of identifier and a parameter. In concrete syntax, it
   could be [e = expr], for instance. The identifier [e] is always present.
   (A use of the keyword [$i] in a semantic action is turned by the lexer
   and parser into a reference to an identifier [_i].) A producer carries
   a number of attributes. *)

and producer =
  identifier located * parameter * attributes

(* ------------------------------------------------------------------------ *)

(* A branch (a production) is a sequence of producers followed with
   a semantic action. *)

and parameterized_branch =
  {
    (* The branch's position in a source file. *)
    pb_position         : Positions.t;
    (* The producers. *)
    pb_producers        : producer list;
    (* The semantic action. *)
    pb_action           : action;
    (* An optional [%prec] annotation. *)
    pb_prec_annotation  : prec_annotation;
    (* The branch's production level. *)
    pb_production_level : production_level;
    (* Attributes attached with this branch. *)
    pb_attributes       : attributes;
  }

(* ------------------------------------------------------------------------ *)

(* A rule is the definition of a nonterminal symbol. *)

(* A rule has a header and several branches (productions). *)

type parameterized_rule =
  {
    (* Is the [%public] keyword present? *)
    pr_public_flag       : bool;
    (* Is the [%inline] keyword present? *)
    pr_inline_flag       : bool;
    (* The name of the nonterminal symbol that is being defined. *)
    pr_nt                : nonterminal;
    (* Positions in the source files. *)
    pr_positions         : Positions.t list;
    (* Attributes attached with this nonterminal symbol. *)
    pr_attributes        : attributes;
    (* The parameters of this nonterminal symbol. *)
    pr_parameters        : symbols;
    (* The productions. *)
    pr_branches          : parameterized_branch list;
  }

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* The new rule syntax. *)

(* In the user's eyes, this replaces the old rule syntax, which corresponds to
   the types [parameter], [producer], [parameterized_branch], and
   [parameterized_rule] above. *)

(* Internally, the new rule syntax is translated down to the old rule syntax;
   see [NewRuleSyntax]. This is done on the fly during parsing. *)

type pattern =
  | SemPatVar of identifier located
  | SemPatWildcard
  | SemPatTilde of Positions.t
  | SemPatTuple of pattern list
  (* Patterns: as in the manual. *)

type raw_action =
  Settings.dollars -> identifier option array -> action
  (* Ugly type produced by the lexer for an ACTION token. *)

type expression =
  choice_expression located
  (* A toplevel expression is a choice expression. *)

and choice_expression =
  | EChoice of branch list
  (* A choice expression is a list of branches. *)

and branch =
  | Branch of seq_expression * production_level
  (* A branch is a sequence expression,
     plus an ugly [production_level]. *)

and seq_expression =
  raw_seq_expression located

and raw_seq_expression =
  | ECons of pattern * symbol_expression * seq_expression
  | ESingleton of symbol_expression
  | EAction of extended_action * prec_annotation * attributes
  (* A sequence is either a cons [p = e1; e2]
     or a lone symbol expression [e]
     or a semantic action. *)

and symbol_expression =
  | ESymbol of symbol located * expression list * attributes
  (* A symbol expression is a symbol,
     possibly accompanied with actual parameters and attributes. *)

and extended_action =
  | XATraditional of raw_action
  | XAPointFree of Stretch.t option
  (* A semantic action is either traditional { ... } or point-free.
     There are two forms of point-free actions, <> and <id>.
     In the latter case, [id] is an OCaml identifier. *)

type rule =
  {
    rule_public: bool;
    rule_inline: bool;
    rule_lhs: symbol located;
    rule_attributes: attributes;
    rule_formals: symbol located list;
    rule_rhs: expression;
  }

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* A declaration. (Only before joining.) *)

type declaration =

    (* Raw OCaml code. *)

  | DCode of Stretch.t

    (* Raw OCaml functor parameter. *)

  | DParameter of Stretch.ocamltype (* really a stretch *)

    (* Terminal symbol (token) declaration. *)

  | DToken of Stretch.ocamltype option * terminal * alias * attributes

    (* Start symbol declaration. *)

  | DStart of nonterminal

    (* Priority and associativity declaration. *)

  | DTokenProperties of terminal * token_associativity * precedence_level

    (* Type declaration. *)

  | DType of Stretch.ocamltype * parameter

    (* Grammar-level attribute declaration. *)

  | DGrammarAttribute of attribute

    (* Attributes shared among multiple symbols, i.e., [%attribute]. *)

  | DSymbolAttributes of parameter list * attributes

    (* On-error-reduce declaration. *)

  | DOnErrorReduce of parameter * on_error_reduce_level

(* ------------------------------------------------------------------------ *)

(* A partial grammar. (Only before joining.) *)

type partial_grammar =
  {
    pg_filename          : filename;
    pg_postlude          : postlude option;
    pg_declarations      : declaration located list;
    pg_rules             : parameterized_rule list;
  }

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* A grammar. (Only after joining.) *)

(* The differences with partial grammars (above) are as follows:
   1. the file name is gone (there could be several file names, anyway).
   2. there can be several postludes.
   3. declarations are organized by kind: preludes, postludes,
      functor %parameters, %start symbols, %types, %tokens, %on_error_reduce,
      grammar attributes, %attributes.
   4. rules are stored in a map, indexed by symbol names, instead of a list.
   5. token aliases have been replaced with ordinary named terminal symbols.
 *)
type grammar =
  {
    p_preludes           : Stretch.t list;
    p_postludes          : postlude list;
    p_parameters         : Stretch.t list;
    p_start_symbols      : Positions.t StringMap.t;
    p_types              : (parameter * Stretch.ocamltype located) list;
    p_tokens             : token_properties StringMap.t;
    p_on_error_reduce    : (parameter * on_error_reduce_level) list;
    p_grammar_attributes : attributes;
    p_symbol_attributes  : (parameter list * attributes) list;
    p_rules              : parameterized_rule StringMap.t;
  }
