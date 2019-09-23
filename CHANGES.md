### 0.8 (2018-10-09)

  + Improve: set break-sequences in sparse and compact profiles (#455) (Josh Berdine)
  + Improve: keep a space inside tuples parens (#453) (Guillaume Petiot)
  + Improve: --root option to isolate configuration files (#402) (Guillaume Petiot)
  + Fix: missing parens around partially-applied `::` (#452) (Josh Berdine)
  + Fix: parens around expressions with attributes (#441) (Guillaume Petiot)
  + Build: do not execute shell scripts directly in build (#448) (David Allsopp)
  + Add: read ocp indent config files (#445) (Guillaume Petiot)
  + Improve: option 'break-sequences' (#434) (Guillaume Petiot)
  + Improve: option 'no-indicate-multiline-delimiters' to have less whitespaces (#429) (Guillaume Petiot)
  + Fix: outdent before arrow (#444) (Guillaume Petiot)
  + Improve: User documentation (#449) (Guillaume Petiot)
  + Improve: option 'cases-exp-indent' to adjust indent (#428) (Guillaume Petiot)
  + Add: compact and sparse profiles (#408) (Josh Berdine)
  + Improve: explicit error message in case of 'permission denied' error (#425) (Guillaume Petiot)
  + Fix: comment stabilization in Pexp_override (#422) (Guillaume Petiot)
  + Fix: corner case while formatting type variables   (#440) (Hugo Heuzard)
  + Fix: many missing comments  (#418) (Hugo Heuzard)
  + Fix: asserts and attributes (#414) (Hugo Heuzard)
  + Fix: extension and attribute (#417) (Hugo Heuzard)
  + Improve: support for function%ext (#416) (Hugo Heuzard)
  + Fix: Inconsistent spacing around comments in signatures vs structures (#437) (Guillaume Petiot)
  + Improve: better error with location when comment dropped (#401) (Guillaume Petiot)
  + Fix doc comments (#413) (Hugo Heuzard)
  + Improve: use input_name for error messages (Hugo Heuzard)
  + Improve: break after inherit (Hugo Heuzard)
  + Fix: aliases inside constructor declaration (#424) (Guillaume Petiot)
  + Fix: broken invariant for Pmod_unpack (#421) (Guillaume Petiot)
  + Fix: print error details in debug mode only (#420) (Hugo Heuzard)
  + Fix: mark_parenzed_inner_nested_match (Hugo Heuzard)
  + Improve: tune the janestreet profile (Hugo Heuzard)
  + Improve: disable ocamlformat if no dot ocamlformat in the project (#391) (Hugo Heuzard)
  + Improve: new option to control spacing around let bindings (#344) (Hugo Heuzard)
  + Fix: prec of string/array sugar (#381) (Hugo Heuzard)
  + Fix: lost comment in constraint expression (#400) (Guillaume Petiot)
  + Fix: lost cmt near functor (#399) (Guillaume Petiot)
  + Improve: preset profiles (default & janestreet) (#390) (Guillaume Petiot)
  + Improve: try to fit simple list/array elements on a single line (#375) (Guillaume Petiot)
  + Fix: bad comment spacing with module-item-spacing=compact (#395) (Guillaume Petiot)
  + Fix: dropped comment in revapply of extension (#394) (Guillaume Petiot)
  + Improve: let-and structures spacing depends on module-item-spacing (#367) (Guillaume Petiot)
  + Fix: consecutive prefix operator (#386) (Hugo Heuzard)
  + Fix: invalid (#383) (Hugo Heuzard)
  + Fix: lazy and alias (#388) (Hugo Heuzard)
  + Improve: main loop and error reporting (#389) (Hugo Heuzard)
  + Fix: exposed_left_typ (#385) (Hugo Heuzard)
  + Fix: rec functor (#382) (Hugo Heuzard)
  + Fix: while%ext/for%ext (Hugo Heuzard)
  + Fix: more on class (Hugo Heuzard)
  + Fix: invalid syntax on class (Hugo Heuzard)
  + Improve: follow XDG for global config files (Guillaume Petiot)
  + Improve: add support for bigarray sugar index operator (Hugo Heuzard)
  + Add: support reading input from stdin (#353) (Brandon Kase)
  + Fix: the precedence of options (Guillaume Petiot)
  + Improve: doc of config option choice alternatives (#354) (Josh Berdine)
  + Improve: string formatting (Hugo Heuzard)

  (plus internal, build, test, etc. changes including contributions from
   Guillaume Petiot, Hugo Heuzard, Josh Berdine, David Allsopp, Anil Madhavapeddy)

### 0.7 (2018-08-23)

  + Improve: simplify setting option defaults, slight --help improvement (#350) (Josh Berdine)
  + Improve: update emacs mode to use replace-buffer-contents (#345) (Hugo Heuzard)
  + Improve: add option to not force-break structs (#346) (Josh Berdine)
  + Improve: move 'formatting' options into separate section (#348) (Guillaume Petiot)
  + Improve: fun sugar (Hugo Heuzard)
  + Improve: add option to omit open lines between one-line module items (#303) (Guillaume Petiot)
  + Fix: infix ops (Hugo Heuzard)
  + Improve: reformat files with no locations (Hugo Heuzard)
  + Improve: better error when max-iters = 1 (Hugo Heuzard)
  + Improve: breaking before arrows in pattern match casees (Josh Berdine)
  + Improve: no parens for trailing 'not' (Hugo Heuzard)
  + Improve: missing break hint, fix #331 (Hugo Heuzard)
  + Improve: comments before match (#330) (Josh Berdine)
  + Fix: missing comments (Hugo Heuzard)
  + Fix: missing attributes due to sugar (Hugo Heuzard)
  + Fix: parens non trivial rhs for # infix ops (Hugo Heuzard)
  + Improve: let-module-in break before `in` (#328) (Josh Berdine)
  + Improve: sugar for nestest module_with (Hugo Heuzard)
  + Improve: attributes on let bindings (#324) (Josh Berdine)
  + Improve: wrapping of functor args in type declaration (#315) (Guillaume Petiot)
  + Fix: comments attachment with infix ops (Hugo Heuzard)
  + Fix: comments attachment with Pexp_fun (Hugo Heuzard)
  + Fix: docstrings as attributes (Hugo Heuzard)
  + Improve: refactor and improve documentation of options (#302) (Guillaume Petiot)
  + Improve: error reporting in emacs integration (#304) (Josh Berdine)
  + Improve: pexp_open as final arg of infix op (#314) (Josh Berdine)
  + Fix: missing parens around labeled record pattern arg (Josh Berdine)
  + Fix: missing attributes (Hugo Heuzard)
  + Fix: duplicated (x3) attributes in pexp_letmodule (Hugo Heuzard)
  + Improve: allow to locally disable ocamlformat (Hugo Heuzard)
  + Improve: corner case indentation of fun -> function (#312) (Josh Berdine)
  + Improve: labeled, optional, and default args (Josh Berdine)
  + Improve: punning default arg with type constraint (Josh Berdine)
  + Improve: add options to controls various spaces (#284) (Hugo Heuzard)
  + Improve: add option to disable wrapping fun args (#283) (Hugo Heuzard)
  + Improve: add option --break-cases to break each pattern-matching case (#251) (Guillaume Petiot)
  + Improve: rename --nested-parens option (Hugo Heuzard)
  + Improve: ws before colon for constraint (#293) (Hugo Heuzard)
  + Improve: option to choose where to parens nested match (Hugo Heuzard)
  + Improve: always parens nested match (even the right most one) (Hugo Heuzard)
  + Improve: always break for let_and contruct (Hugo Heuzard)
  + Fix: missing comments (Hugo Heuzard)
  + Improve: Add option to preserve style of local module open (#267) (Guillaume Petiot)
  + Improve: preserve extension point formatting (Hugo Heuzard)
  + Improve: make double semi consistent between implementation and use_file (#292) (Hugo Heuzard)
  + Improve: configure ocamlformat using attributes (Hugo Heuzard)
  + Improve: extension point (Hugo Heuzard)
  + Improve: break in type declaration for variant and record (#280) (Hugo Heuzard)
  + Fix: memory leak (Hugo Heuzard)
  + Test: add ocaml compiler to test suite, and improve `make -C test` (Josh Berdine)
  + Fix: unary operator +/- (Hugo Heuzard)
  + Fix: doc comments in class (Hugo Heuzard)
  + Fix: ocaml bug, sort fields (Hugo Heuzard)
  + Improve: empty mod with comments (Hugo Heuzard)
  + Improve: disable warning generated by the lexer in quiet mode (Hugo Heuzard)
  + Fix: record update (Hugo Heuzard)
  + Fix: rec let binding and attribute (Hugo Heuzard)
  + Fix: punning (Hugo Heuzard)
  + Fix: let open and constraint (Hugo Heuzard)
  + Fix: not extension sugar when attribute (Hugo Heuzard)
  + Fix: no-comment-check missing case (Hugo Heuzard)
  + Fix: string literal (Hugo Heuzard)
  + Fix: format of infix in presence of %; (Hugo Heuzard)
  + Fix: let binding and type annot (Hugo Heuzard)
  + Fix: binding when lhs is an extension (Hugo Heuzard)
  + Fix: pat constraint in payload (Hugo Heuzard)
  + Fix: let rec with extension (Hugo Heuzard)
  + Fix: comments (Hugo Heuzard)
  + Fix: comments in fmt_case (Hugo Heuzard)
  + Fix: comments around Longident (Hugo Heuzard)
  + Fix: missing comment for pmty_with (Hugo Heuzard)
  + Improve: add option to disambiguate infix precedence with parens (Josh Berdine)
  + Improve: `not` when infix op arg (Josh Berdine)
  + Improve: add a flag to skip all checks about comments (Hugo Heuzard)
  + Improve: breaking of module ident/unpack/extension exps (#269) (Josh Berdine)
  + Fix: literal sub-exps with attributes (Josh Berdine)
  + Fix: many fixes regarding attributes (Hugo Heuzard)
  + Improve: preserve formatting of block comments (#255) (Hugo Heuzard)
  + Improve: breaking of applications with long literal list args (#258) (Josh Berdine)
  + Fix: sugar functor (Hugo Heuzard)
  + Fix: type alias in variant (Hugo Heuzard)
  + Improve: formatting of comments (Josh Berdine)
  + Fix: prefix operators (Hugo Heuzard)
  + Fix: exception declarations (Hugo Heuzard)
  + Fix: doc comments in structure (#250) (Hugo Heuzard)
  + Fix: add parens around pat with trailing attr (Hugo Heuzard)
  + Fix: let binding and Ppat_or (Hugo Heuzard)
  + Fix: be more permissive with pattern (Hugo Heuzard)
  + Fix: fix string_literal with when its location includes its attribute (#244) (Hugo Heuzard)
  + Improve: improve errors returned to the user. (#243) (Hugo Heuzard)
  + Fix: missing comments for Pexp_construct (#240) (Hugo Heuzard)
  + Fix: multiple fixes around classes (#242) (Hugo Heuzard)
  + Fix: comments in empty () and [] (#241) (Hugo Heuzard)
  + Fix: support empty variant (#239) (Hugo Heuzard)
  + Fix: add missing attribute (#238) (Hugo Heuzard)
  + Fix: be more permissive with ppat_interval (#237) (Hugo Heuzard)
  + Improve: remove trailing ws (#210) (Hugo Heuzard)
  + Improve: attributes on type declarations (#232) (Josh Berdine)
  + Improve: breaking of infix Array and String get and set ops (#233) (Josh Berdine)
  + Fix: attributes and doc comments (#221) (Hugo Heuzard)
  + Improve: spacing of module unpack (#230) (Josh Berdine)
  + Improve: no parent for new (Hugo Heuzard)
  + Fix: Revert: Improve: remove redundant parens around application operators (Hugo Heuzard)
  + Improve: array alignment (#226) (Hugo Heuzard)
  + Improve: nested array infix ops (#225) (Hugo Heuzard)
  + Fix: is_adjacent and remove [~inclusive] from [Source.string_between] (Hugo Heuzard)
  + Fix: Cmts.CmtSet.split (Hugo Heuzard)
  + Improve: Allow comments inside empty delimited "things" (#223) (Hugo Heuzard)
  + Fix: Source.ends_line (#222) (Hugo Heuzard)
  + Improve: empty struct and sig (#217) (Hugo Heuzard)
  + Improve: support for toplevel files (#218) (Hugo Heuzard)
  + Fix: string literal, fix #214 (#219) (Hugo Heuzard)
  + Improve: more tuning for functors (Hugo Heuzard)
  + Improve: sugar for functor type with multiple args (Hugo Heuzard)
  + Improve: sugar for functors with multiple args (Hugo Heuzard)
  + Improve: module type with (Hugo Heuzard)
  + Improve: break before with/and type (Hugo Heuzard)
  + Improve: break between fun args (Hugo Heuzard)
  + Improve: module unpacking (#215) (Hugo Heuzard)
  + Improve: for & while loops (#211) (Hugo Heuzard)
  + Fix: attributes on ite (#209) (Hugo Heuzard)
  + Fix: partially applied (+) and (-) (#208) (Hugo Heuzard)
  + Fix: polymorphic variant (#202) (Hugo Heuzard)
  + Fix: parens with lazy pat (fix #199) (#201) (Hugo Heuzard)
  + Improve: omit excess indentation of `function` cases (Josh Berdine)
  + Improve: extensions with payloads of multiple structure items (Josh Berdine)
  + Improve: parenthesization and attribute placement of if-then-else (Josh Berdine)
  + Fix: do not attach comments to docstrings (Josh Berdine)
  + Fix: short syntax for extensions (#193) (Hugo Heuzard)
  + Fix: missing attrs for pcl_fun (Hugo Heuzard)
  + Fix: pos of attribute for functors (Hugo Heuzard)
  + Fix: () module only if not attrs (Hugo Heuzard)
  + Fix: missing attrs for object (Hugo Heuzard)
  + Fix: no short form of extension with attribs (Hugo Heuzard)
  + Fix: normalization for Pexp_poly and Pexp_constraint (#190) (Hugo Heuzard)
  + Fix: some parenthesization context checks (#189) (Hugo Heuzard)
  + Fix: attributes on fun expressions (Josh Berdine)
  + Fix: extensions with multiple module-level eval expressions (#185) (Josh Berdine)
  + Fix: functor & apply (#182) (Hugo Heuzard)
  + Fix: module rec with (Hugo Heuzard)
  + Fix: more parens in pat_constraint (Hugo Heuzard)
  + Improve: tuple & constraint (Hugo Heuzard)
  + Improve: empty module (#178) (Hugo Heuzard)
  + Fix: extensible variant (#177) (Hugo Heuzard)
  + Fix: index operator (#176) (Hugo Heuzard)
  + Improve: empty module sig (Hugo Heuzard)
  + Fix: add attributes to module signature (Hugo Heuzard)
  + Add: support for objects and classes (#173) (Hugo Heuzard)
  + Improve: remove some redundant parens around tuple types (Josh Berdine)
  + Fix: args in let bindings (Hugo Heuzard)
  + Improve: let module%ext (Hugo Heuzard)
  + Fix: infix op in alias (Hugo Heuzard)
  + Fix: extensions pat (Hugo Heuzard)
  + Fix: limit use of short syntax for extensions (Hugo Heuzard)
  + Improve: allow break after Psig_include (Josh Berdine)
  + Fix: { (a; b) with a; b } (Hugo Heuzard)
  + Fix: with type [longident] (Hugo Heuzard)
  + Fix: attributes on polymorphic variants (Hugo Heuzard)
  + Fix: attribute in let bindings (Hugo Heuzard)
  + Fix: private in extensible variant (Hugo Heuzard)
  + Fix: gadt in extensible variant (Hugo Heuzard)
  + Fix: missing parens in list pattern (Hugo Heuzard)
  + Improve: format [new e] like an apply (Hugo Heuzard)
  + Fix: parens for constraint (Hugo Heuzard)
  + Fix: avoid emitting `>]` which is an unparsable keyword (#171) (Hugo Heuzard)
  + Fix: misplaced comments on `module type of` (Josh Berdine)

  (plus many internal, build, test, etc. changes including contributions from
   Hugo Heuzard, Josh Berdine, Thomas Gazagnaire, and Sebastien Mondet)

### 0.6 (2018-04-29)

- Features
  + Add: option to align all infix ops (#150) (hhugo)
  + Add: option to attempt to indent the same as ocp-indent (#162)
  + Add: option for no discretionary parens for tuples (#157) (hhugo)
  + Add: alternative format for if-then-else construct (#155) (hhugo)
  + Add: option to customize position of doc comments (#153) (hhugo)

- Bug fixes
  + Fix: dropped item attributes on module expressions
  + Fix: toplevel let%ext (#167) (hhugo)
  + Fix: parens around type alias & empty object type (#166) (hhugo)
  + Fix: missing comments for [let open] (#165) (hhugo)
  + Fix: missing comments in ppat_record (#164) (hhugo)
  + Fix: check_typ wrt constraint on module type (#163) (hhugo)
  + Fix: let binding with constraint (#160) (hhugo)
  + Fix: handle generative functor type (#152) (hhugo)

- Formatting improvements
  + Improve: remove redundant parens around application operators
  + Improve: parenthesize and break infix constructors the same as infix ops
  + Improve: consider prefix ops and `not` to be trivial if their arg is
  + Improve: align arrow type args and do not wrap them (#161)
  + Improve: formatting for multiple attributes (#154) (hhugo)
  + Improve: keep the original string escaping (#159) (hhugo)
  + Improve: discretionary parens in patterns (#151) (hhugo)
  + Improve: breaking of infix op arguments
  + Improve: consider some extensions to be "simple"
  + Improve: punning (#158) (hhugo)
  + Improve: force break of let module/open/exception/pats (#149) (hhugo)

- Build, packaging, and testing
  + Add support for bisect (#169) (hhugo)
  + Exclude failing tests from `make -C test`

### 0.5 (2018-04-17)

- Features
  + Add: support for new%js (#136) (hhugo)
  + Add: support for Ptyp_object (#104) (Sebastien Mondet)
  + Use original filename when given in error messages. (#96) (Mathieu Barbin)

- Bug fixes
  + Fix: allow extensions in types (#143) (hhugo)
  + Fix: parens on symbol type constructor
  + Fix: parenthesization of '!=' partial application as a prefix op (#126) (hhugo)
  + Fix: parens around Ppat_constraint under Pexp_match or Pexp_try (#124) (hhugo)
  + Fix: parenthesization of tuple args of variant type declarations (#122) (hhugo)
  + Fix: missing parens around list inside Constr pattern (#123) (hhugo)
  + Fix: incorrect breaking of long strings (#130) (hhugo)
  + Fix: missing parens inside array literal (#129) (hhugo)
  + Fix: attributes on arguments of function (#121) (hhugo)
  + Fix: floating docstrings within a type declaration group
  + Fix: missing parens in sugared Array.set
  + Fix: missing attributes on patterns
  + Fix: is_prefix_id for != (#112) (hhugo)
  + Fix: missing parens around module value types in signatures (#108) (Hezekiah M. Carty)
  + Fix: floating docstrings within a value binding group
  + Fix: missing attributes on extension points (#102) (Hezekiah M. Carty)
  + Fix: extensible variants with aliases (#100) (Hezekiah M. Carty)
  + Fix: several issues with extension sequence expressions
  + Fix: generative functors
  + Fix: preserve files with an empty ast (instead of failing) (#92) (Mathieu Barbin)
  + Fix: missing extension on Pexp_sequence
  + Fix: missing docstrings and attributes on types
  + Fix: missing parens around sugared Array and String operations
  + Fix: missing parens around Pexp_newtype
  + Fix: missing parens around Ppat_constraint, Ppat_or, and Ppat_unpack
  + Fix: dropped space when string wrapped between spaces
  + Fix: repeated ppx extension on mutual/recursive let-bindings (#83) (Mathieu Barbin)
  + Fix: dropped comments on Pmty_typeof
  + Fix: missing parens around Ppat_unpack under Ppat_constraint

- Formatting improvements
  + Improve: two open lines following multiline definition only with --sparse (#144)
  + Improve: indent rhs of ref update (#139) (hhugo)
  + Improve: no parens around precedence 0 infix ops (refines #115) (#141) (hhugo)
  + Improve: support (type a b c) (#142) (hhugo)
  + Improve: no parens for { !e with a } (#138) (hhugo)
  + Improve: no parens for constr inside list pattern. (#140) (hhugo)
  + Improve: generative functor applications (#137) (hhugo)
  + Improve: omit parens around lists in local opens (#134) (hhugo)
  + Prepare for ocaml#1705 (#131) (hhugo)
  + Improve: comment wrapping for dangling close
  + Improve: if-then-else conditions that break
  + Improve: suppress spurious terminal line break in wrapped strings
  + Improve: parens for nested constructors in pattern (#125) (hhugo)
  + Improve: remove duplicate parens around Ptyp_package
  + Improve: indentation after comment within record type declaration
  + Improve: add discretionary parens on nested binops with different precedence
  + Improve: empty module as functor argument (#113) (hhugo)
  + Improve: indentation of multiple attributes
  + Improve: attributes on short structure items
  + Improve: attributes on type declarations
  + Improve: tuple attribute args
  + Improve: parenthesization of Ppat_or
  + Improve: determination of file kind based on provided name
  + Improve: extension on the let at toplevel: e.g. let%expect_test _ (#94) (Mathieu Barbin)
  + Improve: constraints in punned record fields (#93) (Mathieu Barbin)
  + Improve: nullary attributes
  + Improve: Ppat_tuple under Ppat_array with unnecessary but clearer parens
  + Improve: breaking of arguments following wrapped strings

- Build, packaging, and testing
  + Simplify using `(universe)` support in jbuilder 1.0+beta20
  + Add some regtests (#135) (hhugo)
  + Upgrade to Base v0.11.0 (#103) (Jérémie Dimino)
  + Add Travis CI script
  + Fix: build [make reason] (#97) (Mathieu Barbin)
  + Simplify Makefile due to jbuilder 1.0+beta18

### 0.4 (2018-02-24)

- Features
  + Wrap lines in string literals, comments and docstrings
  + Improve char escaping to ascii / uniform hexa / utf8 (#73)
  + Add support for `Pexp_new` expressions (#76) (Sebastien Mondet)
  + Add support for `Pexp_send _` expressions (#72) (Sebastien Mondet)
  + Add options to format chars and break strings (#70) (Sebastien Mondet)
  + Formatting of %ext on if/while/for/match/try/; (#63) (Hezekiah M. Carty)
  + Disable formatting with [@@@ocamlformat.disable] (#66) (Hezekiah M. Carty)

- Formatting improvements
  + Improve sequences under if-then-else with unnecessary but safer parens
  + Improve optional arguments with type constraints
  + Improve let-bound functions with type constraints
  + Improve newtype constraints in let-bindings
  + Improve placement of exception docstrings

- Bug fixes
  + Fix missing break hint before comment on sugared `[]`
  + Fix formatting of [%ext e1]; e2 (#75) (Hezekiah M. Carty)
  + Fix missing parens around let exception, let module, for, while under apply
  + Fix missing parens under alias patterns
  + Fix placement of attributes on extension constructors
  + Fix missing parens around unpack patterns
  + Fix let-bindings with pattern constraints
  + Fix mutually recursive signatures

### 0.3 (2017-12-21)

- Features
  + Output to stdout if output file omitted

- Bug fixes
  + Fix Ppat_any value bindings
  + Fix missing parens around variant patterns in fun arg
  + Fix position of comments attached to end of sugared lists
  + Fix missing comments on module names
  + Fix package type constraints
  + Fix first-class module alias patterns
  + Fix first-class module patterns in let bindings
  + Fix missing parens around Ptyp_package under Psig_type
  + Fix missing "as" in Ptyp_alias formatting (Hezekiah M. Carty)
  + Fix let bindings with constraints under 4.06

- Formatting improvements
  + Improve line breaking of or-patterns
  + Improve placement of comments within pattern matches
  + Improve clarity of aliased or-patterns with parens
  + Improve matches on aliased or-patterns
  + Improve infix applications in limbs of if-then-else
  + Improve final function arguments following other complex arguments
  + Improve consistency of paren spacing after Pexp_fun
  + Improve sugar for Pexp_let under Pexp_extension
  + Improve sugar for newtype
  + Improve first-class module expressions
  + Improve indentation when comments are sprinkled through types
  + Do not add open line after last binding in a structure

- Build and packaging
  + Simplify build and packaging, and adopt some common practices
  + Add Warnings.Errors argument for < 4.06 compatibility (Hezekiah M. Carty)
  + Update base to v0.10.0 (Hezekiah M. Carty)

### 0.2 (2017-11-09)

- Features
  + Check fatal warnings not only in inplace mode

- Documentation
  + Improve doc of --no-warn-error
  + Mention object language not implemented
  + Update documentation of --output

- Bug fixes
  + Colon instead of arrow before type for GADT constructors with no arguments (Mehdi Bouaziz)
  + Fix some dropped comments attached to idents
  + Fix missing parens around Ppat_alias under Ppat_variant
  + Fix module type constraints on functors
  + Fix broken record field punning
  + Fix broken docstring attachment with multiple docstrings
  + Fix missing parens around application operators
  + Fix missing parens around Ppat_or under Ppat_variant
  + Fix missing/excess parens around Pexp_open under Pexp_apply/Pexp_construct
  + Fix duplicated attributes on Pexp_function
  + Fix missing parens around Ptyp_package under Pstr_type
  + Add '#' to the list of infix operator prefix (octachron)
  + Do not add space between `[` and `<` or `>` in variant types
  + Add a break hint before "constraint" in a type def (Hezekiah M. Carty)

- Formatting improvements
  + Remove unnecessary parens around Pexp_tuple under Pexp_open
  + Improve single-case matches
  + Improve constructor arguments
  + Remove unnecessary parens around match, etc. with attributes
  + Fix missing parens around constraint arg of variant type
  + Fix missing parens on left arg of infix list constructor
  + Fix missing parens around arrow type args of variant constructors
  + Fix missing parens around type of constraints on module exps

- Build and packaging
  + Separate Format patch into ocamlformat_support package
  + Fix test script
  + Unbreak build of ocamlformat_reason.ml (Marshall Roch)
  + Improve opam installation (JacquesPa)
  + Install emacs support via opam package

### 0.1 (2017-10-19)

- Initial release.
