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
