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
