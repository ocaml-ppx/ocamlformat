### unreleased

#### Bug fixes

  + Fix parsing of invalid file wrt original source handling (#1542, @hhugo)

  + Preserve the syntax of infix set/get operators (#1528, @gpetiot)
    `String.get` and similar calls used to be automatically rewritten to their corresponding infix form `.()`, that was incorrect when using the `-unsafe` compilation flag. Now the concrete syntax of these calls is preserved.

  + Add location of invalid docstring in warning messages (#1529, @gpetiot)

  + Fix comments on the same line as prev and next elements (#1556, @gpetiot)

  + Break or-patterns after comments and preserve their position at the end of line (#1555, @gpetiot)
  
  + Fix linebreak between signature items of the same group (#1560, @gpetiot)

### 0.16.0 (2020-11-16)

#### Removed

  + Remove the 'escape-chars' option, deprecated since 0.14.0 (#1462, @gpetiot)

  + Remove the 'escape-strings' option, deprecated since 0.14.0 (#1463, @gpetiot)

  + Remove the 'doc-comments-val' option, deprecated since 0.14.2 (#1461, @gpetiot)

  + Removed options are now listed in the commandline manual (new REMOVED OPTIONS section) (#1469, @Julow)

#### Changes

  + Set 'indicate-multiline-delimiters=no' on default profile (#1452, @gpetiot)

  + Option 'let-open' is now deprecated, concrete syntax will always be preserved starting from OCamlFormat v0.17.0, corresponding to the current 'let-open=preserve' behavior. (#1467, @gpetiot)

  + Warnings printed by ocamlformat itself now use the 4.12 style with symbolic names (#1511, #1518, @emillon)

  + Remove extension from executable name in error messages. On Windows, this means that messages now start with "ocamlformat: ..." instead of "ocamlformat.exe: ..." (#1531, @emillon)

  + Using tokens instead of string manipulation when inspecting the original source (#1526, #1533, #1541 @hhugo) (#1532, @gpetiot)

#### Bug fixes

  + Allow a break after `if%ext` with `if-then-else=keyword-first` (#1419, #1543, @gpetiot)

  + Fix parentheses around infix applications having attributes (#1464, @gpetiot)

  + Fix parentheses around the index arg of a non-sugared index operation (#1465, @gpetiot)

  + Preserve comment position around `match` and `try` keywords (#1458, @gpetiot)

  + Add missing break in module statement (#1431, @gpetiot)

  + Indent attributes attached to included modules better (#1468, @gpetiot)

  + Clean up `ocamlformat.el` for submission to MELPA (#1476, #1495, @bcc32)
    - Added missing package metadata to `ocamlformat.el` (#1474, @bcc32)
    - Fix `ocamlformat.el` buffer replacement for MacOS Emacs (#1481, @juxd)

  + Add missing parentheses around a pattern matching that is the left-hand part of a sequence when an attribute is attached (#1483, @gpetiot)

  + Add missing parentheses around infix operator used to build a function (#1486, @gpetiot)

  + Fix comments around desugared expression (#1487, @gpetiot)

  + Fix invalid fragment delimiters of format-invalid-files recovery mode (#1485, @hhugo)

  + Fix misalignment of cases in docked `function` match (#1498, @gpetiot)

  + Preserve short-form extensions for structure item extensions (#1502, @gpetiot)
    For example `open%ext M` will not get rewritten to `[%%ext open M]`.

  + Do not change the spaces within the code spans in docstrings (#1499, @gpetiot)

  + Comments of type constrained label in record pattern have to be relocated in 4.12 (#1517, @gpetiot)

  + Preserve functor syntax for OCaml 4.12 (#1514, @gpetiot)

  + Fix inconsistencies of the closing parentheses with indicate-multiline-delimiters (#1377, #1540, @gpetiot)

  + Fix position of comments around list constructor (::) (#1524, @gpetiot)

  + Fix comments position in extensions (#1525, @gpetiot)

  + Fix formatting of field override with constraint (#1544, @gpetiot)

#### New features

### 0.15.1 (2020-11-02)

#### Internal

  + Use ppxlib instead of ocaml-migrate-parsetree 1.x. (#1482, @emillon)
    * No functional changes are expected.
    * Cherry picked commits: 219dc1e3a4614041e1bc5428d003c0af4e, 9e453b0ef87124e33827ee2423289deef8, 7ad1e575ffa4ce3022c71daba39954d3b9, eb49db6772a9adabe611982000465d0ad7, dc79052a085950cd88fdef0843f665a029, c06c544e21bd65b726cde8fee0f78a6248, ce94d2fa50ff276b5782070375a0b30ba1

### 0.15.0 (2020-08-06)

#### Changes

  + Do not break inline elements such as `{i blah}` in docstrings (#1346, @jberdine)

  + Distinguish hash-getter from hash-comparison infix operators. Operators of the form `#**#` or `#**.` where `**` can be 0 or more operator chars are considered getter operators and are not surrounded by spaces, as opposed to regular infix operators (#1376, @gpetiot)

  + Type constraint on return type of functions is now always printed before the function body (#1381, #1397, @gpetiot)

#### Bug fixes

  + Restore previous functionality for pre-post extension points (#1342, @jberdine)

  + Fix extra break before `function` body of a `fun` (#1343, @jberdine)
    Indent further args of anonymous functions (#1440, @gpetiot)

  + Do not clear the emacs `*compilation*` buffer on successful reformat (#1350, @jberdine)

  + Fix disabling with attributes on OCaml < 4.08 (#1322, @emillon)

  + Preserve unwrapped comments by not adding artificial breaks when `wrap-comments=false` and `ocp-indent-compat=true` are set to avoid interfering with ocp-indent indentation. (#1352, @gpetiot)

  + Break long literal strings at the margin (#1367, @gpetiot)

  + Break after a multiline argument in an argument list (#1360, @gpetiot)

  + Remove unnecessary parens around object (#1379, @gpetiot)

  + Fix placement of comments on constants (#1383, @gpetiot)

  + Do not escape arguments of some Odoc tags (#1391, 1408, @gpetiot, @Julow)
    The characters `[]{}` must not be escaped in the arguments of `@raise`, `@author`, `@version` and others.

  + Fix missing open line between multi-line let-binding with poly-typexpr (#1372, @jberdine)

  + Remove trailing space after expression when followed by an attribute and break before attributes attached to multi-line phrases (#1382, @gpetiot)

  + Do not add a space to minimal comments `(* *)`, `(** *)` and `(*$ *)` (#1407, @gpetiot)

  + Fix attributes position in labelled arguments type (#1434, @gpetiot)

  + Add missing parens around type annotation in anonymous function (#1433, @gpetiot)

  + Fix alignment of 'then' keyword in parenthesised expression (#1421, @gpetiot)

#### New features

  + Support quoted extensions (added in ocaml 4.11) (#1405, @gpetiot)

  + Recognise eliom file extensions (#1430, @jrochel)

### 0.14.3 (2020-07-22)

#### Changes

  + No functional changes from 0.14.2. The goal of this release is to be
    compatible with base and stdio v0.14.0.

  + Backport the following PRs:
    - #1386 - Update opam metadata
    - #1396 - Add compatibility with base.v0.14.0
    - #1399 - Allow stdio.v0.14

### 0.14.2 (2020-05-11)

#### Changes

  + Merge `doc-comments-val` option with `doc-comments`. The placement of documentation comments on `val` and `external` items is now controled by `doc-comments`.

    - `doc-comments=after` becomes `doc-comments=after-when-possible` to take into account the technical limitations of ocamlformat;
    - `doc-comments=before` is unchanged;
    - `doc-comments-val` is now replaced with `doc-comments`
      To reproduce the former behaviors
      * `doc-comments=before` + `doc-comments-val=before`: now use `doc-comments=before`;
      * `doc-comments=before` + `doc-comments-val=after`: now use `doc-comments=before-except-val`;
      * `doc-comments=after` + `doc-comments-val=before`: this behavior did not make much sense and is not available anymore;
      * `doc-comments=after` + `doc-comments-val=after`: now use `doc-comments=after-when-possible`.

   (#1358) (Josh Berdine, Jules Aguillon, Guillaume Petiot)

   This reverts changes introduced in 0.14.1 (#1335) and 0.14.0 (#1012).

### 0.14.1 (2020-04-14)

#### Changes

  + The default for `doc-comments` is changed to `after` (#1335) (Jules Aguillon)
    This reverts a change introduced in 0.14.0 (#1012).

  + Revert deprecation of the `doc-comments` option (#1331) (Jules Aguillon)
    This reverts a change introduced in 0.14.0 (#1293).

### 0.14.0 (2020-04-02)

#### New features

  + Add an option `--format-invalid-files` to print unparsable parts of the input as verbatim text. This feature is still experimental. (#1026) (Guillaume Petiot)

  + Support multi-indices extended indexing operators (#1279, #1277) (Jules Aguillon, Guillaume Petiot)
    This feature has been added in OCaml 4.10.0

  + Handle OCaml 4.10.0 AST (#1276) (Guillaume Petiot)

  + Preserve functor syntax for consistency (#1312) (Guillaume Petiot)
    Previously both functor syntax: `module M = functor (K : S) -> struct end` and `module M (K : S) = struct end` would be formatted as the latter, the original syntax is now preserved.

#### Changes

  + Add the option `doc-comments-val=before|after` (#1012) (Jules Aguillon)
    This option set the placement of documentation comment on `val` and `external` only.
    It is set to `after` by default.

  + The default for `doc-comments` is changed from `after` to `before` (#1012, #1325) (Jules Aguillon)
    This affects both `conventional` (default) and `ocamlformat` profiles.

  + Some options are now deprecated:
    * `doc-comments` (#1293, #1012)
      This option depends on a flawed heuristic.
      It is replaced by `doc-comments-val` for `val` and `external` declarations.
      There is no equivalent to this option in the general case.
    * `escape-chars`, `escape-strings` and `extension-sugar` (#1293)
      These options are rarely used and their default behavior is considered to be the right behavior.

  + Add space between `row_field` attributes and the label or arguments, to be
    consistent with the non-polymorphic case. (#1299) (Craig Ferguson)

#### Bug fixes

  + Fix missing parentheses around `let open` (#1229) (Jules Aguillon)
    eg. `M.f (M.(x) [@attr])` would be formatted to `M.f M.(x) [@attr]`, which would crash OCamlformat

  + Remove unecessary parentheses with attributes in some structure items:
    * extensions and eval items (#1230) (Jules Aguillon)
      eg. the expression `[%ext (() [@attr])]` or the structure item `(() [@attr]) ;;`
    * `let _ = ...`  constructs (#1244) (Etienne Millon)

  + Fix some bugs related to comments:
    * after a function on the rhs of an infix (#1231) (Jules Aguillon)
      eg. the comment in `(x >>= fun y -> y (* A *))` would be dropped
    * in module unpack (#1309) (Jules Aguillon)
      eg. in the module expression `module M = (val x : S (* A *))`

  + Fix formatting of empty signature payload `[%a:]` (#1236) (Etienne Millon)

  + Fix parenthesizing when accessing field of construct application (#1247) (Guillaume Petiot)

  + Fix formatting of attributes on object overrides `{< >}` (#1238) (Etienne
    Millon)

  + Fix attributes on coercion (#1239) (Etienne Millon)

  + Fix formatting of attributes on packed modules (#1243) (Etienne Millon)

  + Fix parens around binop operations with attributes (#1252, #1306) (Guillaume Petiot, Craig Ferguson)

  + Remove unecessary parentheses in the argument of indexing operators (#1280) (Jules Aguillon)

  + Retain attributes on various AST nodes:
    * field set expressions, e.g. `(a.x <- b) [@a]` (#1284) (Craig Ferguson)
    * instance variable set expressions, e.g. `(a <- b) [@a]` (#1288) (Craig Ferguson)
    * indexing operators, e.g. `(a.(b)) [@a]` (#1300) (Craig Ferguson)
    * sequences, e.g. `(a; b) [@a]` (#1291) (Craig Ferguson)

  + Avoid unnecessary spacing after object types inside records and polymorphic variants,
    e.g. `{foo : < .. > [@a]}` and `{ foo : < .. > }` (#1296) (Craig Ferguson)

  + Fix missing parentheses around tuples with attributes. (#1301) (Craig Ferguson)
    Previously, `f ((0, 0) [@a])` would be formatted to `f (0, 0) [@a]`, crashing OCamlformat.

  + Avoid emitting `>]` when an object type is contained in an extension point
    or attribute payload (#1298) (Craig Ferguson)

  + Fix crash on the expression `(0).*(0)` (#1304) (Jules Aguillon)
    It was formatting to `0.*(0)` which parses as an other expression.

  + Preserve empty doc-comments syntax. (#1311) (Guillaume Petiot)
    Previously `(**)` would be formatted to `(***)`.

  + Do not crash when a comment contains just a newline (#1290) (Etienne Millon)

  + Handle lazy patterns as arguments to `class` (#1289) (Etienne Millon)

  + Preserve cinaps comments containing unparsable code (#1303) (Jules Aguillon)
    Previously, OCamlformat would fallback to the "wrapping" logic, making the comment
    unreadable and crashing in some cases.

  + Fix normalization of attributes, fixing the docstrings in attributes (#1314) (Guillaume Petiot)

  + Add missing parentheses around OR-patterns with attributes (#1317) (Guillaume Petiot)

  + Fix spacing inside parens for symbols when the spacing was handled by the englobing exp (#1316) (Guillaume Petiot)

  + Fix invalid (unparsable) docstrings (#1315) (Guillaume Petiot)
    When parsing a comment raises an error in odoc, it is printed as-is.

  + Fix parenthesizing of optional arguments rebound to non-variables, e.g. `let
    f ?a:(A) = ()` rather than the unparsable `let f ?a:A = ()` (#1305) (Craig Ferguson)

### 0.13.0 (2020-01-28)

#### New features

  + Add an option `--margin-check` to emit a warning if the formatted output exceeds the margin (#1110) (Guillaume Petiot)
  + Preserve comment indentation when `wrap-comments` is unset (#1138, #1159) (Jules Aguillon)
  + Improve error messages (#1147) (Jules Aguillon)
  + Display standard output in the emacs plugin even when ocamlformat does not fail (#1189) (Guillaume Petiot)

#### Removed

  + Remove `ocamlformat_reason` (#254, #1185) (Etienne Millon).
    This tool has never been released to opam, has no known users, and overlaps
    with what `refmt` can do.
  + Remove `ocamlformat-diff` (#1205) (Guillaume Petiot)
    This tool has never been released to opam, has no known users, and overlaps
    with what `merge-fmt` can do.

#### Packaging

  + Work with base v0.13.0 (#1163) (Jules Aguillon)

#### Bug fixes

  + Fix placement of comments just before a '|' (#1203) (Jules Aguillon)
  + Fix build version detection when building in the absence of a git root (#1198) (Anil Madhavapeddy)
  + Fix wrapping of or-patterns in presence of comments with `break-cases=fit` (#1167) (Jules Aguillon)
    This also fixes an unstable comment bug in or-patterns
  + Fix an unstable comment bug in variant declarations (#1108) (Jules Aguillon)
  + Fix: break multiline comments (#1122) (Guillaume Petiot)
  + Fix: types on named arguments were wrapped incorrectly when preceding comments (#1124) (Guillaume Petiot)
  + Fix the indentation produced by max-indent (#1118) (Guillaume Petiot)
  + Fix break after Psig_include depending on presence of docstring (#1125) (Guillaume Petiot)
  + Remove some calls to if_newline and break_unless_newline and fix break before closing brackets (#1168) (Guillaume Petiot)
  + Fix unstable cmt in or-pattern (#1173) (Guillaume Petiot)
  + Fix location of comment attached to the underscore of an open record (#1208) (Guillaume Petiot)
  + Fix parentheses around optional module parameter (#1212) (Christian Barcenas)
  + Fix grouping of horizontally aligned comments (#1209) (Guillaume Petiot)
  + Fix dropped comments around module pack expressions (#1214) (Jules Aguillon)
  + Fix regression of comment position in list patterns (#1141) (Josh Berdine)
  + Fix: adjust definition of Location.is_single_line to reflect margin (#1102) (Josh Berdine)

#### Documentation

  + Fix documentation of option `version-check` (#1135) (Wilfred Hughes)
  + Fix hint when using `break-separators=after-and-docked` (#1130) (Greta Yorsh)

### 0.12 (2019-11-04)

#### Changes

  + Set "conventional" as the default profile (#1060) (Guillaume Petiot)
    This new profile is made to better match the most used style and is encouraged.
    To continue using the previous default, use `profile = ocamlformat` in your `.ocamlformat`.
  + CLI: Allow both values of boolean options (#1062) (Jules Aguillon)
    Now, both `--opt` and --no-opt` are available on the CLI for any boolean option "opt".
    Previously, only one of them were available depending on the default value.
  + Auto mode for `break-string-literals` (#1057) (Guillaume Petiot)
    `wrap`, `newlines` and `newlines-and-wrap` values of `break-string-literals` are removed.
    `auto` replaces them, it is equivalent to `newlines-and-wrap`.
  + Dock collection brackets (#1014) (Guillaume Petiot)
    `after-and-docked` value of `break-separators` is removed and is replaced by a new `dock-collection-brackets` option.
  + Preserve `begin` and `end` keywords in if-then-else (#978) (Jules Aguillon)
    Previously, `begin`/`end` keywords around if-then-else branches were turned into parentheses.

#### New features

  + Give a hint when warning 50 is raised (#1111) (Guillaume Petiot)
  + Add a message when a config value is removed (#1089) (Etienne Millon)
    Explain what replaces removed options and avoid printing a parsing error.
  + Implement `sequence-blank-line=preserve-one` for let bindings (#1077) (Jules Aguillon)
    Preserve a blank line after `let .. in` when `sequence-blank-line` set to `preserve-one`.
    Previously, only blank lines after `;` could be preserved.
  + Parse toplevel directives (#1020) (Jules Aguillon)
    Allow `#directives` in `.ml` files.
    Previously, files containing a directive needed to be parsed as "use file".
    The "use file" mode is removed and `--use-file` is now the same as `--impl`.
  + Don't require `--name`, require kind, forbid `--inplace`, allow `--check`, make `--enable-outside-detected-project` implicit when reading from stdin (#1018) (Guillaume Petiot)
  + Parse code in docstrings (#941) (Guillaume Petiot)
    Format OCaml code in cinaps-style comments `(*$ code *)` and code blocks in documentation comments `(** {[ code ]} *)`.
  + Parse documentation comments with Odoc (#721) (Jules Aguillon)
    Formatting of documentation comments is more robust and support newer Odoc syntaxes.
    Internally, Odoc replaces Octavius as the documentation parser.

#### Bug fixes

  + Fix unstabilizing comments on assignments (#1093) (Guillaume Petiot)
  + Fix the default value documentation for `max-indent` (#1105) (Guillaume Petiot)
  + Fix closing parenthesis exceeding the margin in function application (#1098) (Jules Aguillon)
  + Missing break before attributes of `Pmty_with` (#1103) (Josh Berdine)
  + Fix closing quote exceeding the margin (#1096) (Jules Aguillon)
  + Fix break before the closing bracket of collections (exceeding the margin) (#1073) (Guillaume Petiot)
  + Fix precedence of Dot wrt Hash (#1058) (Guillaume Petiot)
  + Fix break in variant type definition to not exceed the margin (#1064) (Guillaume Petiot)
  + Fix newlines and indentation in toplevel extension points (#1054) (Guillaume Petiot)
  + Fix placement of doc comments around extensions (#1052) (Jules Aguillon)
  + Inline extensions that do not break (#1050) (Guillaume Petiot)
  + Add missing cut before attributes in type declarations (#1051) (Guillaume Petiot)
  + Fix alignment of cases (#1046) (Guillaume Petiot)
  + Fix blank line after comments at the end of lists (#1045) (Guillaume Petiot)
  + Fix indexing operators precedence (#1039) (Jules Aguillon)
  + Fix dropped comment after infix op (#1030) (Guillaume Petiot)
  + No newline if the input is empty (#1031) (Guillaume Petiot)
  + Fix unstable comments around attributes (#1029) (Guillaume Petiot)
  + Fix extra blank line in sequence (#1021) (Jules Aguillon)
  + Check functor arguments when computing placement of doc comments (#1013) (Jules Aguillon)
  + Fix indentation of labelled args (#1006) (Guillaume Petiot)
  + Fix linebreak between or-cases with comments when `break-cases=all` (#1002) (Guillaume Petiot)
  + Fix unstable unattached doc comment in records (#998) (Jules Aguillon)
  + Fix string literal changed (#995) (Jules Aguillon)
  + Fix type variable (#996) (Jules Aguillon)
  + Fix crash on extension sequence (#992) (Guillaume Petiot)
  + Fix position of expressions regarding of comments in infix-op expressions (#986) (Guillaume Petiot)
  + Escape special characters in external declaration (#988) (Jules Aguillon)
  + Fix parens around constrained expr with attrs (#987) (Guillaume Petiot)
  + Fix the margin, and correctly breaks comments (#957) (Guillaume Petiot)
  + Fix formatting of custom indexing operators (#975) (Guillaume Petiot)
  + Fix position of comments of labelled arrow types (#976) (Guillaume Petiot)
  + No box around inline odoc styles (#971) (Guillaume Petiot)
  + Fix boxing of collection expressions/patterns (#960) (Guillaume Petiot)
  + Fix crash on record expr with pack fields (#963) (Jules Aguillon)
  + Fix letop in subexpr (#956) (hhugo)

#### Internal

  + Take file kind from --name when formatting stdin (#1119) (Jules Aguillon)
  + Make Fmt.t abstract (#1109) (Jules Aguillon)
  + Future-proof Fmt API in case Fmt.t goes abstract (#1106) (Etienne Millon)
  + Future-proof `Fmt` API in case `Fmt.t` goes abstract (#1106) (Etienne Millon)
  + Optional names for formatting boxes in debug output (#1083) (Guillaume Petiot)
  + Check ocamlformat error codes in the testsuite (#1084) (Etienne Millon)
  + Clean `Translation_unit` (#1078) (Guillaume Petiot)
  + Use dune file generation in test/passing/dune (#1082) (Etienne Millon)
  + CI: factorize tests and check reason build (#1079) (Guillaume Petiot)
  + Use short form for action in src/dune (#1076) (Etienne Millon)
  + Cleanup `sequence_blank_line` (#1075) (Jules Aguillon)
  + CI: use a script travis-ci.sh to simplify .travis.yml (#1063) (Guillaume Petiot)
  + Remove utility functions from `Fmt_ast` (#1059) (Guillaume Petiot)
  + CI: use opam-2.0.5 in Travis (#1044) (Anton Kochkov)
  + CI: check the build with OCaml 4.07.1 and 4.08.0 (#1036) (Jules Aguillon)
  + Use the same sets of options for both branches by default in `test_branch.sh` (#1033) (Guillaume Petiot)
  + Fix `test_branch.sh` and CI checking of CHANGES.md (#1032, #1034) (Jules Aguillon)
  + Fix flag of git-worktree in `test_branch.sh` and `bisect.sh` (#1027) (Guillaume Petiot)
  + Remove the `bisect_ppx` dependency and clean the `Makefile` (#1005) (Jules Aguillon)
  + Use a `CHANGES.md` log file again (#1023) (Guillaume Petiot)
  + Support OCaml 4.09.0 (add the odoc.1.4.2 dependency) (#1024) (Guillaume Petiot)
  + Update labels of issue templates (#1017) (Guillaume Petiot)
  + Update labels in `CONTRIBUTING.md` (#1007) (Guillaume Petiot)
  + Allow to ignore invalid options (#984) (hhugo)
    The `--ignore-invalid-option` flag is added to ignore invalid options in `.ocamlformat` files.
  + Improve the documentation of `--doc-comments` (#982) (Jules Aguillon)
  + Remove symbolic links and change naming convention of tests (#980) (Guillaume Petiot)
  + Change the type of `fmt_code` (#974) (Guillaume Petiot)
  + Simplify `Makefile` (#973) (hhugo)
  + Dune should not be flagged as a build dep anymore (#954) (Guillaume Petiot)

### 0.11 (2019-08-07)

  + Improve: generalize API of Config_option (#952) (Guillaume Petiot)
  + Improve: new 'before' value for option 'sequence-style' (#947) (Guillaume Petiot)
  + Project: create issue templates (#950) (Guillaume Petiot)
  + Improve: tidying up Conf.ml (#951) (Guillaume Petiot)
  + Improve: parse code in comments (#934) (Guillaume Petiot)
  + Fix comments' placement (do not look at loc_stack) (#923) (Guillaume Petiot)
  + Doc: setting flags in .ocamlformat (#944) (Guillaume Petiot)
  + Doc: enable-outside-detected-project necessary for global conf file (#948) (Guillaume Petiot)
  + Fix hashbang handling (#946) (hhugo)
  + Improve: support Shell-style regular expressions in .ocamlformat-ignore and .ocamlformat-enable files (#937) (Guillaume Petiot)
  + Improve: force break after an infix op only if followed by another one (#935) (Guillaume Petiot)
  + Fix break-separators=after-and-docked for lists and arrays (#931) (Guillaume Petiot)
  + Improve: deprecate option break-string-literals and change its default value (#932) (Guillaume Petiot)
  + Improve: break with labeled arrow type (#933) (Guillaume Petiot)
  + Improve: disambiguate non-breaking matching structures (#857) (Guillaume Petiot)
  + Improve: warning 50 handled like an internal error (#930) (Guillaume Petiot)
  + Fix break-separators=after-and-docked for record patterns (#929) (Guillaume Petiot)
  + Fix closing parenthesis indentation when on separate line (#928) (Guillaume Petiot)
  + Improve: split the Conf.ml file (#920) (Guillaume Petiot)
  + Fix position of comments after anonymous functions (#919) (Guillaume Petiot)
  + Fix: comments around disabled block (#918) (hhugo)
  + Fix monadic bindings (new 4.08 syntax) (#911) (Guillaume Petiot)
  + Fix attribute when break-infix-before-func=false (#916) (Guillaume Petiot)
  + Improve: update ocamlformat_reason opam file to 2.0 format (#913) (Anil Madhavapeddy)
  + Fix attributes of modules (#910) (Guillaume Petiot)
  + Fix docstrings of exceptions (#909) (Guillaume Petiot)
  + Fix attribute location in Normalization (#908) (Guillaume Petiot)
  + Improve: add the 'ocamlformat-file-kind' argument to the emacs hook (#905) (Guillaume Petiot)
  + Improve: dunify testsuite (#881) (Thomas Refis)
  + Improve: add trailing semicolon inside record when break-separators=after-and-docked (#899) (Guillaume Petiot)
  + Fix compilation with 4.06 and 4.07 (#898) (Guillaume Petiot)
  + Improve: add a new way to indicate multiline delimiters (#876) (Thomas Refis)
  + Fix inconsistency of break-separators=after-and-docked for record expressions (#856) (Guillaume Petiot)

### 0.10 (2019-06-25)

  + Improve: align cases horizontally (#883) (Guillaume Petiot)
  + Improve: option exp-grouping (#828) (Guillaume Petiot)
  + Improve: synchronize Format with upstream stdlib (#885) (Guillaume Petiot)
  + Improve: break-string-literals=newlines-and-wrap (#896) (Guillaume Petiot)
  + Improve: specify break hint in fits_breaks (#894) (Guillaume Petiot)
  + Improve: option break-before-in (#892) (Guillaume Petiot)
  + Fix break-string-literals=newlines (#887) (Guillaume Petiot)
  + Improve: Implement break-fun-sig without Location.is_single_line (#886) (Jules Aguillon)
  + Format gen_version.ml (#893) (hhugo)
  + Improve: switch to ast 4.08 (#831) (hhugo)
  + Fix formatting of arguments when break-fun-decl=fit-or-vertical (#884) (Guillaume Petiot)
  + Test: extend max_indent test (#878) (Thomas Refis)
  + Test: break_cases_normal_indent.ml is a symlink on break_cases_fit.ml (#879) (Guillaume Petiot)
  + Improve unicode text length computation (#816) (Guillaume Petiot)
  + Add an option to control the indentation of nested matches (#870) (Thomas Refis)
  + Fix: properly interpret indicate-multiline-delimiters for if-then-elses (#874) (Thomas Refis)
  + Enable warning 9 (#875) (hhugo)
  + Fix unstable comment in let%ext (#873) (Guillaume Petiot)
  + Improve: option max-indent (#841) (Guillaume Petiot)
  + Improve: option nested-match=align (#827) (Guillaume Petiot)
  + Fix dropped attributes in with_constraints (#846) (Guillaume Petiot)
  + Fix dropped comments in list patterns and module types  (#866) (Guillaume Petiot)
  + Fix comment dropped in object (#849) (Guillaume Petiot)
  + Fix inconsistency of break-separators for wildcards in match cases (#855) (Guillaume Petiot)
  + Improve: new options to support 'with' and 'strict_with' (ocp-indent) (#853) (Guillaume Petiot)
  + Improve: .ocamlformat-enable files listing files to format when ocamlformat is disabled (#854) (Guillaume Petiot)
  + Check that all locations have been considered during formatting (#864) (hhugo)
  + clean Hashtbl.Poly (#862) (hhugo)
  + Fix: test.sh (#858) (hhugo)
  + cleanup Cmts.ml (#861) (hhugo)
  + Clean: Cleanup usage of Poly (#860) (hhugo)
  + Fix: rename sexp_list into list (#859) (hhugo)
  + Fix vim instructions (#852) (Marcin Jekot)
  + Improve: options extension-indent and stritem-extension-indent (#840) (Guillaume Petiot)
  + Fix comment dropped in field alias (#848) (Guillaume Petiot)
  + Fix pro position for with_constraints (#847) (Guillaume Petiot)
  + Improve: finer space-around-exp options (#837) (Guillaume Petiot)
  + Improve: preserve blank lines in conventional and sparse profiles (#838) (Guillaume Petiot)
  + Improve: don't fit tag-only comments after val declarations (#836) (Jules Aguillon)
  + Improve speed with ofday_unit_tests_v1.ml (#833) (hhugo)
  + Fix exception when calling String.sub (#832) (Guillaume Petiot)
  + Improve: implement doc-comments and doc-comments-tag-only for every items (#746) (Jules Aguillon)
  + Improve: Add field-space=tight-decl (#829) (Jules Aguillon)
  + Improve: make Sugar.list_exp and Sugar.list_pat tail-recursive (#823) (Guillaume Petiot)
  + Improve: options 'let-binding-indent', 'type-decl-indent' and 'indent-after-in' (#822) (Guillaume Petiot)
  + Fix: performance issue with deep asts (#826) (hhugo)
  + Improve: preserve blank lines in sequences (#814) (Guillaume Petiot)
  + Improve: tidying Fmt_ast.ml (#821) (Guillaume Petiot)
  + Improve: space before type constraint in record (#819) (Guillaume Petiot)
  + Improve: break-cases=fit-or-vertical (#820) (Guillaume Petiot)
  + Improve: remove break before ending paren for anonymous functions (#818) (Guillaume Petiot)
  + Improve: preserve the position of type annotation in bindings (#815) (Guillaume Petiot)
  + Improve: preserve record type annot (#812) (Guillaume Petiot)
  + Fix break before ending paren (#801) (Guillaume Petiot)
  + Improve: better consistency between structures and signatures (#803) (Guillaume Petiot)
  + Fix let module sparse (sparse mode only for module applications) (#809) (Guillaume Petiot)
  + Improve: change formatting of newtypes (#811) (Guillaume Petiot)
  + Improve: break-cases-all shouldn't break nested patterns (#810) (Guillaume Petiot)
  + Fix: sugarized extensions (#805) (Guillaume Petiot)
  + Improve: tidying Fmt_ast (#808) (Guillaume Petiot)
  + Fix cmt in empty structure (#804) (Guillaume Petiot)
  + Remove dead link to preset profiles (#806) (Andrew Schwartzmeyer)
  + Improve: break with type constraints (#797) (Guillaume Petiot)
  + Fix colon break module type functor (#802) (Guillaume Petiot)
  + Improve: K&R style for if-then-else (#787) (Guillaume Petiot)
  + Improve: new option break-fun-sig (#785) (Guillaume Petiot)
  + Improve: indentation consistency of '<-' and `:=` (#780) (Guillaume Petiot)
  + Fix: functor application and break-struct wrap incorrectly (#786) (Guillaume Petiot)
  + Break after anonymous function arrow after infix op (#781) (Guillaume Petiot)
  + Fix: type extension (#782) (Guillaume Petiot)
  + Improve: Fmt.noop (#784) (Guillaume Petiot)
  + Fix extension of value binding (#779) (chrismamo1)
  + Improve: less sensitivity to concrete syntax (#767) (Guillaume Petiot)
  + Fix missing space before attribute on includes (#775) (Jules Aguillon)
  + Improve: new option let-module (#768) (Guillaume Petiot)
  + Improve: --disable-outside-detected-project is set by default (#761) (Guillaume Petiot)
  + Fix weird parens break (#751) (Guillaume Petiot)
  + Fix: if $XDG_CONFIG_HOME is either not set or empty, use $HOME/.config (#758) (Guillaume Petiot)
  + Fix: --use-file/--impl/--intf should override file extension (#774) (Guillaume Petiot)
  + Improve: less breaks for break-cases=all but correctly breaks or-patterns (#762) (Guillaume Petiot)
  + Remove unecessary break on module pack constraints with with-constraints (#739) (Jules Aguillon)
  + Fix inconsistent break before module signature (#755) (Guillaume Petiot)
  + Fix indentation of functor argument (#773) (Guillaume Petiot)
  + Tidying fmt ast (#748) (Guillaume Petiot)
  + Fix nested parens with no break infix before func (#760) (Guillaume Petiot)
  + Provide an mli for Compat (#772) (hhugo)
  + Fix non-wrapping asterisk prefixed cmts (#759) (Guillaume Petiot)
  + Support for OCaml 4.08 (#763) (hhugo)
  + Fix module type functor (#716) (Guillaume Petiot)
  + Small cleanup (#764) (hhugo)
  + Fix: update ocamlformat-help.txt (follow up on #752) (#756) (Guillaume Petiot)
  + Fix module pack and functor (#735) (juloo)
  + Fix grammar: it's -> its (Antonio Nuno Monteiro)
  + Improve: support --name with --inplace (#740) (Josh Berdine)
  + Fix: dropped comments for pexp_record (#743) (hhugo)
  + Improve: comments arround attributes, fix #726 (#742) (hhugo)
  + Update README for new profiles (#738) (Josh Berdine)
  + Remove deprecated 'default' profile (#736) (Josh Berdine)
  + Fix extra parens around ext match (#733) (Guillaume Petiot)
  + Improve: factorize with compose_module (#729) (Guillaume Petiot)
  + Test: exclude gen_version.ml from test (#732) (Josh Berdine)
  + Improve: make gen_version an ocaml script (#664) (hhugo)

### 0.9.1 (2019-06-24)

  + Small cleanup (#764) (hhugo)
  + Support for OCaml 4.08 (#763) (hhugo)

### 0.9 (2019-03-28)

  + Admin: remove CHANGES.md that was essentially git log (Josh Berdine)
  + Admin: simplify release procedure (Josh Berdine)
  + Build: fix ocaml version constraint, need 4.06 (Josh Berdine)
  + Improve: make gen_version an ocaml script (Hugo Heuzard)
  + Improve: fix associativity of Pexp_setfield (#725) (Josh Berdine)
  + Improve: normalize setfield and setinstvar (#720) (Guillaume Petiot)
  + Remove: deprecated config file syntax parsing (#715) (Josh Berdine)
  + Improve: put the equal first for ocp-indent-compat (#717) (Guillaume Petiot)
  + Fix: parse docstrings once (#713) (Guillaume Petiot)
  + Improve: new profiles conventional and ocamlformat (#663) (Guillaume Petiot)
  + Revert module indentation (#714) (Guillaume Petiot)
  + Fix infix wrap (#691) (Guillaume Petiot)
  + Fix doc comments tag only when docstring parsing disabled (#711) (Guillaume Petiot)
  + Fix missing space before closing paren around function (#705) (Josh Berdine)
  + Fix documentation of doc-comments-tag-only (#710) (Guillaume Petiot)
  + Improve: module-item-spacing=preserve (#538) (Guillaume Petiot)
  + Add a space in "Jane Street" (#703) (Kevin Ji)
  + Fix js_source.ml (#702) (Guillaume Petiot)
  + Fix space-around-collection-expressions for record/variant definitions (#670) (juloo)
  + Fix extra space ifthenelse (#700) (Guillaume Petiot)
  + Improve split attribute in let binding for expect test with uncaught exn (#681) (Guillaume Petiot)
  + Fix empty newline before equal (#701) (Guillaume Petiot)
  + Fix double cmts (#678) (Guillaume Petiot)
  + Fix value binding ocp indent compat (#694) (Guillaume Petiot)
  + Fix ast changed when record ident constrained (#697) (Guillaume Petiot)
  + Fix incorrect ocaml code (#698) (Guillaume Petiot)
  + Fix fmt for CI (#693) (Guillaume Petiot)
  + Fix record break (#689) (Guillaume Petiot)
  + Fix break before parens no wrap fun args (#690) (Guillaume Petiot)
  + Improve: disable conf in files and attributes (#684) (Guillaume Petiot)
  + Fix space around tuples (#679) (Guillaume Petiot)
  + Improve: break before in for let-module construct, because of ocp-indent (#685) (Guillaume Petiot)
  + Improve debugging output (#677) (hhugo)
  + Improve: group open/close of modules and fix indentation (#665) (Guillaume Petiot)
  + Fix constrained match in record (#676) (Guillaume Petiot)
  + Fix: formatting of end line comments (#662) (Guillaume Petiot)
  + Fix cmt in fun when no break infix (#668) (Guillaume Petiot)
  + Add the wrap-fun-decl option (#645) (juloo)
  + Improve: break the list of 'with type' module constraints (#639) (Guillaume Petiot)
  + Reduce the use of Poly comparisons (#661) (hhugo)
  + Improve: check flag to check whether the input files already are formatted (#657) (Guillaume Petiot)
  + Fix cmt placement infix op (#651) (Guillaume Petiot)
  + Restore compat with base.v0.11 (Hugo Heuzard)
  + Fix: disallow '-' with other inputs (#658) (hhugo)
  + Fix build on OCaml 4.06.1 (#646) (juloo)
  + Fix comments on record fields (#650) (juloo)
  + Fix cmts in list (#654) (Guillaume Petiot)
  + Improve: If-then-else = fit-or-vertical mode (#603) (Guillaume Petiot)
  + Link to man page from readme (#648) (Yawar Amin)
  + Fix indent match branches with cmts (#644) (Guillaume Petiot)
  + Build: update to base v0.12 (#642) (Josh Berdine)
  + Fit tag-only doc comments (#637) (juloo)
  + Fix try%lwt indent (#638) (Guillaume Petiot)
  + Fix type manifest formatting (#616) (Guillaume Petiot)
  + Fix: don't include ocamlformat_diff in ocamlformat (#636) (Louis Roché)
  + fix emacs setup (#631) (Louis Roché)
  + tools/update_tests.sh --all (#632) (juloo)
  + Fix: don't break line before wrapping comment (#634) (Guillaume Petiot)
  + Fix ignored ocamlformat attribute (#615) (Guillaume Petiot)
  + Include jsoo in the tests (#618) (hhugo)
  + Fix missing break before comment (#613) (Guillaume Petiot)
  + Do not rely on the file-system to format sources (#611) (hhugo)
  + Ignore file in .ocamlformat-ignore (#606) (hhugo)
  + Improve reason support (#608) (hhugo)
  + Fix: fix fmt_ast wrt strings and chars when sources are not available (#607) (hhugo)
  + Fix ocamlformat_reason (#604) (hhugo)
  + Fix missing break for local open record patterns (#602) (Guillaume Petiot)
  + Fix regression for variants with docstrings (#601) (Guillaume Petiot)
  + Fix extra break in module pack type (#600) (juloo)
  + Add the doc-comments-padding option (#575) (juloo)
  + Improve: externalize Sugar functions from Fmt_ast.ml (#593) (Guillaume Petiot)
  + Fix typedecl attribute (#595) (Guillaume Petiot)
  + Improve: less linebreaks for break-cases=fit (#536) (Guillaume Petiot)
  + fix 590 (#594) (hhugo)
  + Make gen_version.sh use bash. (#592) (hhugo)
  + Implement box debugging (#574) (juloo)
  + Break closing bracket in polymorphic variants (#583) (juloo)
  + Break comment record (#580) (juloo)
  + missing headers (Hugo Heuzard)
  + Improve: mishandling of field_space in record exps and patterns (#587) (Josh Berdine)
  + Add empty mli for executable (#591) (hhugo)
  + tests: test ocamlformat when disabled (Hugo Heuzard)
  + dont reformat if disabled (Hugo Heuzard)
  + remove global ref in Transation_unit (Hugo Heuzard)
  + Fix Emacs (>26.1) temporary buffer not killed (#567) (Ludwig PACIFICI)
  + Improve: opam file for ocamlformat_diff to remove the bos dependency (#579) (Guillaume Petiot)
  + Fix: Require Octavius version 1.2.0 (#576) (juloo)
  + Improve: record fields with type constraints (#565) (Josh Berdine)
  + Fix: comments attachment (#548) (Guillaume Petiot)
  + Improve: parens around constrained any-pattern (#431) (Guillaume Petiot)
  + Revise formatting of compact single case matches (#552) (Josh Berdine)
  + Fix typo in help text (#553) (Wilfred Hughes)
  + Improve: calculate length of comment strings using UTF8 (#550) (Josh Berdine)
  + Admin: update travis versions of ocaml and opam (#551) (Josh Berdine)
  + Fix: missing break before `; _` (#549) (Josh Berdine)
  + Improve: module item spacing in sparse mode (#546) (Josh Berdine)
  + Improve: some simplifications (#542) (Guillaume Petiot)
  + Improve: remove unnecessary parens when open module (#537) (Guillaume Petiot)
  + Improve: not breaking after bind/map operators (#463) (Guillaume Petiot)
  + Fix suboptimal docstring formatting (#540) (Guillaume Petiot)
  + amend janestreet profile (#524) (Mathieu Barbin)
  + Improve: option break-separators (#461) (Guillaume Petiot)
  + Fix formatting of types with ocp-indent-compat=true (#525) (Guillaume Petiot)
  + Preserve shebang (#533) (Guillaume Petiot)
  + Fix: remove indented empty lines between comments (#531) (Guillaume Petiot)
  + Improve: remove indented empty lines separating recursive modules (#528) (Guillaume Petiot)
  + add update_tests.sh (#529) (Guillaume Petiot)
  + Improve: space around collection expressions (#527) (Guillaume Petiot)
  + Improve: remove more spaces inside parenthesized multiline constructs (#526) (Guillaume Petiot)
  + Disable docstring parsing for external tests (#518) (Guillaume Petiot)
  + Fix odoc normalize (#520) (Guillaume Petiot)
  + Better docstring error msgs (#519) (Guillaume Petiot)
  + Fix odoc seps (#511) (Guillaume Petiot)
  + Improve: option 'single-case' (#426) (Guillaume Petiot)
  + Add a parens-tuple-patterns configuration option (#498) (Nathan Rebours)
  + Fix: comments should not be parsed for diff (#509) (Guillaume Petiot)
  + Fix: odoc refs (#510) (Guillaume Petiot)
  + Fix formatting of or-patterns in try expressions (#503) (Nathan Rebours)
  + Test: improve test_branch.sh to allow different config for branch (#496) (Josh Berdine)
  + Improve: option 'parens-ite' (#430) (Guillaume Petiot)
  + fix break-struct for toplevel items (not in a struct) (#497) (Guillaume Petiot)
  + Fix: breaking of variant types (#486) (Guillaume Petiot)
  + Improve: autocompletion of git branch names for test_branch.sh (#485) (Guillaume Petiot)
  + Fix: Sanitize docstring check (#481) (Guillaume Petiot)
  + Improve the formatting of lists in doc comments (#480) (Jérémie Dimino)
  + Add PR test script and update contributing guidelines with expected usage (#479) (Josh Berdine)
  + Fix break struct natural (#443) (Guillaume Petiot)
  + Fix: disable-outside-detected-project: disable ocamlformat when no .ocamlformat file is found (#475) (Guillaume Petiot)
  + Improve: error message when docstrings move (#446) (Guillaume Petiot)
  + Improve: print-config prints all options (#465) (Guillaume Petiot)
  + Ocamldoc docstrings (#460) (Guillaume Petiot)
  + Doc: disable-outside-detected-project (#468) (Guillaume Petiot)
  + Improve: shorter output of regtests (#469) (Guillaume Petiot)
  + Admin: add code of conduct and copyright headers to build and package system (Josh Berdine)
  + Improve: add license header for tools/ocamlformat-diff/ocamlformat_diff.ml (#466) (Guillaume Petiot)
  + Build: a few simplifications enabled by dune 1.1.1 (#457) (Josh Berdine)
  + Improve: record fields with attributes and docs in type definitions (#458) (Josh Berdine)
  + Fix exception comments (#459) (Guillaume Petiot)
  + Ocamlformat diff tool (#450) (Guillaume Petiot)

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
