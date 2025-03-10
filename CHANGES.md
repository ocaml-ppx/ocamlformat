# Changelog

Items marked with an asterisk (\*) are changes that are likely to format
existing code differently from the previous release when using the default
profile. This started with version 0.26.0.

## unreleased

### Fixed

- Fixed `wrap-comments=true` not working with the janestreet profile (#2645, @Julow)
  Asterisk-prefixed comments are also now formatted the same way as with the
  default profile.

- Fixed `nested-match=align` not working with `match%ext` (#2648, @EmileTrotignon)

- Fixed the AST generated for bindings of the form `let pattern : type = function ...`
  (#2651, @v-gb)

- Print valid syntax for the corner case (1).a (#2653, @v-gb)

- `Ast_mapper.default_mapper` now iterates on the location of `in` in `let+ .. in ..`
  (#2658, @v-gb)

- Fix missing parentheses in `let+ (Cstr _) : _ = _` (#2661, @Julow)
  This caused a crash as the generated code wasn't valid syntax.

- Fix bad indentation of `let%ext { ...` (#2663, @EmileTrotignon)
  with `dock-collection-brackets` enabled.

- ocamlformat is now more robust when used as a library to print modified ASTs
  (#2659, @v-gb)

## 0.27.0

### Highlight

- \* Support OCaml 5.2 syntax (#2519, #2544, #2590, #2596, #2621, #2628, @Julow, @EmileTrotignon, @hhugo)
  This includes local open in types, raw identifiers, and the new
  representation for functions.
  This might change the formatting of some functions due to the formatting code
  being completely rewritten.

- Support OCaml 5.3 syntax (#2609, #2610, #2611, #2622, #2623, #2562, #2624, #2625, #2627, @Julow, @Zeta611)
  This adds support for effect patterns, short functor type arguments and utf8
  identifiers.
  To format code using the new `effect` syntax, add this option to your
  `.ocamlformat`:
  ```
  ocaml-version = 5.3
  ```

- Documentation comments are now formatted by default (#2390, @Julow)
  Use the option `parse-docstrings = false` to restore the previous behavior.

- \* Consistent indentation of polymorphic variant arguments (#2427, @Julow)
  Increases the indentation by one to make the formatting consistent with
  normal variants. For example:
  ```
    ...
    (* before *)
      (`Msg
        (foo bar))
    (* after *)
      (`Msg
         (foo bar))
  ```

- Build on OCaml 5.3 (#2603, @adamchol, @Julow)

### Added

- Improve the emacs plugin (#2577, #2600, @gridbugs, @thibautbenjamin)
  Allow a custom command to be used to run ocamlformat and add compatibility
  with emacs ocaml tree-sitter modes.

- Added option `let-binding-deindent-fun` (#2521, @henrytill)
  to control the indentation of the `fun` in:
  ```
  let f =
   fun foo ->
    bar
  ```

- Added back the flag `--disable-outside-detected-project` (#2439, @gpetiot)
  It was removed in version 0.22.

- Support newer Odoc syntax (#2631, #2632, #2633, @Julow)

### Changed

- \* Consistent formatting of comments (#2371, #2550, @Julow)
  This is mostly an internal change but some comments might be formatted differently.

- \* Improve formatting of type constraints with type variables (#2437, @gpetiot)
  For example:
  ```
  let f : type a b c.
      a -> b -> c =
    ...
  ```

- \* Improve formatting of functor arguments (#2505, @Julow)
  This also reduce the indentation of functor arguments with long signatures.

- Improvements to the Janestreet profile (#2445, #2314, #2460, #2593, #2612, @Julow, @tdelvecchio-jsc)

- \* Undo let-bindings and methods normalizations (#2523, #2529, @gpetiot)
  This remove the rewriting of some forms of let-bindings and methods:
  + `let f x = (x : int)` is no longer rewritten into `let f x : int = x`
  + `let f (type a) (type b) ...` is no longer rewritten into `let f (type a b) ...`
  + `let f = fun x -> ...` is no longer rewritten into `let f x = ...`

- \* The `break-colon` option is now taken into account for method type constraints (#2529, @gpetiot)

- \* Force a break around comments following an infix operator (fix non-stabilizing comments) (#2478, @gpetiot)
  This adds a line break:
  ```
    a
    ||
    (* this comment is now on its own line *)
    b
  ```

### Fixed

- Fix placement of comments in some cases (#2471, #2503, #2506, #2540, #2541, #2592, #2617, @gpetiot, @Julow)
  Some comments were being moved or causing OCamlformat to crash.
  OCamlformat refuses to format if a comment would be missing in its output, to avoid loosing code.

- Fix attributes being dropped or moved (#2247, #2459, #2551, #2564, #2602, @EmileTrotignon, @tdelvecchio-jsc, @Julow)
  OCamlformat refuses to format if the formatted code has a different meaning than the original code, for example, if an attribute is removed.
  We also try to avoid moving attributes even if that doesn't change the original code, for example we no longer format `open[@attr] M` as `open M [@@attr]`.

- Remove trailing space inside a wrapping empty signature (#2443, @Julow)
- Fix extension-point spacing in structures (#2450, @Julow)
- \* Consistent break after string constant argument (#2453, @Julow)
- \* Fix cinaps comment formatting to not change multiline string contents (#2463, @tdelvecchio-jsc)
- \* Fix the indentation of tuples in attributes and extensions (#2488, @Julow)
- \* Fix weird indentation and line breaks after comments (#2507, #2589, #2606, @Julow)
- \* Fix unwanted alignment in if-then-else (#2511, @Julow)
- Fix missing parentheses around constraint expressions with attributes (#2513, @alanechang)
- Fix formatting of type vars in GADT constructors (#2518, @Julow)
- Fix `[@ocamlformat "disable"]` in some cases (#2242, #2525, @EmileTrotignon)
  This caused a bug inside `class type` constructs and when attached to a `let ... in`
- Display `a##b` instead of `a ## b` and similarly for operators that start with # (#2580, @v-gb)
- \* Fix arrow type indentation with `break-separators=before` (#2598, @Julow)
- Fix missing parentheses around a let in class expressions (#2599, @Julow)
- Fix formatting of paragraphs in lists in documentation (#2607, @Julow)
- Avoid unwanted space in references and links text in documentation (#2608, @Julow)
- \* Improve the indentation of attributes in patterns (#2613, @Julow)
- \* Avoid large indentation in patterns after `let%ext` (#2615, @Julow)

## 0.26.2 (2024-04-18)

### Changed

- Compatible with OCaml 5.2.0 (#2510, @gpetiot, @Julow)

## 0.26.1 (2023-09-15)

### Changed

- Compatible with OCaml 5.1.0 (#2412, @Julow)
  The syntax of let-bindings changed sligthly in this version.
- Improved ocp-indent compatibility (#2428, @Julow)
- \* Removed extra break in constructor declaration with comment (#2429, @Julow)
- \* De-indent the `object` keyword in class types (#2425, @Julow)
- \* Consistent formatting of arrows in class types (#2422, @Julow)

### Fixed

- Fix dropped attributes on a begin-end in a match case (#2421, @Julow)
- Fix dropped attributes on begin-end in an if-then-else branch (#2436, @gpetiot)
- Fix non-stabilizing comments before a functor type argument (#2420, @Julow)
- Fix crash caused by module types with nested `with module` (#2419, @Julow)
- Fix ';;' formatting between doc-comments and toplevel directives (#2432, @gpetiot)

## 0.26.0 (2023-07-18)

### Removed

- Remove `--numeric` feature (#2333, #2357, @gpetiot)

### Deprecated

### Fixed

- Fix crash caused by `let f (type a) :> a M.u = ..` (#2399, @Julow)
- Fix crash caused by `module T = (val (x : (module S)))` (#2370, @Julow)
- Fix invalid formatting of `then begin end` (#2369, @Julow)
- Protect match after `fun _ : _ ->` (#2352, @Julow)
- Fix invalid formatting of `(::)` (#2347, @Julow)
- Fix indentation of module-expr extensions (#2323, @gpetiot)
- \* Remove double parentheses around tuples in a match (#2308, @Julow)
- \* Remove extra parentheses around module packs (#2305, @Julow, @gpetiot)
- Fix indentation of trailing double-semicolons (#2295, @gpetiot)
- Fix formatting of comments in "disable" chunks (#2279, @gpetiot)
- Fix non-stabilizing comments attached to private/virtual/mutable keywords (#2272, #2307, @gpetiot, @Julow)

### Changed

- Improve formatting of doc-comments (#2338, #2349, #2376, #2377, #2379, #2378, @Julow)
  Remove unnecessary escaping and preserve empty lines.
- \* Indent `as`-patterns that have parentheses (#2359, @Julow)
- Don't print warnings related to odoc code-blocks when '--quiet' is set (#2336, #2373, @gpetiot, @Julow)
- \* Improve formatting of module arguments (#2322, @Julow)
- \* Don't indent attributes after a let/val/external (#2317, @Julow)
- Consistent indentation of `@@ let+ x = ...` (#2315, #2396, @Julow)
  It was formatted differently than `@@ let x = ...`.
- \* Improve formatting of class expressions and signatures (#2301, #2328, #2387, @gpetiot, @Julow)
- \* Consistent indentation of `fun (type a) ->` following `fun x ->` (#2294, @Julow)
- \* Restore short-form formatting of record field aliases (#2282, #2388, @gpetiot, @Julow)
- \* Restore short-form for first-class modules: `((module M) : (module S))` is formatted as `(module M : S)`) (#2280, #2300, @gpetiot, @Julow)
- \* Improve indentation of `~label:(fun ...` (#2271, #2291, #2293, #2298, #2398, @Julow)
  The `fun` keyword is docked where possible and the arguments are indented to avoid confusion with the body.
- JaneStreet profile: treat comments as doc-comments (#2261, #2344, #2354, #2365, #2392, @gpetiot, @Julow)
- Tweaks the JaneStreet profile to be more consistent with ocp-indent (#2214, #2281, #2284, #2289, #2299, #2302, #2309, #2310, #2311, #2313, #2316, #2362, #2363, @gpetiot, @Julow)

### Added

- Handle short syntax for generative functor types (#2348, @gpetiot)
- Improved error reporting for unstable or dropped comments (#2292, @gpetiot)

## 0.25.1 (2023-03-06)

### Fixed

- Janestreet: Fix indentation of functions passed as labelled argument (#2259, @Julow)

## 0.25.0 (2023-02-24)

### Internal

- The declaration of options is a regular module instead of a functor. (#2193, @EmileTrotignon)

### Fixed

- Fix indentation when ocamlformat is disabled on an expression (#2129, @gpetiot)
- Reset max-indent when the `max-indent` option is not set (#2131, @hhugo, @gpetiot)
- Add missing parentheses around immediate objects having attributes attached in 4.14 (#2144, @gpetiot)
- Fix dropped comment attached to the identifier of an open-expression (#2155, @gpetiot)
- Correctly format chunks of file in presence of `enable`/`disable` floating attributes (#2156, @gpetiot)
- Remove abusive normalization in docstrings references (#2159, #2162, @EmileTrotignon)
- Fix parentheses around symbols in if-then-else branches (#2169, @gpetiot)
- Preserve position of comments around variant identifiers (#2179, @gpetiot)
- Fix parentheses around symbol identifiers (#2185, @gpetiot)
- Fix alignment inconsistency between let-binding and let-open (#2187, @gpetiot)
- Fix reporting of operational settings origin in presence of profiles (#2188, @EmileTrotignon)
- Fix alignment inconsistency of if-then-else in apply (#2203, @gpetiot)
- Fix automated Windows build (#2205, @nojb)
- Fix spacing between recursive module bindings and recursive module declarations (#2217, @gpetiot)
- ocamlformat-rpc: use binary mode for stdin/stdout (#2218, @rgrinberg)
- Fix interpretation of glob pattern in `.ocamlformat-ignore` under Windows (#2206, @nojb)
- Remove conf mutability, and correctly display the conventional profile when using print-config (#2233, @EmileTrotignon)
- Preserve position of comments around type alias (#2239, @EmileTrotignon)
- Preserve position of comments around constructor record (#2237, @EmileTrotignon)
- Preserve position of comments around external declaration strings (#2238, @EmileTrotignon, @gpetiot)
- Preserve position of comments around module pack expressions (#2234, @EmileTrotignon, @gpetiot)
- Correctly parenthesize array literals with attributes in argument positions (#2250, @ccasin)

### Changed

- Indent 2 columns after `initializer` keyword (#2145, @gpetiot)
- Preserve syntax of generative modules (`(struct end)` vs `()`) (#2135, #2146, @trefis, @gpetiot)
- Preserve syntax of module unpack with type constraint (`((module X) : (module Y))` vs `(module X : Y)`) (#2136, @trefis, @gpetiot)
- Normalize location format for warning and error messages (#2139, @gpetiot)
- Preserve syntax and improve readability of indexop-access expressions (#2150, @trefis, @gpetiot)
  + Break sequences containing indexop-access assignments
  + Remove unnecessary parentheses around indices
- Mute warnings for odoc code blocks whose syntax is not specified (#2151, @gpetiot)
- Improve formatting of odoc links (#2152, @gpetiot)
- Preserve sugared extension node attached to an `if` carrying attributes (#2167, @trefis, @gpetiot)
- Remove unnecessary parentheses around partially applied infix operators with attributes (#2198, @gpetiot)
- JaneStreet profile: doesn't align infix ops with open paren (#2204, @gpetiot)
- Re-use the type let_binding from the parser instead of value_binding, improve the spacing of let-bindings regarding of having extension or comments (#2219, @gpetiot)
- The `ocamlformat` package now only contains the binary, the library is available through the `ocamlformat-lib` package (#2230, @gpetiot)
- The position of module and module type attributes is now preserved. (#2451, #2455, @emiletrotignon)

### Added

- Add a `break-colon` option to decide whether to break before or after the `:` symbol in value binding declarations and type constraints. This behavior is no longer ensured by `ocp-indent-compat`. (#2149, @gpetiot)
- Format `.mld` files as odoc documentation files (#2008, @gpetiot)
- New value `vertical` for option `if-then-else` (#2174, @gpetiot)
- New value `vertical` for option `break-cases` (#2176, @gpetiot)
- New value `wrap-or-vertical` for option `break-infix` that only wraps high precedence infix ops (#1865, @gpetiot)

## 0.24.1 (2022-07-18)

### Added

- Support `odoc-parser.2.0.0` (#2123, @gpetiot)
  * Breaking change: incompatible with earlier versions of `odoc-parser`
  * New inline math elements `{m ...}` available in doc-comments
  * New block math elements `{math ...}` available in doc-comments

## 0.23.0 (2022-07-07)

### Removed

- `bench` binary is not distributed anymore to avoid name collisions (#2104, @gpetiot)

### Fixed

- Preserve comments around object open/close flag (#2097, @trefis, @gpetiot)
- Preserve comments around private/mutable/virtual keywords (#2098, @trefis, @gpetiot)
- Closing parentheses of local open now comply with `indicate-multiline-delimiters` (#2116, @gpetiot)
- emacs: fix byte-compile warnings (#2119, @syohex)

### Changed

- Use the API of ocp-indent to parse the `.ocp-indent` files (#2103, @gpetiot)
- JaneStreet profile: set `max-indent = 2` (#2099, @gpetiot)
- JaneStreet profile: align pattern-matching bar `|` under keyword instead of parenthesis (#2102, @gpetiot)

## 0.22.4 (2022-05-26)

### Removed

- Profiles `compact` and `sparse` are now removed (#2075, @gpetiot)
- Options `align-cases`, `align-constructors-decl` and `align-variants-decl` are now removed (#2076, @gpetiot)
- Option `disable-outside-detected-project` is now removed (#2077, @gpetiot)

### Deprecated

- Cancel the deprecations of options that are not set by the preset profiles (#2074, @gpetiot)

### Fixed

- emacs: Remove temp files in the event of an error (#2003, @gpetiot)
- Fix unstable comment formatting around prefix op (#2046, @gpetiot)

### Changed

- Qtest comments are not re-formatted (#2034, @gpetiot)
- ocamlformat-rpc is now distributed through the ocamlformat package (#2035, @Julow)
- Doc-comments code blocks with a language other than 'ocaml' (set in metadata) are not parsed as OCaml (#2037, @gpetiot)
- More comprehensible error message in case of version mismatch (#2042, @gpetiot)
- The global configuration file (`$XDG_CONFIG_HOME` or `$HOME/.config`) is only applied when no project is detected, `--enable-outside-detected-project` is set, and no applicable `.ocamlformat` file has been found. Global and local configurations are no longer used at the same time. (#2039, @gpetiot)
- Set `ocaml-version` to a fixed version (4.04.0) by default to avoid reproducibility issues and surprising behaviours (#2064, @kit-ty-kate)
- Split option `--numeric=X-Y` into `--range=X-Y` and `--numeric` (flag). For now `--range` can only be used with `--numeric`. (#2073, #2082, @gpetiot)

### Added

- New syntax `(*= ... *)` for verbatim comments (#2028, @gpetiot)
- Preserve the begin-end construction in the AST (#1785, @hhugo, @gpetiot)
- Preserve position of comments located after the semi-colon of the last element of lists/arrays/records (#2032, @gpetiot)
- Option `--print-config` displays a warning when an .ocamlformat file defines redundant options (already defined by a profile) (#2084, @gpetiot)

## 0.21.0 (2022-02-25)

### Fixed

- Add missing parentheses around variant class arguments (#1967, @gpetiot)
- Fix indentation of module binding RHS (#1969, @gpetiot)
- Fix position of `:=` when `assignment-operator=end-line` (#1985, @gpetiot)
- Fix position of comments attached to constructor decl (#1986, @gpetiot)
- Do not wrap docstrings, `wrap-comments` should only impact non-documentation comments, wrapping invalid docstrings would cause the whole file to not be formatted (#1988, @gpetiot)
- Do not break between 2 module items when the first one has a comment attached on the same line. Only a comment on the next line should induce a break to make it clear to which element it is attached to (#1989, @gpetiot)
- Preserve position of comments attached to the last node of a subtree (#1667, @gpetiot)
- Do not override the values of the following non-formatting options when a profile is set: `comment-check`, `disable`, `max-iters`, `ocaml-version`, and `quiet` (#1995, @gpetiot).
- Remove incorrect parentheses around polymorphic type constraint (#2002, @gpetiot)
- Handle cases where an attribute is added to a bind expression, e.g. `(x >>= (fun () -> ())) [@a]` (#2013, @emillon)
- Fix indentation of constraints of a package type pattern (#2025, @gpetiot)

### Changed

- More expressions are considered "simple" (not inducing a break e.g. as an argument of an application):
  + Variants with no argument (#1968, @gpetiot)
  + Empty or singleton arrays/lists (#1943, @gpetiot)
- Print odoc code block delimiters on their own line (#1980, @gpetiot)
- Make formatting of cons-list patterns consistent with cons-list expressions, (::) operators are aligned when possible, comments position also improved (#1983, @gpetiot)
- Apply 'sequence-style' to add a space before ';;' between toplevel items, consistently with the formatting of ';' in sequences (#2004, @gpetiot)

### Added

- Format toplevel phrases and their output (#1941, @Julow, @gpetiot).
  This feature is enabled with the flag `--parse-toplevel-phrases`.
  Toplevel phrases are supported when they are located in doc-comments blocks and cinaps comments.
  Whole input files can also be formatted as toplevel phrases with the flag `--repl-file`.

### RPC

- ocamlformat-rpc-lib is now functorized over the IO (#1975, @gpetiot).
  Now handles `Csexp.t` types instead of `Sexplib0.Sexp.t`.
- RPC v2 (#1935, @panglesd):
  Define a 'Format' command parameterized with optionnal arguments to set or override the config and path, to format in the style of a file.
- Prevent RPC to crash on version mismatch with `.ocamlformat` (#2011, @panglesd, @Julow)

## 0.20.1 (2021-12-13)

### Added

- Update to odoc-parser 1.0.0 (#1843, @Julow).
  New syntax: code blocks can carry metadata, e.g.:
  `{@ocaml kind=toplevel env=e1[ code ]}`

## 0.20.0 (2021-12-06)

### Deprecated

- Profiles `compact` and `sparse` are now deprecated and will be removed by version 1.0 (#1803, @gpetiot)
- Options that are not set by the preset profiles are now deprecated and will be removed by version 1.0:
  + `align-cases`, `align-constructors-decl` and `align-variants-decl` (#1793, @gpetiot)
  + `disambiguate-non-breaking-match` (#1805, @gpetiot)
  + `break-before-in` (#1888, @gpetiot)
  + `break-cases={toplevel,all}` (#1890, @gpetiot)
  + `break-collection-expressions` (#1891, @gpetiot)
  + `break-fun-decl=smart` (#1892, @gpetiot)
  + `break-fun-sig=smart` (#1893, @gpetiot)
  + `break-string-literals` (#1894, @gpetiot)
  + `break-struct` (#1895, @gpetiot)
  + `extension-indent` (#1896, @gpetiot)
  + `function-indent` (#1897, @gpetiot)
  + `function-indent-nested` (#1898, @gpetiot)
  + `if-then-else={fit-or-vertical,k-r}` (#1899, @gpetiot)
  + `indicate-multiline-delimiters=closing-on-separate-line` (#1900, @gpetiot)
  + `indent-after-in` (#1901, @gpetiot)
  + `let-binding-indent` (#1902, @gpetiot)
  + `let-binding-spacing=sparse` (#1903, @gpetiot)
  + `match-indent` (#1904, @gpetiot)
  + `match-indent-nested` (#1905, @gpetiot)
  + `module-item-spacing=preserve` (#1906, @gpetiot)
  + `nested-match` (#1907, @gpetiot)
  + `parens-tuple-patterns` (#1908, @gpetiot)
  + `sequence-style=before` (#1909, @gpetiot)
  + `stritem-extension-indent` (#1910, @gpetiot)
  + `type-decl-indent` (#1911, @gpetiot)

### Fixed

- Fix normalization of sequences of expressions (#1731, @gpetiot)
- Type constrained patterns are now always parenthesized, parentheses were missing in a class context (#1734, @gpetiot)
- Support sugared form of coercions in let bindings (#1739, @gpetiot)
- Add missing parentheses around constructor used as indexing op (#1740, @gpetiot)
- Honour .ocamlformat-ignore on Windows (#1752, @nojb)
- Avoid normalizing newlines inside quoted strings `{|...|}` (#1754, @nojb, @hhugo)
- Fix quadratic behavior when certain constructs are nested. This corresponds
  to the cases where a partial layout is triggered to determine if a construct
  fits on a single line for example. (#1750, #1766, @emillon)
- Fix non stabilizing comments after infix operators (`*`, `%`, `#`-ops) (#1776, @gpetiot)
- Fix excessive break and wrong indentation after a short-open when `indicate-multiline-delimiters=closing-on-separate-line` (#1786, @gpetiot)
- Add parentheses around type alias used as type constraint (#1801, @gpetiot)
- Fix alignment of comments inside a tuple pattern and remove incorrect linebreak.
  Fix formatting of labelled arguments containing comments. (#1797, @gpetiot)
- Emacs: only hook ocamlformat mode on tuareg/caml modes when ocamlformat is not disabled (#1814, @gpetiot)
- Fix boxing of labelled arguments, avoid having a linebreak after a label when the argument has a comment attached (#1830, #1885, @gpetiot)
- Add missing parentheses around application of prefix op when applied to other operands (#1825, @gpetiot)
- Fix application of a monadic binding when 'break-infix-before-func=false' (#1849, @gpetiot)
- Fix dropped comments attached to a sequence in a sugared extension node (#1853, @gpetiot)
- Fix formatting of exception types, and add missing parentheses (#1873, @gpetiot)
- Fix indentation of with-type constraints (#1883, @gpetiot)
- Preserve sugared syntax of extension points with attributes (#1913, @gpetiot)
- Improve comment attachment when followed but not preceded by a linebreak (#1926, @gpetiot)
- Fix position of comments preceding Pmod_ident (#1939, @gpetiot)
- Make the formatting of attributes and docstrings more consistent (#1929, @gpetiot)
- Fix stabilization of comments inside attributes (#1942, @gpetiot)

### Changed

- Set 'module-item-spacing=compact' in the default/conventional profile (#1848, @gpetiot)
- Preserve bracketed lists in the Parsetree (#1694, #1876, #1914, @gpetiot)
- Line directives now cause OCamlFormat to emit an error, they were previously silently ignored (#1845, @gpetiot)
- Apply option 'module-item-spacing' on mutually recursive type declarations for more consistency (#1854, @gpetiot)

### Added

- Handle merlin typed holes (#1698, @gpetiot)
- Handle punned labelled arguments with type constraint in function applications.
  For example, function application of the form `foo ~(x:int)` instead of the explicit `foo ~x:(x:int)`. (ocaml#10434) (#1756, #1759, @gpetiot).
  This syntax is only produced when the output syntax is at least OCaml 4.14.
- Allow explicit binders for type variables (ocaml#10437) (#1757, @gpetiot)
- Add a new `ocaml-version` option to select the version of OCaml syntax of the output (#1759, @gpetiot)
- Allow disambiguated global identifiers (like t/2) so they can be formatted by tools like OCaml-LSP (#1716, @let-def)
- Handle let operator punning uniformly with other punning forms.
  Normalizes let operator to the punned form where possible, if output syntax version is at least OCaml 4.13.0. (#1834, #1846, @jberdine)
- Remove unnecessary surrounding parentheses for immediate objects.
  This syntax is only produced when the output syntax is at least OCaml 4.14. (#1934, @gpetiot)

## 0.19.0 (2021-07-16)

### Fixed

- Fix formatting of odoc tags: the argument should be on the same line, indent description that wraps (#1634, #1635, @gpetiot)
- Consistently format let bindings and monadic let bindings, do not drop comments before monadic bindings (#1636, @gpetiot)
- Fix dropped comments attached to pattern constrained by polynewtype (#1645, @gpetiot)
- Fix comment attachment on infix operators (#1643, @gpetiot)
- Add missing spaces inside begin-end delimiting an ite branch (#1646, @gpetiot)
- Add missing parens around function at RHS of infix op (#1642, @gpetiot)
- Preserve begin-end keywords delimiting match cases (#1651, @gpetiot)
- Fix alignment of closing paren on separate line for anonymous functions (#1649, @gpetiot)
- Preserve begin-end keywords around infix operators (#1652, @gpetiot)
- Preserve `begin%ext` syntax for infix opererator expressions (#1653, @gpetiot)
- Consistently format comments attached to let-and bindings located at toplevel (#1663, @gpetiot)
- Remove double parens around a functor in a module application (#1681, @gpetiot)
- Improve breaking of comments to avoid violating the margin (#1676, @jberdine)
- Fix parentheses around successive unary operations (#1696, @gpetiot)
- Add missing break between pattern and attribute (#1711, @gpetiot)
- Add missing parentheses around expression having attributes or comments inside a shorthand let-open clause (#1708, @gpetiot)
- Do not consider leading star '*' when checking the diff of doc comments (#1712, @hhugo)
- Fix formatting of multiline non-wrapping comments (#1723, @gpetiot)
- Fix position of comments following a record field (#1945, @gpetiot)

### Changed

- Improve the diff of unstable docstrings displayed in error messages (#1654, @gpetiot)
- Use UTF8 length of strings, not only in wrapped comments (#1673, @jberdine)
- Improve position of `;;` tokens (#1688, @gpetiot)
- Depend on `odoc-parser` instead of `odoc` (#1683, #1713, @kit-ty-kate, @jonludlam, @julow).
  The parser from odoc has been split from the main odoc package and put into its own package, `odoc-parser`.
- Revert infix-form list formatting to pre-0.17.0 (#1717, @gpetiot)

### Added

- Implement OCaml 4.13 features (#1680, @gpetiot)
  + Named existentials in pattern-matching (ocaml#9584)
  + Let-punning (ocaml#10013)
  + Module type substitutions (ocaml#10133)
- Emacs integration (disabled for ocamlformat < 0.19.0):
  + Indent a line or a region with ocamlformat when pressing <TAB>
  + Break the line and reindent the cursor when pressing <ENTER>
  (#1639, #1685, @gpetiot) (#1687, @bcc32)
- Add 'line-endings=lf|crlf' option to specify the line endings used in the
  formatted output. (#1703, @nojb)

### Internal

- A script `tools/build-mingw64.sh` is provided to build a native Windows
  binary of `ocamlformat` using `mingw64` toolchain under Cygwin.

## 0.18.0 (2021-03-30)

### Fixed

- Fix extraneous parenthesis after `let open` with `closing-on-separate-line` (#1612, @Julow)
- Add missing break between polytype quantification and arrow-type body (#1615, @gpetiot)

### Changed

- Use dune instrumentation backend for `bisect_ppx` (#1550, @tmattio)
- Format objects and classes consistently with structure and signature items (#1569, @bikallem)

### Added

- Expose a RPC interface through a new binary `ocamlformat-rpc` and a new library `ocamlformat-rpc-lib` (#1586, @gpetiot, @voodoos)

## 0.17.0 (2021-02-15)

### Removed

- Remove the 'let-open' option, deprecated since 0.16.0 (#1563, @gpetiot)
- Remove support for OCaml 4.06 and 4.07, minimal version requirement bumped to OCaml 4.08 (#1549, @gpetiot)
- Remove the 'extension-sugar' option, deprecated since 0.14.0 (#1588, @gpetiot)

### Fixed

- Fix parsing of invalid file wrt original source handling (#1542, @hhugo)
- Preserve the syntax of infix set/get operators (#1528, @gpetiot).
  `String.get` and similar calls used to be automatically rewritten to their corresponding infix form `.()`, that was incorrect when using the `-unsafe` compilation flag. Now the concrete syntax of these calls is preserved.
- Add location of invalid docstring in warning messages (#1529, @gpetiot)
- Fix comments on the same line as prev and next elements (#1556, @gpetiot)
- Break or-patterns after comments and preserve their position at the end of line (#1555, @gpetiot)
- Fix linebreak between signature items of the same group (#1560, @gpetiot)
- Fix stack overflow on large string constants (#1562, @gpetiot)
- Fix comment position around list cons operator (#1567, @gpetiot)
- Fix the vertical alignment test to break down comment groups (#1575, @gpetiot)
- Preserve spacing of toplevel comments (#1554, @gpetiot)
- Support more sugared extension points (#1587, @gpetiot)

### Changed

- Add buffer filename in the logs when applying ocamlformat (#1557, @dannywillems)
- Improve comment position in pattern collection (#1576, @gpetiot)
- Consistent positioning of lambda return type annotations when no-break-infix-before-func and pre/post extensions (#1581, @gpetiot)

### Added

- Support injectivity type annotations (OCaml 4.12 feature) (#1523, @gpetiot)

## 0.16.0 (2020-11-16)

### Removed

- Remove the 'escape-chars' option, deprecated since 0.14.0 (#1462, @gpetiot)
- Remove the 'escape-strings' option, deprecated since 0.14.0 (#1463, @gpetiot)
- Remove the 'doc-comments-val' option, deprecated since 0.14.2 (#1461, @gpetiot)
- Removed options are now listed in the commandline manual (new REMOVED OPTIONS section) (#1469, @Julow)

### Changed

- Set 'indicate-multiline-delimiters=no' on default profile (#1452, @gpetiot)
- Option 'let-open' is now deprecated, concrete syntax will always be preserved starting from OCamlFormat v0.17.0, corresponding to the current 'let-open=preserve' behavior. (#1467, @gpetiot)
- Warnings printed by ocamlformat itself now use the 4.12 style with symbolic names (#1511, #1518, @emillon)
- Remove extension from executable name in error messages. On Windows, this means that messages now start with "ocamlformat: ..." instead of "ocamlformat.exe: ..." (#1531, @emillon)
- Using tokens instead of string manipulation when inspecting the original source (#1526, #1533, #1541 @hhugo) (#1532, @gpetiot)

### Fixed

- Allow a break after `if%ext` with `if-then-else=keyword-first` (#1419, #1543, @gpetiot)
- Fix parentheses around infix applications having attributes (#1464, @gpetiot)
- Fix parentheses around the index arg of a non-sugared index operation (#1465, @gpetiot)
- Preserve comment position around `match` and `try` keywords (#1458, @gpetiot)
- Add missing break in module statement (#1431, @gpetiot)
- Indent attributes attached to included modules better (#1468, @gpetiot)
- Clean up `ocamlformat.el` for submission to MELPA (#1476, #1495, @bcc32)
  + Added missing package metadata to `ocamlformat.el` (#1474, @bcc32)
  + Fix `ocamlformat.el` buffer replacement for MacOS Emacs (#1481, @juxd)
- Add missing parentheses around a pattern matching that is the left-hand part of a sequence when an attribute is attached (#1483, @gpetiot)
- Add missing parentheses around infix operator used to build a function (#1486, @gpetiot)
- Fix comments around desugared expression (#1487, @gpetiot)
- Fix invalid fragment delimiters of format-invalid-files recovery mode (#1485, @hhugo)
- Fix misalignment of cases in docked `function` match (#1498, @gpetiot)
- Preserve short-form extensions for structure item extensions (#1502, @gpetiot).
  For example `open%ext M` will not get rewritten to `[%%ext open M]`.
- Do not change the spaces within the code spans in docstrings (#1499, @gpetiot)
- Comments of type constrained label in record pattern have to be relocated in 4.12 (#1517, @gpetiot)
- Preserve functor syntax for OCaml 4.12 (#1514, @gpetiot)
- Fix inconsistencies of the closing parentheses with indicate-multiline-delimiters (#1377, #1540, @gpetiot)
- Fix position of comments around list constructor (::) (#1524, @gpetiot)
- Fix comments position in extensions (#1525, @gpetiot)
- Fix formatting of field override with constraint (#1544, @gpetiot)

### Added

## 0.15.1 (2020-11-02)

### Internal

- Use ppxlib instead of ocaml-migrate-parsetree 1.x. (#1482, @emillon)
  + No functional changes are expected.
  + Cherry picked commits: 219dc1e3a4614041e1bc5428d003c0af4e, 9e453b0ef87124e33827ee2423289deef8, 7ad1e575ffa4ce3022c71daba39954d3b9, eb49db6772a9adabe611982000465d0ad7, dc79052a085950cd88fdef0843f665a029, c06c544e21bd65b726cde8fee0f78a6248, ce94d2fa50ff276b5782070375a0b30ba1

## 0.15.0 (2020-08-06)

### Changed

- Do not break inline elements such as `{i blah}` in docstrings (#1346, @jberdine)
- Distinguish hash-getter from hash-comparison infix operators. Operators of the form `#**#` or `#**.` where `**` can be 0 or more operator chars are considered getter operators and are not surrounded by spaces, as opposed to regular infix operators (#1376, @gpetiot)
- Type constraint on return type of functions is now always printed before the function body (#1381, #1397, @gpetiot)

### Fixed

- Restore previous functionality for pre-post extension points (#1342, @jberdine)
- Fix extra break before `function` body of a `fun` (#1343, @jberdine)
- Indent further args of anonymous functions (#1440, @gpetiot)
- Do not clear the emacs `*compilation*` buffer on successful reformat (#1350, @jberdine)
- Fix disabling with attributes on OCaml < 4.08 (#1322, @emillon)
- Preserve unwrapped comments by not adding artificial breaks when `wrap-comments=false` and `ocp-indent-compat=true` are set to avoid interfering with ocp-indent indentation. (#1352, @gpetiot)
- Break long literal strings at the margin (#1367, @gpetiot)
- Break after a multiline argument in an argument list (#1360, @gpetiot)
- Remove unnecessary parens around object (#1379, @gpetiot)
- Fix placement of comments on constants (#1383, @gpetiot)
- Do not escape arguments of some Odoc tags (#1391, 1408, @gpetiot, @Julow).
  The characters `[]{}` must not be escaped in the arguments of `@raise`, `@author`, `@version` and others.
- Fix missing open line between multi-line let-binding with poly-typexpr (#1372, @jberdine)
- Remove trailing space after expression when followed by an attribute and break before attributes attached to multi-line phrases (#1382, @gpetiot)
- Do not add a space to minimal comments `(* *)`, `(** *)` and `(*$ *)` (#1407, @gpetiot)
- Fix attributes position in labelled arguments type (#1434, @gpetiot)
- Add missing parens around type annotation in anonymous function (#1433, @gpetiot)
- Fix alignment of 'then' keyword in parenthesised expression (#1421, @gpetiot)

### Added

- Support quoted extensions (added in ocaml 4.11) (#1405, @gpetiot)
- Recognise eliom file extensions (#1430, @jrochel)

## 0.14.3 (2020-07-22)

### Changed

- No functional changes from 0.14.2. The goal of this release is to be
  compatible with base and stdio v0.14.0.
- Backport the following PRs:
  + Update opam metadata (#1386)
  + Add compatibility with base.v0.14.0 (#1396)
  + Allow stdio.v0.14 (#1399)

## 0.14.2 (2020-05-11)

### Changed

- Merge `doc-comments-val` option with `doc-comments`. The placement of documentation comments on `val` and `external` items is now controled by `doc-comments`.
  + `doc-comments=after` becomes `doc-comments=after-when-possible` to take into account the technical limitations of ocamlformat;
  + `doc-comments=before` is unchanged;
  + `doc-comments-val` is now replaced with `doc-comments`.
    To reproduce the former behaviors
    * `doc-comments=before` + `doc-comments-val=before`: now use `doc-comments=before`;
    * `doc-comments=before` + `doc-comments-val=after`: now use `doc-comments=before-except-val`;
    * `doc-comments=after` + `doc-comments-val=before`: this behavior did not make much sense and is not available anymore;
    * `doc-comments=after` + `doc-comments-val=after`: now use `doc-comments=after-when-possible`.
 (#1358, @jberdine, @Julow, @gpetiot).
 This reverts changes introduced in 0.14.1 (#1335) and 0.14.0 (#1012).

## 0.14.1 (2020-04-14)

### Changed

- The default for `doc-comments` is changed to `after` (#1335, @Julow).
  This reverts a change introduced in 0.14.0 (#1012).
- Revert deprecation of the `doc-comments` option (#1331, @Julow).
  This reverts a change introduced in 0.14.0 (#1293).

## 0.14.0 (2020-04-02)

### Added

- Add an option `--format-invalid-files` to print unparsable parts of the input as verbatim text. This feature is still experimental. (#1026, @gpetiot)
- Support multi-indices extended indexing operators (#1279, #1277, @Julow, @gpetiot).
  This feature has been added in OCaml 4.10.0
- Handle OCaml 4.10.0 AST (#1276, @gpetiot)
- Preserve functor syntax for consistency (#1312, @gpetiot).
  Previously both functor syntax: `module M = functor (K : S) -> struct end` and `module M (K : S) = struct end` would be formatted as the latter, the original syntax is now preserved.

### Changed

- Add the option `doc-comments-val=before|after` (#1012, @Julow).
  This option set the placement of documentation comment on `val` and `external` only.
  It is set to `after` by default.
- The default for `doc-comments` is changed from `after` to `before` (#1012, #1325, @Julow).
  This affects both `conventional` (default) and `ocamlformat` profiles.
- Some options are now deprecated:
  + `doc-comments` (#1293, #1012).
    This option depends on a flawed heuristic.
    It is replaced by `doc-comments-val` for `val` and `external` declarations.
    There is no equivalent to this option in the general case.
  + `escape-chars`, `escape-strings` and `extension-sugar` (#1293).
    These options are rarely used and their default behavior is considered to be the right behavior.
- Add space between `row_field` attributes and the label or arguments, to be
  consistent with the non-polymorphic case. (#1299, @CraigFe)

### Fixed

- Fix missing parentheses around `let open` (#1229, @Julow).
  eg. `M.f (M.(x) [@attr])` would be formatted to `M.f M.(x) [@attr]`, which would crash OCamlformat
- Remove unecessary parentheses with attributes in some structure items:
  + extensions and eval items (#1230, @Julow).
    eg. the expression `[%ext (() [@attr])]` or the structure item `(() [@attr]) ;;`
  + `let _ = ...`  constructs (#1244, @emillon)
- Fix some bugs related to comments:
  + after a function on the rhs of an infix (#1231, @Julow).
    eg. the comment in `(x >>= fun y -> y (* A *))` would be dropped
  + in module unpack (#1309, @Julow).
    eg. in the module expression `module M = (val x : S (* A *))`
- Fix formatting of empty signature payload `[%a:]` (#1236, @emillon)
- Fix parenthesizing when accessing field of construct application (#1247, @gpetiot)
- Fix formatting of attributes on object overrides `{< >}` (#1238, @emillon)
- Fix attributes on coercion (#1239, @emillon)
- Fix formatting of attributes on packed modules (#1243, @emillon)
- Fix parens around binop operations with attributes (#1252, #1306, @gpetiot, @CraigFe)
- Remove unecessary parentheses in the argument of indexing operators (#1280, @Julow)
- Retain attributes on various AST nodes:
  + field set expressions, e.g. `(a.x <- b) [@a]` (#1284, @CraigFe)
  + instance variable set expressions, e.g. `(a <- b) [@a]` (#1288, @CraigFe)
  + indexing operators, e.g. `(a.(b)) [@a]` (#1300, @CraigFe)
  + sequences, e.g. `(a; b) [@a]` (#1291, @CraigFe)
- Avoid unnecessary spacing after object types inside records and polymorphic variants, e.g. `{foo : < .. > [@a]}` and `{ foo : < .. > }` (#1296, @CraigFe)
- Fix missing parentheses around tuples with attributes. (#1301, @CraigFe).
  Previously, `f ((0, 0) [@a])` would be formatted to `f (0, 0) [@a]`, crashing OCamlformat.
- Avoid emitting `>]` when an object type is contained in an extension point or attribute payload (#1298, @CraigFe)
- Fix crash on the expression `(0).*(0)` (#1304, @Julow).
  It was formatting to `0.*(0)` which parses as an other expression.
- Preserve empty doc-comments syntax. (#1311, @gpetiot).
  Previously `(**)` would be formatted to `(***)`.
- Do not crash when a comment contains just a newline (#1290, @emillon)
- Handle lazy patterns as arguments to `class` (#1289, @emillon)
- Preserve cinaps comments containing unparsable code (#1303, @Julow).
  Previously, OCamlformat would fallback to the "wrapping" logic, making the comment unreadable and crashing in some cases.
- Fix normalization of attributes, fixing the docstrings in attributes (#1314, @gpetiot)
- Add missing parentheses around OR-patterns with attributes (#1317, @gpetiot)
- Fix spacing inside parens for symbols when the spacing was handled by the englobing exp (#1316, @gpetiot)
- Fix invalid (unparsable) docstrings (#1315, @gpetiot).
  When parsing a comment raises an error in odoc, it is printed as-is.
- Fix parenthesizing of optional arguments rebound to non-variables, e.g.
  `let f ?a:(A) = ()` rather than the unparsable `let f ?a:A = ()` (#1305, @CraigFe)

## 0.13.0 (2020-01-28)

### Added

- Add an option `--margin-check` to emit a warning if the formatted output exceeds the margin (#1110, @gpetiot)
- Preserve comment indentation when `wrap-comments` is unset (#1138, #1159, @Julow)
- Improve error messages (#1147, @Julow)
- Display standard output in the emacs plugin even when ocamlformat does not fail (#1189, @gpetiot)

### Removed

- Remove `ocamlformat_reason` (#254, #1185, @emillon).
  This tool has never been released to opam, has no known users, and overlaps
  with what `refmt` can do.
- Remove `ocamlformat-diff` (#1205, @gpetiot).
  This tool has never been released to opam, has no known users, and overlaps
  with what `merge-fmt` can do.

### Packaging

- Work with base v0.13.0 (#1163, @Julow)

### Fixed

- Fix placement of comments just before a '|' (#1203, @Julow)
- Fix build version detection when building in the absence of a git root (#1198, @avsm)
- Fix wrapping of or-patterns in presence of comments with `break-cases=fit` (#1167, @Julow).
  This also fixes an unstable comment bug in or-patterns
- Fix an unstable comment bug in variant declarations (#1108, @Julow)
- Fix: break multiline comments (#1122, @gpetiot)
- Fix: types on named arguments were wrapped incorrectly when preceding comments (#1124, @gpetiot)
- Fix the indentation produced by max-indent (#1118, @gpetiot)
- Fix break after Psig_include depending on presence of docstring (#1125, @gpetiot)
- Remove some calls to if_newline and break_unless_newline and fix break before closing brackets (#1168, @gpetiot)
- Fix unstable cmt in or-pattern (#1173, @gpetiot)
- Fix location of comment attached to the underscore of an open record (#1208, @gpetiot)
- Fix parentheses around optional module parameter (#1212, @cbarcenas)
- Fix grouping of horizontally aligned comments (#1209, @gpetiot)
- Fix dropped comments around module pack expressions (#1214, @Julow)
- Fix regression of comment position in list patterns (#1141, @jberdine)
- Fix: adjust definition of Location.is_single_line to reflect margin (#1102, @jberdine)

### Documentation

- Fix documentation of option `version-check` (#1135, @Wilfred)
- Fix hint when using `break-separators=after-and-docked` (#1130, @gretay-js)

## 0.12 (2019-11-04)

### Changed

- Set "conventional" as the default profile (#1060, @gpetiot).
  This new profile is made to better match the most used style and is encouraged.
  To continue using the previous default, use `profile = ocamlformat` in your `.ocamlformat`.
- CLI: Allow both values of boolean options (#1062, @Julow).
  Now, both `--opt` and --no-opt` are available on the CLI for any boolean option "opt".
  Previously, only one of them were available depending on the default value.
- Auto mode for `break-string-literals` (#1057, @gpetiot).
  `wrap`, `newlines` and `newlines-and-wrap` values of `break-string-literals` are removed.
  `auto` replaces them, it is equivalent to `newlines-and-wrap`.
- Dock collection brackets (#1014, @gpetiot).
  `after-and-docked` value of `break-separators` is removed and is replaced by a new `dock-collection-brackets` option.
- Preserve `begin` and `end` keywords in if-then-else (#978, @Julow).
  Previously, `begin`/`end` keywords around if-then-else branches were turned into parentheses.

#### Added

- Give a hint when warning 50 is raised (#1111, @gpetiot)
- Add a message when a config value is removed (#1089, @emillon).
  Explain what replaces removed options and avoid printing a parsing error.
- Implement `sequence-blank-line=preserve-one` for let bindings (#1077, @Julow).
  Preserve a blank line after `let .. in` when `sequence-blank-line` set to `preserve-one`.
  Previously, only blank lines after `;` could be preserved.
- Parse toplevel directives (#1020, @Julow).
  Allow `#directives` in `.ml` files.
  Previously, files containing a directive needed to be parsed as "use file".
  The "use file" mode is removed and `--use-file` is now the same as `--impl`.
- Don't require `--name`, require kind, forbid `--inplace`, allow `--check`, make `--enable-outside-detected-project` implicit when reading from stdin (#1018, @gpetiot)
- Parse code in docstrings (#941, @gpetiot).
  Format OCaml code in cinaps-style comments `(*$ code *)` and code blocks in documentation comments `(** {[ code ]} *)`.
- Parse documentation comments with Odoc (#721, @Julow).
  Formatting of documentation comments is more robust and support newer Odoc syntaxes.
  Internally, Odoc replaces Octavius as the documentation parser.

#### Fixed

- Fix unstabilizing comments on assignments (#1093, @gpetiot)
- Fix the default value documentation for `max-indent` (#1105, @gpetiot)
- Fix closing parenthesis exceeding the margin in function application (#1098, @Julow)
- Missing break before attributes of `Pmty_with` (#1103, @jberdine)
- Fix closing quote exceeding the margin (#1096, @Julow)
- Fix break before the closing bracket of collections (exceeding the margin) (#1073, @gpetiot)
- Fix precedence of Dot wrt Hash (#1058, @gpetiot)
- Fix break in variant type definition to not exceed the margin (#1064, @gpetiot)
- Fix newlines and indentation in toplevel extension points (#1054, @gpetiot)
- Fix placement of doc comments around extensions (#1052, @Julow)
- Inline extensions that do not break (#1050, @gpetiot)
- Add missing cut before attributes in type declarations (#1051, @gpetiot)
- Fix alignment of cases (#1046, @gpetiot)
- Fix blank line after comments at the end of lists (#1045, @gpetiot)
- Fix indexing operators precedence (#1039, @Julow)
- Fix dropped comment after infix op (#1030, @gpetiot)
- No newline if the input is empty (#1031, @gpetiot)
- Fix unstable comments around attributes (#1029, @gpetiot)
- Fix extra blank line in sequence (#1021, @Julow)
- Check functor arguments when computing placement of doc comments (#1013, @Julow)
- Fix indentation of labelled args (#1006, @gpetiot)
- Fix linebreak between or-cases with comments when `break-cases=all` (#1002, @gpetiot)
- Fix unstable unattached doc comment in records (#998, @Julow)
- Fix string literal changed (#995, @Julow)
- Fix type variable (#996, @Julow)
- Fix crash on extension sequence (#992, @gpetiot)
- Fix position of expressions regarding of comments in infix-op expressions (#986, @gpetiot)
- Escape special characters in external declaration (#988, @Julow)
- Fix parens around constrained expr with attrs (#987, @gpetiot)
- Fix the margin, and correctly breaks comments (#957, @gpetiot)
- Fix formatting of custom indexing operators (#975, @gpetiot)
- Fix position of comments of labelled arrow types (#976, @gpetiot)
- No box around inline odoc styles (#971, @gpetiot)
- Fix boxing of collection expressions/patterns (#960, @gpetiot)
- Fix crash on record expr with pack fields (#963, @Julow)
- Fix letop in subexpr (#956, @hhugo)

### Internal

- Take file kind from --name when formatting stdin (#1119, @Julow)
- Make Fmt.t abstract (#1109, @Julow)
- Future-proof Fmt API in case Fmt.t goes abstract (#1106, @emillon)
- Future-proof `Fmt` API in case `Fmt.t` goes abstract (#1106, @emillon)
- Optional names for formatting boxes in debug output (#1083, @gpetiot)
- Check ocamlformat error codes in the testsuite (#1084, @emillon)
- Clean `Translation_unit` (#1078, @gpetiot)
- Use dune file generation in test/passing/dune (#1082, @emillon)
- CI: factorize tests and check reason build (#1079, @gpetiot)
- Use short form for action in src/dune (#1076, @emillon)
- Cleanup `sequence_blank_line` (#1075, @Julow)
- CI: use a script travis-ci.sh to simplify .travis.yml (#1063, @gpetiot)
- Remove utility functions from `Fmt_ast` (#1059, @gpetiot)
- CI: use opam-2.0.5 in Travis (#1044, @XVilka)
- CI: check the build with OCaml 4.07.1 and 4.08.0 (#1036, @Julow)
- Use the same sets of options for both branches by default in `test_branch.sh` (#1033, @gpetiot)
- Fix `test_branch.sh` and CI checking of CHANGES.md (#1032, #1034, @Julow)
- Fix flag of git-worktree in `test_branch.sh` and `bisect.sh` (#1027, @gpetiot)
- Remove the `bisect_ppx` dependency and clean the `Makefile` (#1005, @Julow)
- Use a `CHANGES.md` log file again (#1023, @gpetiot)
- Support OCaml 4.09.0 (add the odoc.1.4.2 dependency) (#1024, @gpetiot)
- Update labels of issue templates (#1017, @gpetiot)
- Update labels in `CONTRIBUTING.md` (#1007, @gpetiot)
- Allow to ignore invalid options (#984, @hhugo).
  The `--ignore-invalid-option` flag is added to ignore invalid options in `.ocamlformat` files.
- Improve the documentation of `--doc-comments` (#982, @Julow)
- Remove symbolic links and change naming convention of tests (#980, @gpetiot)
- Change the type of `fmt_code` (#974, @gpetiot)
- Simplify `Makefile` (#973, @hhugo)
- Dune should not be flagged as a build dep anymore (#954, @gpetiot)

## 0.11 (2019-08-07)

- Improve: generalize API of Config_option (#952, @gpetiot)
- Improve: new 'before' value for option 'sequence-style' (#947, @gpetiot)
- Project: create issue templates (#950, @gpetiot)
- Improve: tidying up Conf.ml (#951, @gpetiot)
- Improve: parse code in comments (#934, @gpetiot)
- Fix comments' placement (do not look at loc_stack) (#923, @gpetiot)
- Doc: setting flags in .ocamlformat (#944, @gpetiot)
- Doc: enable-outside-detected-project necessary for global conf file (#948, @gpetiot)
- Fix hashbang handling (#946, @hhugo)
- Improve: support Shell-style regular expressions in .ocamlformat-ignore and .ocamlformat-enable files (#937, @gpetiot)
- Improve: force break after an infix op only if followed by another one (#935, @gpetiot)
- Fix break-separators=after-and-docked for lists and arrays (#931, @gpetiot)
- Improve: deprecate option break-string-literals and change its default value (#932, @gpetiot)
- Improve: break with labeled arrow type (#933, @gpetiot)
- Improve: disambiguate non-breaking matching structures (#857, @gpetiot)
- Improve: warning 50 handled like an internal error (#930, @gpetiot)
- Fix break-separators=after-and-docked for record patterns (#929, @gpetiot)
- Fix closing parenthesis indentation when on separate line (#928, @gpetiot)
- Improve: split the Conf.ml file (#920, @gpetiot)
- Fix position of comments after anonymous functions (#919, @gpetiot)
- Fix: comments around disabled block (#918, @hhugo)
- Fix monadic bindings (new 4.08 syntax) (#911, @gpetiot)
- Fix attribute when break-infix-before-func=false (#916, @gpetiot)
- Improve: update ocamlformat_reason opam file to 2.0 format (#913, @avsm)
- Fix attributes of modules (#910, @gpetiot)
- Fix docstrings of exceptions (#909, @gpetiot)
- Fix attribute location in Normalization (#908, @gpetiot)
- Improve: add the 'ocamlformat-file-kind' argument to the emacs hook (#905, @gpetiot)
- Improve: dunify testsuite (#881, @trRefis)
- Improve: add trailing semicolon inside record when break-separators=after-and-docked (#899, @gpetiot)
- Fix compilation with 4.06 and 4.07 (#898, @gpetiot)
- Improve: add a new way to indicate multiline delimiters (#876, @trefis)
- Fix inconsistency of break-separators=after-and-docked for record expressions (#856, @gpetiot)

## 0.10 (2019-06-25)

- Improve: align cases horizontally (#883, @gpetiot)
- Improve: option exp-grouping (#828, @gpetiot)
- Improve: synchronize Format with upstream stdlib (#885, @gpetiot)
- Improve: break-string-literals=newlines-and-wrap (#896, @gpetiot)
- Improve: specify break hint in fits_breaks (#894, @gpetiot)
- Improve: option break-before-in (#892, @gpetiot)
- Fix break-string-literals=newlines (#887, @gpetiot)
- Improve: Implement break-fun-sig without Location.is_single_line (#886, @Julow)
- Format gen_version.ml (#893, @hhugo)
- Improve: switch to ast 4.08 (#831, @hhugo)
- Fix formatting of arguments when break-fun-decl=fit-or-vertical (#884, @gpetiot)
- Test: extend max_indent test (#878, @trefis)
- Test: break_cases_normal_indent.ml is a symlink on break_cases_fit.ml (#879, @gpetiot)
- Improve unicode text length computation (#816, @gpetiot)
- Add an option to control the indentation of nested matches (#870, @trefis)
- Fix: properly interpret indicate-multiline-delimiters for if-then-elses (#874, @trefis)
- Enable warning 9 (#875, @hhugo)
- Fix unstable comment in `let%ext` (#873, @gpetiot)
- Improve: option max-indent (#841, @gpetiot)
- Improve: option nested-match=align (#827, @gpetiot)
- Fix dropped attributes in with_constraints (#846, @gpetiot)
- Fix dropped comments in list patterns and module types  (#866, @gpetiot)
- Fix comment dropped in object (#849, @gpetiot)
- Fix inconsistency of break-separators for wildcards in match cases (#855, @gpetiot)
- Improve: new options to support 'with' and 'strict_with' (ocp-indent) (#853, @gpetiot)
- Improve: .ocamlformat-enable files listing files to format when ocamlformat is disabled (#854, @gpetiot)
- Check that all locations have been considered during formatting (#864, @hhugo)
- clean Hashtbl.Poly (#862, @hhugo)
- Fix: test.sh (#858, @hhugo)
- cleanup Cmts.ml (#861, @hhugo)
- Clean: Cleanup usage of Poly (#860, @hhugo)
- Fix: rename sexp_list into list (#859, @hhugo)
- Fix vim instructions (#852, @naartjie)
- Improve: options extension-indent and stritem-extension-indent (#840, @gpetiot)
- Fix comment dropped in field alias (#848, @gpetiot)
- Fix pro position for with_constraints (#847, @gpetiot)
- Improve: finer space-around-exp options (#837, @gpetiot)
- Improve: preserve blank lines in conventional and sparse profiles (#838, @gpetiot)
- Improve: don't fit tag-only comments after val declarations (#836, @Julow)
- Improve speed with ofday_unit_tests_v1.ml (#833, @hhugo)
- Fix exception when calling String.sub (#832, @gpetiot)
- Improve: implement doc-comments and doc-comments-tag-only for every items (#746, @Julow)
- Improve: Add field-space=tight-decl (#829, @Julow)
- Improve: make Sugar.list_exp and Sugar.list_pat tail-recursive (#823, @gpetiot)
- Improve: options 'let-binding-indent', 'type-decl-indent' and 'indent-after-in' (#822, @gpetiot)
- Fix: performance issue with deep asts (#826, @hhugo)
- Improve: preserve blank lines in sequences (#814, @gpetiot)
- Improve: tidying Fmt_ast.ml (#821, @gpetiot)
- Improve: space before type constraint in record (#819, @gpetiot)
- Improve: break-cases=fit-or-vertical (#820, @gpetiot)
- Improve: remove break before ending paren for anonymous functions (#818, @gpetiot)
- Improve: preserve the position of type annotation in bindings (#815, @gpetiot)
- Improve: preserve record type annot (#812, @gpetiot)
- Fix break before ending paren (#801, @gpetiot)
- Improve: better consistency between structures and signatures (#803, @gpetiot)
- Fix let module sparse (sparse mode only for module applications) (#809, @gpetiot)
- Improve: change formatting of newtypes (#811, @gpetiot)
- Improve: break-cases-all shouldn't break nested patterns (#810, @gpetiot)
- Fix: sugarized extensions (#805, @gpetiot)
- Improve: tidying Fmt_ast (#808, @gpetiot)
- Fix cmt in empty structure (#804, @gpetiot)
- Remove dead link to preset profiles (#806, @andschwa)
- Improve: break with type constraints (#797, @gpetiot)
- Fix colon break module type functor (#802, @gpetiot)
- Improve: K&R style for if-then-else (#787, @gpetiot)
- Improve: new option break-fun-sig (#785, @gpetiot)
- Improve: indentation consistency of '<-' and `:=` (#780, @gpetiot)
- Fix: functor application and break-struct wrap incorrectly (#786, @gpetiot)
- Break after anonymous function arrow after infix op (#781, @gpetiot)
- Fix: type extension (#782, @gpetiot)
- Improve: Fmt.noop (#784, @gpetiot)
- Fix extension of value binding (#779, @chrismamo1)
- Improve: less sensitivity to concrete syntax (#767, @gpetiot)
- Fix missing space before attribute on includes (#775, @Julow)
- Improve: new option let-module (#768, @gpetiot)
- Improve: --disable-outside-detected-project is set by default (#761, @gpetiot)
- Fix weird parens break (#751, @gpetiot)
- Fix: if $XDG_CONFIG_HOME is either not set or empty, use $HOME/.config (#758, @gpetiot)
- Fix: --use-file/--impl/--intf should override file extension (#774, @gpetiot)
- Improve: less breaks for break-cases=all but correctly breaks or-patterns (#762, @gpetiot)
- Remove unecessary break on module pack constraints with with-constraints (#739, @Julow)
- Fix inconsistent break before module signature (#755, @gpetiot)
- Fix indentation of functor argument (#773, @gpetiot)
- Tidying fmt ast (#748, @gpetiot)
- Fix nested parens with no break infix before func (#760, @gpetiot)
- Provide an mli for Compat (#772) (hhugo)
-Fix non-wrapping asterisk prefixed cmts (#759, @gpetiot)
- Support for OCaml 4.08 (#763, @hhugo)
- Fix module type functor (#716, @gpetiot)
- Small cleanup (#764, @hhugo)
- Fix: update ocamlformat-help.txt (follow up on #752) (#756, @gpetiot)
- Fix module pack and functor (#735, @Julow)
- Fix grammar: it's -> its (@anmonteiro)
- Improve: support --name with --inplace (#740, @jberdine)
- Fix: dropped comments for pexp_record (#743, @hhugo)
- Improve: comments arround attributes, fix #726 (#742, @hhugo)
- Update README for new profiles (#738, @jberdine)
- Remove deprecated 'default' profile (#736, @jberdine)
- Fix extra parens around ext match (#733, @gpetiot)
- Improve: factorize with compose_module (#729, @gpetiot)
- Test: exclude gen_version.ml from test (#732, @jberdine)
- Improve: make gen_version an ocaml script (#664, @hhugo)

## 0.9.1 (2019-06-24)

- Small cleanup (#764, @hhugo)
- Support for OCaml 4.08 (#763, @hhugo)

## 0.9 (2019-03-28)

- Admin: remove CHANGES.md that was essentially git log (@jberdine)
- Admin: simplify release procedure (@jberdine)
- Build: fix ocaml version constraint, need 4.06 (@jberdine)
- Improve: make gen_version an ocaml script (@hhugo)
- Improve: fix associativity of Pexp_setfield (#725, @jberdine)
- Improve: normalize setfield and setinstvar (#720, @gpetiot)
- Remove: deprecated config file syntax parsing (#715, @jberdine)
- Improve: put the equal first for ocp-indent-compat (#717, @gpetiot)
- Fix: parse docstrings once (#713, @gpetiot)
- Improve: new profiles conventional and ocamlformat (#663, @gpetiot)
- Revert module indentation (#714, @gpetiot)
- Fix infix wrap (#691, @gpetiot)
- Fix doc comments tag only when docstring parsing disabled (#711, @gpetiot)
- Fix missing space before closing paren around function (#705, @jberdine)
- Fix documentation of doc-comments-tag-only (#710, @gpetiot)
- Improve: module-item-spacing=preserve (#538, @gpetiot)
- Add a space in "Jane Street" (#703, @kevinji)
- Fix js_source.ml (#702, @gpetiot)
- Fix space-around-collection-expressions for record/variant definitions (#670, @Julow)
- Fix extra space ifthenelse (#700, @gpetiot)
- Improve split attribute in let binding for expect test with uncaught exn (#681, @gpetiot)
- Fix empty newline before equal (#701, @gpetiot)
- Fix double cmts (#678, @gpetiot)
- Fix value binding ocp indent compat (#694, @gpetiot)
- Fix ast changed when record ident constrained (#697, @gpetiot)
- Fix incorrect ocaml code (#698, @gpetiot)
- Fix fmt for CI (#693, @gpetiot)
- Fix record break (#689, @gpetiot)
- Fix break before parens no wrap fun args (#690, @gpetiot)
- Improve: disable conf in files and attributes (#684, @gpetiot)
- Fix space around tuples (#679, @gpetiot)
- Improve: break before in for let-module construct, because of ocp-indent (#685, @gpetiot)
- Improve debugging output (#677, @hhugo)
- Improve: group open/close of modules and fix indentation (#665, @gpetiot)
- Fix constrained match in record (#676, @gpetiot)
- Fix: formatting of end line comments (#662, @gpetiot)
- Fix cmt in fun when no break infix (#668, @gpetiot)
- Add the wrap-fun-decl option (#645, @Julow)
- Improve: break the list of 'with type' module constraints (#639, @gpetiot)
- Reduce the use of Poly comparisons (#661, @hhugo)
- Improve: check flag to check whether the input files already are formatted (#657, @gpetiot)
- Fix cmt placement infix op (#651, @gpetiot)
- Restore compat with base.v0.11 (@hhugo)
- Fix: disallow '-' with other inputs (#658, @hhugo)
- Fix build on OCaml 4.06.1 (#646, @Julow)
- Fix comments on record fields (#650, @Julow)
- Fix cmts in list (#654, @gpetiot)
- Improve: If-then-else = fit-or-vertical mode (#603, @gpetiot)
- Link to man page from readme (#648, @yawaramin)
- Fix indent match branches with cmts (#644, @gpetiot)
- Build: update to base v0.12 (#642, @jberdine)
- Fit tag-only doc comments (#637, @Julow)
- Fix try%lwt indent (#638, @gpetiot)
- Fix type manifest formatting (#616, @gpetiot)
- Fix: don't include ocamlformat_diff in ocamlformat (#636, @Khady)
- fix emacs setup (#631, @Khady)
- tools/update_tests.sh --all (#632, Julow)
- Fix: don't break line before wrapping comment (#634, @gpetiot)
- Fix ignored ocamlformat attribute (#615, @gpetiot)
- Include jsoo in the tests (#618, @hhugo)
- Fix missing break before comment (#613, @gpetiot)
- Do not rely on the file-system to format sources (#611, @hhugo)
- Ignore file in .ocamlformat-ignore (#606, @hhugo)
- Improve reason support (#608, @hhugo)
- Fix: fix fmt_ast wrt strings and chars when sources are not available (#607, @hhugo)
- Fix ocamlformat_reason (#604, @hhugo)
- Fix missing break for local open record patterns (#602, @gpetiot)
- Fix regression for variants with docstrings (#601, @gpetiot)
- Fix extra break in module pack type (#600, @Julow)
- Add the doc-comments-padding option (#575, @Julow)
- Improve: externalize Sugar functions from Fmt_ast.ml (#593, @gpetiot)
- Fix typedecl attribute (#595, @gpetiot)
- Improve: less linebreaks for break-cases=fit (#536, @gpetiot)
- fix #590 (#594, @hhugo)
- Make gen_version.sh use bash. (#592, @hhugo)
- Implement box debugging (#574, @Julow)
- Break closing bracket in polymorphic variants (#583, @Julow)
- Break comment record (#580, @Julow)
- missing headers (@hhugo)
- Improve: mishandling of field_space in record exps and patterns (#587, @jberdine)
- Add empty mli for executable (#591, @hhugo)
- tests: test ocamlformat when disabled (@hhugo)
- dont reformat if disabled (@hhugo)
- remove global ref in Transation_unit (@hhugo)
- Fix Emacs (>26.1) temporary buffer not killed (#567, @ludwigpacifici)
- Improve: opam file for ocamlformat_diff to remove the bos dependency (#579, @gpetiot)
- Fix: Require Octavius version 1.2.0 (#576, @Julow)
- Improve: record fields with type constraints (#565, @jberdine)
- Fix: comments attachment (#548, @gpetiot)
- Improve: parens around constrained any-pattern (#431, @gpetiot)
- Revise formatting of compact single case matches (#552, @jberdine)
- Fix typo in help text (#553) (Wilfred Hughes)
- Improve: calculate length of comment strings using UTF8 (#550, @jberdine)
- Admin: update travis versions of ocaml and opam (#551, @jberdine)
- Fix: missing break before `; _` (#549, @jberdine)
- Improve: module item spacing in sparse mode (#546, @jberdine)
- Improve: some simplifications (#542, @gpetiot)
- Improve: remove unnecessary parens when open module (#537, @gpetiot)
- Improve: not breaking after bind/map operators (#463, @gpetiot)
- Fix suboptimal docstring formatting (#540, @gpetiot)
- amend janestreet profile (#524, @mbarbin)
- Improve: option break-separators (#461, @gpetiot)
- Fix formatting of types with ocp-indent-compat=true (#525, @gpetiot)
- Preserve shebang (#533, @gpetiot)
- Fix: remove indented empty lines between comments (#531, @gpetiot)
- Improve: remove indented empty lines separating recursive modules (#528, @gpetiot)
- add update_tests.sh (#529, @gpetiot)
- Improve: space around collection expressions (#527, @gpetiot)
- Improve: remove more spaces inside parenthesized multiline constructs (#526, @gpetiot)
- Disable docstring parsing for external tests (#518, @gpetiot)
- Fix odoc normalize (#520, @gpetiot)
- Better docstring error msgs (#519, @gpetiot)
- Fix odoc seps (#511, @gpetiot)
- Improve: option 'single-case' (#426, @gpetiot)
- Add a parens-tuple-patterns configuration option (#498, @NathanReb)
- Fix: comments should not be parsed for diff (#509, @gpetiot)
- Fix: odoc refs (#510, @gpetiot)
- Fix formatting of or-patterns in try expressions (#503, @NathanReb)
- Test: improve test_branch.sh to allow different config for branch (#496, @jberdine)
- Improve: option 'parens-ite' (#430, @gpetiot)
- fix break-struct for toplevel items (not in a struct) (#497, @gpetiot)
- Fix: breaking of variant types (#486, @gpetiot)
- Improve: autocompletion of git branch names for test_branch.sh (#485, @gpetiot)
- Fix: Sanitize docstring check (#481, @gpetiot)
- Improve the formatting of lists in doc comments (#480, @jeremiedimino)
- Add PR test script and update contributing guidelines with expected usage (#479, @jberdine)
- Fix break struct natural (#443, @gpetiot)
- Fix: disable-outside-detected-project: disable ocamlformat when no .ocamlformat file is found (#475, @gpetiot)
- Improve: error message when docstrings move (#446, @gpetiot)
- Improve: print-config prints all options (#465, @gpetiot)
- Ocamldoc docstrings (#460, @gpetiot)
- Doc: disable-outside-detected-project (#468, @gpetiot)
- Improve: shorter output of regtests (#469, @gpetiot)
- Admin: add code of conduct and copyright headers to build and package system (@jberdine)
- Improve: add license header for tools/ocamlformat-diff/ocamlformat_diff.ml (#466, @gpetiot)
- Build: a few simplifications enabled by dune 1.1.1 (#457, @jberdine)
- Improve: record fields with attributes and docs in type definitions (#458, @jberdine)
- Fix exception comments (#459, @gpetiot)
- Ocamlformat diff tool (#450, @gpetiot)

## 0.8 (2018-10-09)

- Improve: set break-sequences in sparse and compact profiles (#455, @jberdine)
- Improve: keep a space inside tuples parens (#453, @gpetiot)
- Improve: --root option to isolate configuration files (#402, @gpetiot)
- Fix: missing parens around partially-applied `::` (#452, @jberdine)
- Fix: parens around expressions with attributes (#441, @gpetiot)
- Build: do not execute shell scripts directly in build (#448, @dra27)
- Add: read ocp indent config files (#445, @gpetiot)
- Improve: option 'break-sequences' (#434, @gpetiot)
- Improve: option 'no-indicate-multiline-delimiters' to have less whitespaces (#429, @gpetiot)
- Fix: outdent before arrow (#444, @gpetiot)
- Improve: User documentation (#449, @gpetiot)
- Improve: option 'cases-exp-indent' to adjust indent (#428, @gpetiot)
- Add: compact and sparse profiles (#408, @jberdine)
- Improve: explicit error message in case of 'permission denied' error (#425, @gpetiot)
- Fix: comment stabilization in Pexp_override (#422, @gpetiot)
- Fix: corner case while formatting type variables (#440, @hhugo)
- Fix: many missing comments (#418, @hhugo)
- Fix: asserts and attributes (#414, @hhugo)
- Fix: extension and attribute (#417, @hhugo)
- Improve: support for `function%ext` (#416, @hhugo)
- Fix: Inconsistent spacing around comments in signatures vs structures (#437, @gpetiot)
- Improve: better error with location when comment dropped (#401, @gpetiot)
- Fix doc comments (#413, @hhugo)
- Improve: use input_name for error messages (@hhugo)
- Improve: break after inherit (@hhugo)
- Fix: aliases inside constructor declaration (#424, @gpetiot)
- Fix: broken invariant for Pmod_unpack (#421, @gpetiot)
- Fix: print error details in debug mode only (#420, @hhugo)
- Fix: mark_parenzed_inner_nested_match (@hhugo)
- Improve: tune the janestreet profile (@hhugo)
- Improve: disable ocamlformat if no dot ocamlformat in the project (#391, @hhugo)
- Improve: new option to control spacing around let bindings (#344, @hhugo)
- Fix: prec of string/array sugar (#381, @hhugo)
- Fix: lost comment in constraint expression (#400, @gpetiot)
- Fix: lost cmt near functor (#399, @gpetiot)
- Improve: preset profiles (default & janestreet) (#390, @gpetiot)
- Improve: try to fit simple list/array elements on a single line (#375, @gpetiot)
- Fix: bad comment spacing with module-item-spacing=compact (#395, @gpetiot)
- Fix: dropped comment in revapply of extension (#394, @gpetiot)
- Improve: let-and structures spacing depends on module-item-spacing (#367, @gpetiot)
- Fix: consecutive prefix operator (#386, @hhugo)
- Fix: invalid (#383, @hhugo)
- Fix: lazy and alias (#388, @hhugo)
- Improve: main loop and error reporting (#389, @hhugo)
- Fix: exposed_left_typ (#385, @hhugo)
- Fix: rec functor (#382, @hhugo)
- Fix: `while%ext`/`for%ext` (@hhugo)
- Fix: more on class (@hhugo)
- Fix: invalid syntax on class (@hhugo)
- Improve: follow XDG for global config files (@gpetiot)
- Improve: add support for bigarray sugar index operator (@hhugo)
- Add: support reading input from stdin (#353, @bkase)
- Fix: the precedence of options (@gpetiot)
- Improve: doc of config option choice alternatives (#354, @jberdine)
- Improve: string formatting (@hhugo)

## 0.7 (2018-08-23)

- Improve: simplify setting option defaults, slight --help improvement (#350, @jberdine)
- Improve: update emacs mode to use replace-buffer-contents (#345, @hhugo)
- Improve: add option to not force-break structs (#346, @jberdine)
- Improve: move 'formatting' options into separate section (#348, @gpetiot)
- Improve: fun sugar (@hhugo)
- Improve: add option to omit open lines between one-line module items (#303, @gpetiot)
- Fix: infix ops (@hhugo)
- Improve: reformat files with no locations (@hhugo)
- Improve: better error when max-iters = 1 (@hhugo)
- Improve: breaking before arrows in pattern match cases (@jberdine)
- Improve: no parens for trailing 'not' (@hhugo)
- Improve: missing break hint, fix #331 (@hhugo)
- Improve: comments before match (#330, @jberdine)
- Fix: missing comments (@hhugo)
- Fix: missing attributes due to sugar (@hhugo)
- Fix: parens non trivial rhs for `#` infix ops (@hhugo)
- Improve: let-module-in break before `in` (#328, @jberdine)
- Improve: sugar for nestest module_with (@hhugo)
- Improve: attributes on let bindings (#324, @jberdine)
- Improve: wrapping of functor args in type declaration (#315, @gpetiot)
- Fix: comments attachment with infix ops (@hhugo)
- Fix: comments attachment with Pexp_fun (@hhugo)
- Fix: docstrings as attributes (@hhugo)
- Improve: refactor and improve documentation of options (#302, @gpetiot)
- Improve: error reporting in emacs integration (#304, @jberdine)
- Improve: pexp_open as final arg of infix op (#314, @jberdine)
- Fix: missing parens around labeled record pattern arg (@jberdine)
- Fix: missing attributes (@hhugo)
- Fix: duplicated (x3) attributes in pexp_letmodule (@hhugo)
- Improve: allow to locally disable ocamlformat (@hhugo)
- Improve: corner case indentation of fun -> function (#312, @jberdine)
- Improve: labeled, optional, and default args (@jberdine)
- Improve: punning default arg with type constraint (@jberdine)
- Improve: add options to controls various spaces (#284, @hhugo)
- Improve: add option to disable wrapping fun args (#283, @hhugo)
- Improve: add option --break-cases to break each pattern-matching case (#251, @gpetiot)
- Improve: rename --nested-parens option (@hhugo)
- Improve: ws before colon for constraint (#293, @hhugo)
- Improve: option to choose where to parens nested match (@hhugo)
- Improve: always parens nested match (even the right most one) (@hhugo)
- Improve: always break for let_and contruct (@hhugo)
- Fix: missing comments (@hhugo)
- Improve: Add option to preserve style of local module open (#267, @gpetiot)
- Improve: preserve extension point formatting (@hhugo)
- Improve: make double semi consistent between implementation and use_file (#292, @hhugo)
- Improve: configure ocamlformat using attributes (@hhugo)
- Improve: extension point (@hhugo)
- Improve: break in type declaration for variant and record (#280, @hhugo)
- Fix: memory leak (@hhugo)
- Test: add ocaml compiler to test suite, and improve `make -C test` (@jberdine)
- Fix: unary operator `+`/`-` (@hhugo)
- Fix: doc comments in class (@hhugo)
- Fix: ocaml bug, sort fields (@hhugo)
- Improve: empty mod with comments (@hhugo)
- Improve: disable warning generated by the lexer in quiet mode (@hhugo)
- Fix: record update (@hhugo)
- Fix: rec let binding and attribute (@hhugo)
- Fix: punning (@hhugo)
- Fix: let open and constraint (@hhugo)
- Fix: not extension sugar when attribute (@hhugo)
- Fix: no-comment-check missing case (@hhugo)
- Fix: string literal (@hhugo)
- Fix: format of infix in presence of `%;` (@hhugo)
- Fix: let binding and type annot (@hhugo)
- Fix: binding when lhs is an extension (@hhugo)
- Fix: pat constraint in payload (@hhugo)
- Fix: let rec with extension (@hhugo)
- Fix: comments (@hhugo)
- Fix: comments in fmt_case (@hhugo)
- Fix: comments around Longident (@hhugo)
- Fix: missing comment for pmty_with (@hhugo)
- Improve: add option to disambiguate infix precedence with parens (@jberdine)
- Improve: `not` when infix op arg (@jberdine)
- Improve: add a flag to skip all checks about comments (@hhugo)
- Improve: breaking of module ident/unpack/extension exps (#269, @jberdine)
- Fix: literal sub-exps with attributes (@jberdine)
- Fix: many fixes regarding attributes (@hhugo)
- Improve: preserve formatting of block comments (#255, @hhugo)
- Improve: breaking of applications with long literal list args (#258, @jberdine)
- Fix: sugar functor (@hhugo)
- Fix: type alias in variant (@hhugo)
- Improve: formatting of comments (@jberdine)
- Fix: prefix operators (@hhugo)
- Fix: exception declarations (@hhugo)
- Fix: doc comments in structure (#250, @hhugo)
- Fix: add parens around pat with trailing attr (@hhugo)
- Fix: let binding and Ppat_or (@hhugo)
- Fix: be more permissive with pattern (@hhugo)
- Fix: fix string_literal with when its location includes its attribute (#244, @hhugo)
- Improve: improve errors returned to the user. (#243, @hhugo)
- Fix: missing comments for Pexp_construct (#240, @hhugo)
- Fix: multiple fixes around classes (#242, @hhugo)
- Fix: comments in empty () and [] (#241, @hhugo)
- Fix: support empty variant (#239, @hhugo)
- Fix: add missing attribute (#238, @hhugo)
- Fix: be more permissive with ppat_interval (#237, @hhugo)
- Improve: remove trailing ws (#210, @hhugo)
- Improve: attributes on type declarations (#232, @jberdine)
- Improve: breaking of infix Array and String get and set ops (#233, @jberdine)
- Fix: attributes and doc comments (#221, @hhugo)
- Improve: spacing of module unpack (#230, @jberdine)
- Improve: no parent for new (@hhugo)
- Fix: Revert: Improve: remove redundant parens around application operators (@hhugo)
- Improve: array alignment (#226, @hhugo)
- Improve: nested array infix ops (#225, @hhugo)
- Fix: is_adjacent and remove [~inclusive] from [Source.string_between] (@hhugo)
- Fix: Cmts.CmtSet.split (@hhugo)
- Improve: Allow comments inside empty delimited "things" (#223, @hhugo)
- Fix: Source.ends_line (#222, @hhugo)
- Improve: empty struct and sig (#217, @hhugo)
- Improve: support for toplevel files (#218, @hhugo)
- Fix: string literal, fix #214 (#219, @hhugo)
- Improve: more tuning for functors (@hhugo)
- Improve: sugar for functor type with multiple args (@hhugo)
- Improve: sugar for functors with multiple args (@hhugo)
- Improve: module type with (@hhugo)
- Improve: break before with/and type (@hhugo)
- Improve: break between fun args (@hhugo)
- Improve: module unpacking (#215, @hhugo)
- Improve: for & while loops (#211, @hhugo)
- Fix: attributes on ite (#209, @hhugo)
- Fix: partially applied (+) and (-) (#208, @hhugo)
- Fix: polymorphic variant (#202, @hhugo)
- Fix: parens with lazy pat (fix #199) (#201, @hhugo)
- Improve: omit excess indentation of `function` cases (@jberdine)
- Improve: extensions with payloads of multiple structure items (@jberdine)
- Improve: parenthesization and attribute placement of if-then-else (@jberdine)
- Fix: do not attach comments to docstrings (@jberdine)
- Fix: short syntax for extensions (#193, @hhugo)
- Fix: missing attrs for pcl_fun (@hhugo)
- Fix: pos of attribute for functors (@hhugo)
- Fix: () module only if not attrs (@hhugo)
- Fix: missing attrs for object (@hhugo)
- Fix: no short form of extension with attribs (@hhugo)
- Fix: normalization for Pexp_poly and Pexp_constraint (#190, @hhugo)
- Fix: some parenthesization context checks (#189, @hhugo)
- Fix: attributes on fun expressions (@jberdine)
- Fix: extensions with multiple module-level eval expressions (#185, @jberdine)
- Fix: functor & apply (#182, @hhugo)
- Fix: module rec with (@hhugo)
- Fix: more parens in pat_constraint (@hhugo)
- Improve: tuple & constraint (@hhugo)
- Improve: empty module (#178, @hhugo)
- Fix: extensible variant (#177, @hhugo)
- Fix: index operator (#176, @hhugo)
- Improve: empty module sig (@hhugo)
- Fix: add attributes to module signature (@hhugo)
- Add: support for objects and classes (#173, @hhugo)
- Improve: remove some redundant parens around tuple types (@jberdine)
- Fix: args in let bindings (@hhugo)
- Improve: let module%ext (@hhugo)
- Fix: infix op in alias (@hhugo)
- Fix: extensions pat (@hhugo)
- Fix: limit use of short syntax for extensions (@hhugo)
- Improve: allow break after Psig_include (@jberdine)
- Fix: { (a; b) with a; b } (@hhugo)
- Fix: with type [longident] (@hhugo)
- Fix: attributes on polymorphic variants (@hhugo)
- Fix: attribute in let bindings (@hhugo)
- Fix: private in extensible variant (@hhugo)
- Fix: gadt in extensible variant (@hhugo)
- Fix: missing parens in list pattern (@hhugo)
- Improve: format [new e] like an apply (@hhugo)
- Fix: parens for constraint (@hhugo)
- Fix: avoid emitting `>]` which is an unparsable keyword (#171, @hhugo)
- Fix: misplaced comments on `module type of` (@jberdine)

## 0.6 (2018-04-29)

### Features

- Add: option to align all infix ops (#150, @hhugo)
- Add: option to attempt to indent the same as ocp-indent (#162)
- Add: option for no discretionary parens for tuples (#157, @hhugo)
- Add: alternative format for if-then-else construct (#155, @hhugo)
- Add: option to customize position of doc comments (#153, @hhugo)

### Fixed

- Fix: dropped item attributes on module expressions
- Fix: toplevel let%ext (#167, @hhugo)
- Fix: parens around type alias & empty object type (#166, @hhugo)
- Fix: missing comments for [let open] (#165, @hhugo)
- Fix: missing comments in ppat_record (#164, @hhugo)
- Fix: check_typ wrt constraint on module type (#163, @hhugo)
- Fix: let binding with constraint (#160, @hhugo)
- Fix: handle generative functor type (#152, @hhugo)

### Formatting improvements

- Improve: remove redundant parens around application operators
- Improve: parenthesize and break infix constructors the same as infix ops
- Improve: consider prefix ops and `not` to be trivial if their arg is
- Improve: align arrow type args and do not wrap them (#161)
- Improve: formatting for multiple attributes (#154, @hhugo)
- Improve: keep the original string escaping (#159, @hhugo)
- Improve: discretionary parens in patterns (#151, @hhugo)
- Improve: breaking of infix op arguments
- Improve: consider some extensions to be "simple"
- Improve: punning (#158, @hhugo)
- Improve: force break of let module/open/exception/pats (#149, @hhugo)

### Build, packaging, and testing

- Add support for bisect (#169, @hhugo)
- Exclude failing tests from `make -C test`

## 0.5 (2018-04-17)

### Features

- Add: support for `new%js` (#136, @hhugo)
- Add: support for Ptyp_object (#104, @smondet)
- Use original filename when given in error messages. (#96, @mbarbin)

### Fixed

- Fix: allow extensions in types (#143, @hhugo)
- Fix: parens on symbol type constructor
- Fix: parenthesization of '!=' partial application as a prefix op (#126, @hhugo)
- Fix: parens around Ppat_constraint under Pexp_match or Pexp_try (#124, @hhugo)
- Fix: parenthesization of tuple args of variant type declarations (#122, @hhugo)
- Fix: missing parens around list inside Constr pattern (#123, @hhugo)
- Fix: incorrect breaking of long strings (#130, @hhugo)
- Fix: missing parens inside array literal (#129, @hhugo)
- Fix: attributes on arguments of function (#121, @hhugo)
- Fix: floating docstrings within a type declaration group
- Fix: missing parens in sugared Array.set
- Fix: missing attributes on patterns
- Fix: is_prefix_id for != (#112, @hhugo)
- Fix: missing parens around module value types in signatures (#108, @hcarty)
- Fix: floating docstrings within a value binding group
- Fix: missing attributes on extension points (#102, @hcarty)
- Fix: extensible variants with aliases (#100, @hcarty)
- Fix: several issues with extension sequence expressions
- Fix: generative functors
- Fix: preserve files with an empty ast (instead of failing) (#92, @mbarbin)
- Fix: missing extension on Pexp_sequence
- Fix: missing docstrings and attributes on types
- Fix: missing parens around sugared Array and String operations
- Fix: missing parens around Pexp_newtype
- Fix: missing parens around Ppat_constraint, Ppat_or, and Ppat_unpack
- Fix: dropped space when string wrapped between spaces
- Fix: repeated ppx extension on mutual/recursive let-bindings (#83, @mbarbin)
- Fix: dropped comments on Pmty_typeof
- Fix: missing parens around Ppat_unpack under Ppat_constraint

### Formatting improvements

- Improve: two open lines following multiline definition only with --sparse (#144)
- Improve: indent rhs of ref update (#139, @hhugo)
- Improve: no parens around precedence 0 infix ops (refines #115) (#141, @hhugo)
- Improve: support `(type a b c)` (#142, hhugo)
- Improve: no parens for `{ !e with a }` (#138, @hhugo)
- Improve: no parens for constr inside list pattern. (#140, @hhugo)
- Improve: generative functor applications (#137, @hhugo)
- Improve: omit parens around lists in local opens (#134, @hhugo)
- Prepare for ocaml#1705 (#131, @hhugo)
- Improve: comment wrapping for dangling close
- Improve: if-then-else conditions that break
- Improve: suppress spurious terminal line break in wrapped strings
- Improve: parens for nested constructors in pattern (#125, @hhugo)
- Improve: remove duplicate parens around Ptyp_package
- Improve: indentation after comment within record type declaration
- Improve: add discretionary parens on nested binops with different precedence
- Improve: empty module as functor argument (#113, @hhugo)
- Improve: indentation of multiple attributes
- Improve: attributes on short structure items
- Improve: attributes on type declarations
- Improve: tuple attribute args
- Improve: parenthesization of Ppat_or
- Improve: determination of file kind based on provided name
- Improve: extension on the let at toplevel: e.g. let%expect_test _ (#94, @mbarbin)
- Improve: constraints in punned record fields (#93, @mbarbin)
- Improve: nullary attributes
- Improve: Ppat_tuple under Ppat_array with unnecessary but clearer parens
- Improve: breaking of arguments following wrapped strings

### Build, packaging, and testing

- Simplify using `(universe)` support in jbuilder 1.0+beta20
- Add some regtests (#135, @hhugo)
- Upgrade to Base v0.11.0 (#103, @jeremiedimino)
- Add Travis CI script
- Fix: build [make reason] (#97, @mbarbin)
- Simplify Makefile due to jbuilder 1.0+beta18

## 0.4 (2018-02-24)

### Features

- Wrap lines in string literals, comments and docstrings
- Improve char escaping to ascii / uniform hexa / utf8 (#73)
- Add support for `Pexp_new` expressions (#76, @smondet)
- Add support for `Pexp_send _` expressions (#72, @smondet)
- Add options to format chars and break strings (#70, @smondet)
- Formatting of %ext on if/while/for/match/try/; (#63, @hcarty)
- Disable formatting with [@@@ocamlformat.disable] (#66, @hcarty)

### Formatting improvements

- Improve sequences under if-then-else with unnecessary but safer parens
- Improve optional arguments with type constraints
- Improve let-bound functions with type constraints
- Improve newtype constraints in let-bindings
- Improve placement of exception docstrings

### Fixed

- Fix missing break hint before comment on sugared `[]`
- Fix formatting of [%ext e1]; e2 (#75, @hcarty)
- Fix missing parens around let exception, let module, for, while under apply
- Fix missing parens under alias patterns
- Fix placement of attributes on extension constructors
- Fix missing parens around unpack patterns
- Fix let-bindings with pattern constraints
- Fix mutually recursive signatures

## 0.3 (2017-12-21)

### Features

- Output to stdout if output file omitted

### Fixed

- Fix Ppat_any value bindings
- Fix missing parens around variant patterns in fun arg
- Fix position of comments attached to end of sugared lists
- Fix missing comments on module names
- Fix package type constraints
- Fix first-class module alias patterns
- Fix first-class module patterns in let bindings
- Fix missing parens around Ptyp_package under Psig_type
- Fix missing "as" in Ptyp_alias formatting (@hcarty)
- Fix let bindings with constraints under 4.06

### Formatting improvements

- Improve line breaking of or-patterns
- Improve placement of comments within pattern matches
- Improve clarity of aliased or-patterns with parens
- Improve matches on aliased or-patterns
- Improve infix applications in limbs of if-then-else
- Improve final function arguments following other complex arguments
- Improve consistency of paren spacing after Pexp_fun
- Improve sugar for Pexp_let under Pexp_extension
- Improve sugar for newtype
- Improve first-class module expressions
- Improve indentation when comments are sprinkled through types
- Do not add open line after last binding in a structure

### Build and packaging

- Simplify build and packaging, and adopt some common practices
- Add Warnings.Errors argument for < 4.06 compatibility (@hcarty)
- Update base to v0.10.0 (@hcarty)

## 0.2 (2017-11-09)

### Features

- Check fatal warnings not only in inplace mode

### Documentation

- Improve doc of --no-warn-error
- Mention object language not implemented
- Update documentation of --output

### Fixed

- Colon instead of arrow before type for GADT constructors with no arguments (@mbouaziz)
- Fix some dropped comments attached to idents
- Fix missing parens around Ppat_alias under Ppat_variant
- Fix module type constraints on functors
- Fix broken record field punning
- Fix broken docstring attachment with multiple docstrings
- Fix missing parens around application operators
- Fix missing parens around Ppat_or under Ppat_variant
- Fix missing/excess parens around Pexp_open under Pexp_apply/Pexp_construct
- Fix duplicated attributes on Pexp_function
- Fix missing parens around Ptyp_package under Pstr_type
- Add '#' to the list of infix operator prefix (@octachron)
- Do not add space between `[` and `<` or `>` in variant types
- Add a break hint before "constraint" in a type def (@hcarty)

### Formatting improvements

- Remove unnecessary parens around Pexp_tuple under Pexp_open
- Improve single-case matches
- Improve constructor arguments
- Remove unnecessary parens around match, etc. with attributes
- Fix missing parens around constraint arg of variant type
- Fix missing parens on left arg of infix list constructor
- Fix missing parens around arrow type args of variant constructors
- Fix missing parens around type of constraints on module exps

### Build and packaging

- Separate Format patch into ocamlformat_support package
- Fix test script
- Unbreak build of ocamlformat_reason.ml (@mroch)
- Improve opam installation (JacquesPa)
- Install emacs support via opam package

## 0.1 (2017-10-19)

- Initial release.
