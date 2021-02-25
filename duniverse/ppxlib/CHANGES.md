0.22.0 (04/02/2021)
-------------------

- Bump ppxlib's AST to 4.12 (#193, @NathanReb)

0.21.0 (22/01/2021)
-------------------

- Fix ppxlib.traverse declaration and make it a deriver and not a rewriter
  (#213, @NathanReb)
- Driver (important for bucklescript): handling binary AST's, accept any
  supported version as input; preserve that version (#205, @pitag-ha)

- `-as-ppx`: take into account the `-loc-filename` argument (#197, @pitag-ha)

- Add input name to expansion context (#202, @pitag-ha)

- Add Driver.V2: give access to expansion context in whole file transformation
  callbacks of `register_transformation` (#202, @pitag-ha)

- Driver: take `-cookie` argument into account, also when the input is a
  binary AST (@pitag-ha, #209)

- `run_as_ppx_rewriter`: take into account the arguments
  `-loc-filename`, `apply` and `dont-apply` (#205, @pitag-ha)

- Location.Error: add functions `raise` and `update_loc`
  (#205, @pitag-ha)

0.20.0 (16/11/2020)
-------------------

- Expose `Ppxlib.Driver.map_signature` (#194, @kit-ty-kate)

0.19.0 (23/10/2020)
-------------------

- Make ppxlib compatible with 4.12 compiler (#191, @kit-ty-kate)

0.18.0 (06/10/2020)
-------------------

- Bump ppxlib's AST to 4.11 (#180, @NathanReb)

0.17.0 (17/09/2020)
-------------------

- Add accessors for `code_path` and `tool_name` to `Expansion_context.Base`
  (#173, @jberdine)
- Add `cases` methods to traversal classes in `Ast_traverse` (#183, @pitag-ha)

0.16.0 (18/08/2020)
-------------------

- `Driver.register_transformation`: add optional parameter `~instrument`
  (#161, @pitag-ha)
- Add missing `Location.init` (#165, @pitag-ha)
- Upgrade to ocaml-migrate-parsetree.2.0.0 (#164, @ceastlund)

0.15.0 (04/08/2020)
-------------------

- Remove `base` and `stdio` dependencies (#151, @ceastlund)

- Update README and opam description (#155, @jeremiedimino)

- Fix `Driver.partition_transformation` (#156, @NathanReb)

- Implement name mangling for `ppxlib_traverse` (#159, @ceastlund)

0.14.0 (08/07/2020)
-------------------

- Bump ppxlib's AST to 4.10 (#130, @NathanReb)

- Remove omp_config from `Expansion_context` and replace it with `tool_name`
  (#149, @NathanReb)

- Change undocumented `Ppxlib.Driver.map_structure` to return a ppxlib's
  `structure` instead of a `Migrate_parsetree.Driver.some_structure`.
  (#153, @NathanReb)

0.13.0 (04/15/2020)
-------------------

- Add 'metaquot.' prefix to disambiguate metaquote extensions (#121,
  @ceastlund)

- Bump dune language to 1.11 since the cinaps extension requires at
  least Dune 1.11 (#126, @diml)

0.12.0 (01/07/2020)
-------------------

- Support for OCaml 4.10 (#109, @xclerc)

0.11.0 (01/07/2020)
-------------------

- Invariant check on locations (#107, @trefis)

0.10.0 (11/21/2019)
-------------------

- Do not produce a suprious empty correction when deriving_inline
  expands into an extension that undergoes further expansion (#86,
  @aalekseyev)

- Add `Ppxlib.Quoter`. This module allows to generate hygienic code fragments in
  the spirit of ppx_deriving. (#92, @rgrinberg)

- Allow for registering derivers on module type declarations. (#94, fix #83,
  @rgrinberg)

- Fix parsing long idenitifiers. (#98, @NathanReb)

0.9.0
-----

- Bump AST to 4.08 (#80, @xclerc)

0.8.1
-----

### Fixed

- Report errors according to the value of `OCAML_ERROR_STYLE` and
  `OCAML_COLOR` in the standalone driver (#83, @NathanReb)

0.6.0
-----

- Set `Location.input_name` to the original filename when reading a
  binary AST (#.., @diml)

0.5.0
-----

- Add an `(** @inline *)` to the include generated when silencing
  warning 32 (#58, @trefis)

- Add `Ppxlib.mk_named_sig` and `Ppxlib.is_polymorphic_variant` (#57,
  @trefis)

0.4.0
-----

- Do not report errors about dropped or uninterpreted attributes
  starting with `_` (#46, fix #40, @diml)

- Fix he `special_function` rule for dotted operators and allow
  `Longident.parse` to parse dotted operators (#44, @Octachron)

- Port to `dune` and remove use of bash (#45, @rgrinberg)

- Ignore all attribites starting with `_` (#46, @diml)

- Reserve the `reason` and `refmt` namespaces (#46, @diml)

- Reserve the `metaocaml` namespace (#50, @rgrinberg)

- Fix attribute extraction for Otag/Rtag (#51, @xclerc)

- Do not relocate files unless `-loc-filename` is passed (#55, @hhugo)

- Preserve the filename in the output (#56, @hhugo)

0.3.1
-----

- Add `Attribute.declare_with_name_loc` (#33, @diml)

- Let the tool name pass thought when used as a -ppx (#41, @diml)

- Update the AST to 4.06 (#8, @xclerc)

0.3.0
-----

- Update the AST to 4.06 (#8, @xclerc)

- Deprecate old references to type_conv in argument and rewriter names
  and add new ones mentioning deriving instead (#7, #9 @xclerc)

- Fix compatibility with `-safe-string` (#10, @hhugo)

- Restore tests (#11, @xclerc)

- Allow to set the suffix of corrected files (#15, @diml)

- Restore compatibility with OCaml 4.04.x (#16, @xclerc)

0.2.0
-----

- Make sure to import command line arguments registered with
  ocaml-migrate-parsetree (#5, @diml)

- Fix an issue where cookies set from the command line sometimes
  disappeared (#6, @diml)

0.1.0
-----

Initial release.
