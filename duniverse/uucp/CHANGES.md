v13.0.0 2020-03-10 La Forclaz (VS)
----------------------------------

- Unicode 13.0.0 support. Adds the `Emoji` module with the new emoji
  properties.
- Reduce data size by improving structure sharing. Thanks to David Kaloper
  Meršinjak for the help.
- Handle `Pervasives` deprecation.
- Require OCaml >= 4.03.0

v12.0.0 2019-03-07 La Forclaz (VS)
----------------------------------

- Unicode 12.0.0 support.

v11.0.0 2018-06-06 Zürich
-------------------------

- Unicode 11.0.0 support.
- Add support for the Join_Control property (`Uucp.Func.is_join_control`)
  and the Hangul_Syllable_Type property (`Uucp.Hangul.syllable_type`).

v10.0.1 2017-06-21 Cambridge (UK)
---------------------------------

- Fix wrong build dependencies of ucharinfo. Thanks to Andreas Hauptmann
  for the report.

v10.0.0 2017-06-20 Cambridge (UK)
---------------------------------

- Unicode 10.0.0 support.
- Add ucharinfo tool to query Unicode character information on the cli.
- OCaml 4.05 compatibility (removal of `Uchar.dump`)

v2.0.0 2016-11-23 Zagreb
------------------------

- Unicode 9.0.0 support.
- OCaml standard library `Uchar.t` support.
  - Removes and substitutes `type Uucp.uchar = int` by the (abstract)
    `Uchar.t` type. `Uchar.{of,to}_int` allows to recover the previous
    representation.
  - Removes the `Uucp.Uchar` module, corresponding functionality can
    be found in `Uchar`.
- Safe string support.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v1.1.0 2015-11-20 Cambridge (UK)
--------------------------------

- Add support for the East Asian width property (`Uucp.Break.east_asian_width`).
- Add the non-normative, heuristic function `Uucp.Break.tty_width_hint`.
  Thanks to David Kaloper for the contribution.

v1.0.0 2015-06-17 Cambridge (UK)
--------------------------------

- Updated for Unicode 8.0.0
  Incompatible release, new variants cases are introduced, see commit
  adbb5efc036 for details.

v0.9.1 2014-12-23 Cugy (VD)
---------------------------

- Add access to the `Line_break`, `Grapheme_cluster_break`, `Word_break` and
  `Sentence_break` Unicode properties. See the `Uucp.Break` module.
- Improvements and fixes to the minimal Unicode Introduction. 


v0.9.0 2014-06-28 Cambridge (UK)
-------------------------------

First release. Part of the work was sponsored by OCaml Labs.
