# New Unicode release

First you will an install of [`uucp`] that supports the new Unicode
version installed.

You can then bump the Unicode release number at the top of the `B0.ml`
file. Veryify that everything is as expected with:

    b0 -- unicode-version

Update the opam file with: 

    b0 -- .opam file > opam

Before the formal Unicode release date check the proposed update of
[UAX29] and [UAX14]. If the rules need to be adjusted then do so.

Compile Uuseg against the uucp and check the reference tests (see
below).

[UAX29]: https://www.unicode.org/reports/tr29/
[UAX14]: https://www.unicode.org/reports/tr14/
[`uucp`]: https://erratique.ch/software/uucp

# Reference tests

To test the package on the reference segmentation tests you must 
download a copy of the tests to:

    test/LineBreakTest.txt
    test/GraphemeBreakTest.txt
    test/WordBreakTest.txt
    test/SentenceBreakTest.txt

These file are ignored by git. If you have `curl` in your `PATH`
you can simply issue:

    b0 -- download-tests

this downloads the tests for the Unicode version mentioned in `B0.ml`. 

You can then check them via: 

    b0 -- test
