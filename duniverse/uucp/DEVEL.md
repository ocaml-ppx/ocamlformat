# New Unicode release

The files `src/uucd_*_data.ml` contain generated data. These files need 
to be regenerated on new Unicode releases, as well as the `opam` file.

In order to do so you need to install an updated version of the [uucd]
OCaml package which is capable of reading the latest XML Unicode
character database.

You can then bump the Unicode release number at the top of the `B0.ml`
file. Verify that everything is as expected with:

    b0 -- unicode-version

You should then download a copy of the XML Unicode character database
to the `support/ucd.xml` file which is ignored by git. If you have
`curl` and `unzip` in your `PATH` you can simply issue:

    b0 -- download-ucdxml

You can now proceed to generate the `src/uunf_data.ml` and update the opam file
file by issuing:

    b0 -- generate-data
    b0 -- .opam file > opam

[uucd]: http://erratique.ch/software/uucd


# Tests and performance

To check Uucp against the data of the Unicode character database to
ensure they agree make sure you have a copy of the XML Unicode
character database in `support/ucd.xml` (use `b0 -- download-ucdxml`
or specify it on the cli) and run:

    b0 -- test

If you are tweaking the datastructures, the performance lookup of Uucp 
can be tested with: 

    time $(b0 --path -- pref)
  
See `b0 -- perf --help` for more options.
