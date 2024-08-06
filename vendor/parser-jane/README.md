# parser-jane
This directory contains a direct copy of files from Jane Street's compiler's
parser. The code is not used in `ocamlformat` at all; it only exists as a base
to perform a merge off of.

## How to merge changes from the compiler's parser
### "Manually"
First, in the `vendor/` directory, generate patchfiles
```
diff -ruN parser-jane/for-parser-standard/ parser-standard/ > changes-parser.patch
diff -ruN parser-jane/for-ocaml-common/ ocaml-common/ > changes-common.patch
```
Then, update the files in `parser-jane/` by running the update script
```
./parser-jane/update.sh {path-to-flambda-backend}
```
Finally, create the new `parser-standard/` and `ocaml-common/` by copying from
`parser-jane/` and applying the patchfiles
```
rm -rf parser-standard/ ocaml-common/
cp -r parser-jane/for-parser-standard parser-standard/
cp -r parser-jane/for-ocaml-common ocaml-common/
patch -p1 -d parser-standard/ < changes-parser.patch
patch -p1 -d ocaml-common/ < changes-common.patch
rm changes-parser.patch
rm changes-common.patch
```

### With [repatch.sh]
You can also just run the repatch script to do all the above steps automatically.
```
./parser-jane/repatch.sh {path-to-flambda-backend}
```
