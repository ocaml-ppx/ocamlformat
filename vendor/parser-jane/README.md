# parser-jane
This directory contains a direct copy of files from Jane Street's compiler's
parser. The code is not used in `ocamlformat` at all; it only exists as a base
to perform a merge off of.

## How to merge changes from the compiler's parser
*WARNING*: Currently, the version of the parser in `parser-jane/` is ahead of
the compiler's parser. Be careful about "downgrading" it, as it might break a
lot of the logic in `Normalize_std_ast.ml`.

### "Manually"
First, in the `vendor/` directory, generate a patchfile
```
diff -ruN parser-jane/ parser-standard/ > changes.patch
```
Then, update the files in `parser-jane/` by running the update script
```
./parser-jane/update.sh {path-to-flambda-backend}
```
Finally, create the new `parser-standard/` by copying `parser-jane/` and applying the patchfile
```
rm -rf parser-standard/
cp -r parser-jane/ parser-standard/
patch -p1 -d parser-standard/ < changes.patch
rm changes.patch
```

### With [repatch.sh]
You can also just run the repatch script to do all the above steps automatically.
```
./parser-jane/repatch.sh {path-to-flambda-backend}
```
