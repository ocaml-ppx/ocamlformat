#!/bin/bash
set -euo pipefail

if [[ "$#" == "1" ]] ; then
    flambda_backend_dir="$1"
else
    echo "Wrong number of arguments"
    exit 1
fi

parsing_dir="${flambda_backend_dir}/parsing"
utils_dir="${flambda_backend_dir}/utils"
lex_dir="${flambda_backend_dir}/lex"

cd $(dirname $0)
# parser-standard
cp "$parsing_dir"/asttypes.mli for-parser-standard/
cp "$parsing_dir"/ast_helper.ml for-parser-standard/
cp "$parsing_dir"/ast_mapper.ml for-parser-standard/
cp "$parsing_dir"/docstrings.ml for-parser-standard/
cp "$parsing_dir"/jane_syntax.ml for-parser-standard/
cp "$parsing_dir"/jane_syntax.mli for-parser-standard/
cp "$parsing_dir"/jane_syntax_parsing.ml for-parser-standard/
cp "$parsing_dir"/jane_syntax_parsing.mli for-parser-standard/
cp "$utils_dir"/language_extension.ml for-parser-standard/
cp "$utils_dir"/language_extension.mli for-parser-standard/
cp "$utils_dir"/language_extension_kernel.ml for-parser-standard/
cp "$utils_dir"/language_extension_kernel.mli for-parser-standard/
cp "$parsing_dir"/lexer.mll for-parser-standard/
cp "$parsing_dir"/parse.ml for-parser-standard/
cp "$parsing_dir"/parser.mly for-parser-standard/
cp "$parsing_dir"/parsetree.mli for-parser-standard/
cp "$parsing_dir"/printast.ml for-parser-standard/
cp "$parsing_dir"/parser_types.ml for-parser-standard/
cp "$parsing_dir"/parser_types.mli for-parser-standard/

# ocaml-common
cp "$parsing_dir"/location.ml for-ocaml-common/
cp "$parsing_dir"/location.mli for-ocaml-common/
cp "$parsing_dir"/longident.ml for-ocaml-common/
cp "$parsing_dir"/longident.mli for-ocaml-common/
cp "$parsing_dir"/syntaxerr.ml for-ocaml-common/
cp "$parsing_dir"/syntaxerr.mli for-ocaml-common/
cp "$utils_dir"/warnings.ml for-ocaml-common/
cp "$utils_dir"/warnings.mli for-ocaml-common/

# save git commit
git -C "$flambda_backend_dir" rev-parse HEAD > imported_commit.txt
