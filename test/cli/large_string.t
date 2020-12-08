  $ echo "let _ = \"$(printf '%*s' 200000 | sed 's/ /_ _/g')\"" > a.ml

  $ ocamlformat --impl a.ml -o /dev/null
