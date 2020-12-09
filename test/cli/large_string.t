  $ echo "let _ = \"$(printf '%*s' 300000 | sed 's/ /_ _/g')\"" > a.ml

  $ ocamlformat --impl a.ml -o /dev/null
