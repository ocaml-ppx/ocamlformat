  $ cat > a.ml << EOF
  > (* Intentionally not formatted *)
  > let () =
  >   print_endline
  >     A.x
  > EOF

  $ ocamlformat --debug a.ml
  
  Loc_tree:
  "([2,34+0]..[4,59+7])" before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([3,43+2]..[4,59+7])"
     {"([3,43+2]..[3,43+15])"
      "([4,59+4]..[4,59+7])" } }
  
  
  Loc_tree:
  "([2,34+0]..[4,59+7])" before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([3,43+2]..[4,59+7])"
     {"([3,43+2]..[3,43+15])"
      "([4,59+4]..[4,59+7])" } }
  
  
  Loc_tree:
  "([2,34+0]..[2,34+26])" before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([2,34+9]..[2,34+26])"
     {"([2,34+9]..[2,34+22])"
      "([2,34+23]..[2,34+26])" } }
  
  
  Loc_tree:
  "([2,34+0]..[2,34+26])" before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([2,34+9]..[2,34+26])"
     {"([2,34+9]..[2,34+22])"
      "([2,34+23]..[2,34+26])" } }
  
  (* Intentionally not formatted *)
  let () = print_endline A.x
