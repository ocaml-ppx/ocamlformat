  $ cat > a.ml << EOF
  > (* Intentionally not formatted *)
  > let () =
  >   print_endline
  >     A.x
  > EOF

  $ ocamlformat --debug a.ml
  
  Loc_tree:
  "([2,34+0]..[4,59+7])"
          before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([3,43+2]..[4,59+7])"
     {"([3,43+2]..[3,43+15])"
      "([4,59+4]..[4,59+7])" } }
  
  
  Loc_tree:
  "([2,34+0]..[4,59+7])"
          before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([3,43+2]..[4,59+7])"
     {"([3,43+2]..[3,43+15])"
      "([4,59+4]..[4,59+7])" } }
  
  
  Loc_tree:
  "([2,34+0]..[2,34+26])"
          before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([2,34+9]..[2,34+26])"
     {"([2,34+9]..[2,34+22])"
      "([2,34+23]..[2,34+26])" } }
  
  
  Loc_tree:
  "([2,34+0]..[2,34+26])"
          before: (* Intentionally not formatted *)
   {"([2,34+4]..[2,34+6])"
    "([2,34+9]..[2,34+26])"
     {"([2,34+9]..[2,34+22])"
      "([2,34+23]..[2,34+26])" } }
  
  (* Intentionally not formatted *)
  let () = print_endline A.x

  $ cat > a.ml << EOF
  > (* before let-binding *)
  > let () =
  >   (* before x binding #1 *)
  >   (* before x binding #2 *)
  >   let (* before x *) x (* after x #1 *) (* after x #2 *) = (* before unit *) ( (* within unit #1 *) (* within unit #2 *) ) (* after unit *) in
  >   x
  > (* after let-binding *)
  > EOF

  $ ocamlformat --debug a.ml
  
  Loc_tree:
  "([2,25+0]..[6,233+3])"
          before: (* before let-binding *)
          after: (* after let-binding *)
   {"([2,25+4]..[2,25+6])"
    "([5,90+2]..[6,233+3])"
            before: (* before x binding #1 *)
            before: (* before x binding #2 *)
     {"([5,90+2]..[5,90+122])"
              after: (* after unit *)
       {"([5,90+21]..[5,90+22])"
                before: (* before x *)
                after: (* after x #1 *)
                after: (* after x #2 *)
        "([5,90+77]..[5,90+122])"
                before: (* before unit *)
                within: (* within unit #1 *)
                within: (* within unit #2 *) }
      "([6,233+2]..[6,233+3])" } }
  
  
  Loc_tree:
  "([2,25+0]..[6,233+3])"
          before: (* before let-binding *)
          after: (* after let-binding *)
   {"([2,25+4]..[2,25+6])"
    "([5,90+2]..[6,233+3])"
            before: (* before x binding #1 *)
            before: (* before x binding #2 *)
     {"([5,90+2]..[5,90+122])"
              after: (* after unit *)
       {"([5,90+21]..[5,90+22])"
                before: (* before x *)
                after: (* after x #1 *)
                after: (* after x #2 *)
        "([5,90+77]..[5,90+122])"
                before: (* before unit *)
                within: (* within unit #1 *)
                within: (* within unit #2 *) }
      "([6,233+2]..[6,233+3])" } }
  
  
  Loc_tree:
  "([2,25+0]..[13,265+3])"
          before: (* before let-binding *)
          after: (* after let-binding *)
   {"([2,25+4]..[2,25+6])"
    "([5,90+2]..[13,265+3])"
            before: (* before x binding #1 *)
            before: (* before x binding #2 *)
     {"([5,90+2]..[10,210+28])"
              after: (* after unit *)
       {"([5,90+21]..[5,90+22])"
                before: (* before x *)
                after: (* after x #1 *)
                after: (* after x #2 *)
        "([9,183+4]..[10,210+28])"
                before: (* before unit *)
                within: (* within unit #1 *)
                within: (* within unit #2 *) }
      "([13,265+2]..[13,265+3])" } }
  
  
  Loc_tree:
  "([2,25+0]..[13,265+3])"
          before: (* before let-binding *)
          after: (* after let-binding *)
   {"([2,25+4]..[2,25+6])"
    "([5,90+2]..[13,265+3])"
            before: (* before x binding #1 *)
            before: (* before x binding #2 *)
     {"([5,90+2]..[10,210+28])"
              after: (* after unit *)
       {"([5,90+21]..[5,90+22])"
                before: (* before x *)
                after: (* after x #1 *)
                after: (* after x #2 *)
        "([9,183+4]..[10,210+28])"
                before: (* before unit *)
                within: (* within unit #1 *)
                within: (* within unit #2 *) }
      "([13,265+2]..[13,265+3])" } }
  
  (* before let-binding *)
  let () =
    (* before x binding #1 *)
    (* before x binding #2 *)
    let (* before x *) x
        (* after x #1 *)
        (* after x #2 *) =
      (* before unit *)
      ( (* within unit #1 *)
        (* within unit #2 *) )
      (* after unit *)
    in
    x
  (* after let-binding *)
