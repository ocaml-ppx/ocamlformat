  $ echo profile=default > .ocamlformat

  $ cat > a.ml << EOF
  > (* Intentionally not formatted *)
  > let () =
  >   print_endline
  >     A.x
  > EOF

  $ (ocamlformat --debug a.ml) 2>&1 | sed -e 's/\([^ ]*\.html$\)/<file>.html/g' -e 's/\([^ ]*\.ast$\)/<file>.ast/g'
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,34+0]..[4,59+7])
      comments
        before: (* Intentionally not formatted *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,34+0]..[4,59+7])
          comments
            before: (* Intentionally not formatted *) is_pun=false
                None
            pattern (a.ml[2,34+4]..[2,34+6])
              Ppat_construct "()" (a.ml[2,34+4]..[2,34+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[3,43+2]..[4,59+7])
                Pexp_apply
                expression (a.ml[3,43+2]..[3,43+15])
                  Pexp_ident "print_endline" (a.ml[3,43+2]..[3,43+15])
                [
                  <arg>
                  Nolabel
                    expression (a.ml[4,59+4]..[4,59+7])
                      Pexp_ident "A.x" (a.ml[4,59+4]..[4,59+7])
                ]
        ]
    ]
  
  [DEBUG] Box structure: <file>.html
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,34+0]..[4,59+7])
      comments
        before: (* Intentionally not formatted *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,34+0]..[4,59+7])
          comments
            before: (* Intentionally not formatted *) is_pun=false
                None
            pattern (a.ml[2,34+4]..[2,34+6])
              Ppat_construct "()" (a.ml[2,34+4]..[2,34+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[3,43+2]..[4,59+7])
                Pexp_apply
                expression (a.ml[3,43+2]..[3,43+15])
                  Pexp_ident "print_endline" (a.ml[3,43+2]..[3,43+15])
                [
                  <arg>
                  Nolabel
                    expression (a.ml[4,59+4]..[4,59+7])
                      Pexp_ident "A.x" (a.ml[4,59+4]..[4,59+7])
                ]
        ]
    ]
  
  [DEBUG] AST structure: <file>.ast
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,34+0]..[2,34+26])
      comments
        before: (* Intentionally not formatted *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,34+0]..[2,34+26])
          comments
            before: (* Intentionally not formatted *) is_pun=false
                None
            pattern (a.ml[2,34+4]..[2,34+6])
              Ppat_construct "()" (a.ml[2,34+4]..[2,34+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[2,34+9]..[2,34+26])
                Pexp_apply
                expression (a.ml[2,34+9]..[2,34+22])
                  Pexp_ident "print_endline" (a.ml[2,34+9]..[2,34+22])
                [
                  <arg>
                  Nolabel
                    expression (a.ml[2,34+23]..[2,34+26])
                      Pexp_ident "A.x" (a.ml[2,34+23]..[2,34+26])
                ]
        ]
    ]
  
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,34+0]..[2,34+26])
      comments
        before: (* Intentionally not formatted *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,34+0]..[2,34+26])
          comments
            before: (* Intentionally not formatted *) is_pun=false
                None
            pattern (a.ml[2,34+4]..[2,34+6])
              Ppat_construct "()" (a.ml[2,34+4]..[2,34+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[2,34+9]..[2,34+26])
                Pexp_apply
                expression (a.ml[2,34+9]..[2,34+22])
                  Pexp_ident "print_endline" (a.ml[2,34+9]..[2,34+22])
                [
                  <arg>
                  Nolabel
                    expression (a.ml[2,34+23]..[2,34+26])
                      Pexp_ident "A.x" (a.ml[2,34+23]..[2,34+26])
                ]
        ]
    ]
  
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

  $ (ocamlformat --debug a.ml) 2>&1 | sed -e 's/\([^ ]*\.html$\)/<file>.html/g' -e 's/\([^ ]*\.ast$\)/<file>.ast/g'
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,25+0]..[6,233+3])
      comments
        before: (* before let-binding *)
         after: (* after let-binding *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,25+0]..[6,233+3])
          comments
            before: (* before let-binding *)
             after: (* after let-binding *) is_pun=false
                None
            pattern (a.ml[2,25+4]..[2,25+6])
              Ppat_construct "()" (a.ml[2,25+4]..[2,25+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[5,90+2]..[6,233+3])
              comments
                before: (* before x binding #1 *)
                before: (* before x binding #2 *)
                Pexp_let Nonrec
                  loc_in: (a.ml[5,90+140]..[5,90+142])
                  comments
                    before: (* after unit *)
                [
                  <def> (a.ml[5,90+2]..[5,90+122]) is_pun=false
                        None
                    pattern (a.ml[5,90+21]..[5,90+22])
                    comments
                      before: (* before x *)
                       after: (* after x #1 *)
                       after: (* after x #2 *)
                      Ppat_var "x" (a.ml[5,90+21]..[5,90+22])
                      comments
                        before: (* before x *)
                         after: (* after x #1 *)
                         after: (* after x #2 *)
                    args
                    []
                    Pfunction_body
                      expression (a.ml[5,90+77]..[5,90+122])
                      comments
                        before: (* before unit *)
                        within: (* within unit #1 *)
                        within: (* within unit #2 *)
                        Pexp_construct "()" (a.ml[5,90+77]..[5,90+122])
                        comments
                          before: (* before unit *)
                          within: (* within unit #1 *)
                          within: (* within unit #2 *)
                        None
                ]
                expression (a.ml[6,233+2]..[6,233+3])
                  Pexp_ident "x" (a.ml[6,233+2]..[6,233+3])
        ]
    ]
  
  [DEBUG] Box structure: <file>.html
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,25+0]..[6,233+3])
      comments
        before: (* before let-binding *)
         after: (* after let-binding *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,25+0]..[6,233+3])
          comments
            before: (* before let-binding *)
             after: (* after let-binding *) is_pun=false
                None
            pattern (a.ml[2,25+4]..[2,25+6])
              Ppat_construct "()" (a.ml[2,25+4]..[2,25+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[5,90+2]..[6,233+3])
              comments
                before: (* before x binding #1 *)
                before: (* before x binding #2 *)
                Pexp_let Nonrec
                  loc_in: (a.ml[5,90+140]..[5,90+142])
                  comments
                    before: (* after unit *)
                [
                  <def> (a.ml[5,90+2]..[5,90+122]) is_pun=false
                        None
                    pattern (a.ml[5,90+21]..[5,90+22])
                    comments
                      before: (* before x *)
                       after: (* after x #1 *)
                       after: (* after x #2 *)
                      Ppat_var "x" (a.ml[5,90+21]..[5,90+22])
                      comments
                        before: (* before x *)
                         after: (* after x #1 *)
                         after: (* after x #2 *)
                    args
                    []
                    Pfunction_body
                      expression (a.ml[5,90+77]..[5,90+122])
                      comments
                        before: (* before unit *)
                        within: (* within unit #1 *)
                        within: (* within unit #2 *)
                        Pexp_construct "()" (a.ml[5,90+77]..[5,90+122])
                        comments
                          before: (* before unit *)
                          within: (* within unit #1 *)
                          within: (* within unit #2 *)
                        None
                ]
                expression (a.ml[6,233+2]..[6,233+3])
                  Pexp_ident "x" (a.ml[6,233+2]..[6,233+3])
        ]
    ]
  
  [DEBUG] AST structure: <file>.ast
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,25+0]..[13,265+3])
      comments
        before: (* before let-binding *)
         after: (* after let-binding *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,25+0]..[13,265+3])
          comments
            before: (* before let-binding *)
             after: (* after let-binding *) is_pun=false
                None
            pattern (a.ml[2,25+4]..[2,25+6])
              Ppat_construct "()" (a.ml[2,25+4]..[2,25+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[5,90+2]..[13,265+3])
              comments
                before: (* before x binding #1 *)
                before: (* before x binding #2 *)
                Pexp_let Nonrec
                  loc_in: (a.ml[12,260+2]..[12,260+4])
                [
                  <def> (a.ml[5,90+2]..[10,210+28])
                  comments
                     after: (* after unit *) is_pun=false
                        None
                    pattern (a.ml[5,90+21]..[5,90+22])
                    comments
                      before: (* before x *)
                       after: (* after x #1 *)
                       after: (* after x #2 *)
                      Ppat_var "x" (a.ml[5,90+21]..[5,90+22])
                      comments
                        before: (* before x *)
                         after: (* after x #1 *)
                         after: (* after x #2 *)
                    args
                    []
                    Pfunction_body
                      expression (a.ml[9,183+4]..[10,210+28])
                      comments
                        before: (* before unit *)
                        within: (* within unit #1 *)
                        within: (* within unit #2 *)
                        Pexp_construct "()" (a.ml[9,183+4]..[10,210+28])
                        comments
                          before: (* before unit *)
                          within: (* within unit #1 *)
                          within: (* within unit #2 *)
                        None
                ]
                expression (a.ml[13,265+2]..[13,265+3])
                  Pexp_ident "x" (a.ml[13,265+2]..[13,265+3])
        ]
    ]
  
  AST:
  Ptop_def
    [
      structure_item (a.ml[2,25+0]..[13,265+3])
      comments
        before: (* before let-binding *)
         after: (* after let-binding *)
        Pstr_value Nonrec
        [
          <def> (a.ml[2,25+0]..[13,265+3])
          comments
            before: (* before let-binding *)
             after: (* after let-binding *) is_pun=false
                None
            pattern (a.ml[2,25+4]..[2,25+6])
              Ppat_construct "()" (a.ml[2,25+4]..[2,25+6])
              None
            args
            []
            Pfunction_body
              expression (a.ml[5,90+2]..[13,265+3])
              comments
                before: (* before x binding #1 *)
                before: (* before x binding #2 *)
                Pexp_let Nonrec
                  loc_in: (a.ml[12,260+2]..[12,260+4])
                [
                  <def> (a.ml[5,90+2]..[10,210+28])
                  comments
                     after: (* after unit *) is_pun=false
                        None
                    pattern (a.ml[5,90+21]..[5,90+22])
                    comments
                      before: (* before x *)
                       after: (* after x #1 *)
                       after: (* after x #2 *)
                      Ppat_var "x" (a.ml[5,90+21]..[5,90+22])
                      comments
                        before: (* before x *)
                         after: (* after x #1 *)
                         after: (* after x #2 *)
                    args
                    []
                    Pfunction_body
                      expression (a.ml[9,183+4]..[10,210+28])
                      comments
                        before: (* before unit *)
                        within: (* within unit #1 *)
                        within: (* within unit #2 *)
                        Pexp_construct "()" (a.ml[9,183+4]..[10,210+28])
                        comments
                          before: (* before unit *)
                          within: (* within unit #1 *)
                          within: (* within unit #2 *)
                        None
                ]
                expression (a.ml[13,265+2]..[13,265+3])
                  Pexp_ident "x" (a.ml[13,265+2]..[13,265+3])
        ]
    ]
  
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
