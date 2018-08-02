
FAIL
Str:
let tail2 = 0 :: (([1; 2])[@hello ])
[
  structure_item (failing/attribute_and_expression.ml[4,54+0]..[4,54+33])
    Pstr_value Nonrec
    [
      <def>
        pattern (failing/attribute_and_expression.ml[4,54+4]..[4,54+9])
          Ppat_var "tail2" (failing/attribute_and_expression.ml[4,54+4]..[4,54+9])
        expression (failing/attribute_and_expression.ml[4,54+12]..[4,54+33])
          Pexp_construct "::" (failing/attribute_and_expression.ml[4,54+13]..[4,54+15])
          Some
            expression (failing/attribute_and_expression.ml[4,54+12]..[4,54+33]) ghost
              Pexp_tuple
              [
                expression (failing/attribute_and_expression.ml[4,54+12]..[4,54+13])
                  Pexp_constant PConst_int (0,None)
                expression (failing/attribute_and_expression.ml[4,54+15]..[4,54+33])
                  attribute "hello"
                    []
                  Pexp_construct "::" (failing/attribute_and_expression.ml[4,54+18]..[4,54+23]) ghost
                  Some
                    expression (failing/attribute_and_expression.ml[4,54+18]..[4,54+23]) ghost
                      Pexp_tuple
                      [
                        expression (failing/attribute_and_expression.ml[4,54+18]..[4,54+19])
                          Pexp_constant PConst_int (1,None)
                        expression (failing/attribute_and_expression.ml[4,54+21]..[4,54+23]) ghost
                          Pexp_construct "::" (failing/attribute_and_expression.ml[4,54+21]..[4,54+23]) ghost
                          Some
                            expression (failing/attribute_and_expression.ml[4,54+21]..[4,54+23]) ghost
                              Pexp_tuple
                              [
                                expression (failing/attribute_and_expression.ml[4,54+21]..[4,54+22])
                                  Pexp_constant PConst_int (2,None)
                                expression (failing/attribute_and_expression.ml[4,54+22]..[4,54+23]) ghost
                                  Pexp_construct "[]" (failing/attribute_and_expression.ml[4,54+22]..[4,54+23]) ghost
                                  None
                              ]
                      ]
              ]
    ]
]

Raised at file "src/Fmt_ast.ml", line 216, characters 2-62
Called from file "src/Fmt_ast.ml", line 1455, characters 10-30
Called from file "src/Fmt_ast.ml", line 1005, characters 29-75
Called from file "src/Fmt_ast.ml", line 3467, characters 8-24
Called from file "src/Fmt_ast.ml", line 3356, characters 13-261
Called from file "src/Fmt.ml", line 123, characters 2-6
Called from file "src/Fmt.ml", line 123, characters 2-6
Called from file "src/Fmt_ast.ml", line 29, characters 8-13

ocamlformat.exe: Cannot process "failing/attribute_and_expression.ml".
  Please report this bug at https://github.com/ocaml-ppx/ocamlformat/issues.
  BUG: unhandled exception.
