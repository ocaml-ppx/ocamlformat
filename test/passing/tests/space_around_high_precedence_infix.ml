let formula_base x =
  let open Formula.Infix in (Expr.typeof x) #== (Lit (Type IntType))
    #&& (x #<= (Expr.int 4))
        #&& ((Expr.int 0) #< x)