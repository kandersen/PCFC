structure CPSSExprs =
struct
  open ILCPS

  infix <>
  fun s1 <> s2 = s1 ^ s2

  infix <+>
  fun s1 <+> s2 = s1 <> " " <> s2

  infix </>
  fun s1 </> s2 = s1 <> "\n" <> s2

  fun parens s = "(" <> s <> ")"

  fun indent 0 = " "
    | indent n = " " <> indent (n - 1)

  fun ppkont k = KVariable.disamb k

  fun ppvar v = Variable.disamb v

  fun pptype ty =
      let
          fun ppposty TNat = "nat"
            | ppposty (TArr(t1, t2)) = ppnegty t1 <+> "->" <+> ppposty t2
          and ppnegty TNat = "nat"
            | ppnegty t = parens (ppposty t)
      in
          ppposty ty
      end

  fun ppktype TNat = "¬nat"
    | ppktype t = "¬" <> parens (pptype t)


  fun format_binding n t = parens (n <+> ":" <+> t)

  fun ppexp i e =
      indent i <> parens (case e of
                              ELetVal(t, x, v, e) =>
                              "letval" <+> format_binding (ppvar x) (pptype t)
                                       </> ppval (i + 4) v
                                       </> ppexp i e
                            | EIfz(x, k1, k2) =>
                              "ifz" <+> ppvar x <+> ppkont k1 <+> ppkont k2
                            | ELetFun(f, tk, k, tx, x, e1, e2) =>
                              "letfun" <+> ppvar f
                                       <+> format_binding (ppkont k) (ppktype tk)
                                       <+> format_binding (ppvar x) (pptype tx)
                                       </> ppexp (i + 4) e1
                                       </> ppexp i e2
                            | EAppFun(f, k, x) =>
                              "appfun" <+> ppvar f <+> ppkont k <+> ppvar x
                            | ELetCont(k, tx, x, e1, e2) =>
                              "letcont" <+> ppkont k <+> format_binding (ppvar x) (pptype tx)
                                        </> ppexp (i + 4) e1
                                        </> ppexp i e2
                            | EAppCont(k, x) =>
                              "appcont" <+> ppkont k <+> ppvar x
                         )

  and ppval i v =
      indent i <>
      (case v of
           VZero =>
           "0"
         | VSucc x =>
           parens ("+1" <+> ppvar x)
         | VLam(tk, k, tx, x, e) =>
           parens ("lambda" <+> format_binding (ppkont k) (ppktype tk)
                            <+> format_binding (ppvar x) (pptype tx)
                            </> ppexp (i + 4) e)
      )

  fun prettyprint (e : exp) =
      ppexp 0 e

end
