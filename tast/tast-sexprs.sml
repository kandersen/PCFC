structure TASTSExprs =
struct
  open ILTAST

  infix <>
  fun s1 <> s2 = s1 ^ s2

  infix <+>
  fun s1 <+> s2 = s1 <> " " <> s2

  infix </>
  fun s1 </> s2 = s1 <> "\n" <> s2

  fun parens s = "(" <> s <> ")"

  fun indent 0 = " "
    | indent n = " " <> indent (n - 1)


  fun ppvar v = Variable.disamb v

  fun pptype ty =
      let
          fun ppposty ILAST.TNat = "nat"
            | ppposty (ILAST.TArr(t1, t2)) = ppnegty t1 <+> "->" <+> ppposty t2
          and ppnegty ILAST.TNat = "nat"
            | ppnegty t = parens (ppposty t)
      in
          ppposty ty
      end

  fun format_binding n t = parens (n <+> ":" <+> t)

  fun ppexp i e =
      let val t = pptype (typeof e) in
          indent i <> (case unexp e of
                           EVar x =>
                           format_binding (ppvar x) t
                         | EZero =>
                           "0" <+> t
                         | ESucc e' =>
                           parens ("S" <+> t
                                       </> ppexp (i + 3) e')
                         | EApp(e1, e2) =>
                           parens ("app" <+> t
                                         </> ppexp (i + 5) e1
                                         </> ppexp (i + 5) e2)
                         | ELam(x, e') =>
                           let val (ILAST.TArr(t1, t2)) = typeof e in
                               parens ("lambda" <+> format_binding (ppvar x) (pptype t1) <+> ":" <+> pptype t2
                                                </> ppexp (i + 4) e')
                           end
                         | EIfz(e1, e2, n', e3) =>
                           parens ("ifz" <+> t
                                         </> ppexp (i + 5) e1
                                         </> ppexp (i + 5) e2
                                         </> indent (i + 5) <> format_binding (ppvar n') (pptype ILAST.TNat)
                                         </> ppexp (i + 5) e3)
                         | EFix(x, e) =>
                           parens ("fix" <+> t
                                         </> format_binding (ppvar x) t
                                         </> ppexp (i + 5) e)
                      )
      end

  fun prettyprint (e : exp) =
      ppexp 0 e

end
