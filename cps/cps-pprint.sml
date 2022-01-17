structure PPrintCPS =
struct
  open ILCPS
  open PPrintUtils

  type varset = unit VarMap.map
  type kontset = unit KVarMap.map


  infix <>
  infix <+>
  infix </>
  infix <+/>

  fun ppkont kvars k =
      text (
          if KVarMap.inDomain(kvars, k)
          then KVariable.disamb k
          else KVariable.pprint k
      )

  fun ppvar vars v =
      text (
          if VarMap.inDomain(vars, v)
          then Variable.disamb v
          else Variable.pprint v
      )

  fun parens x = text "(" <> x <> text ")"

  fun pptype ty =
      case ty of
          TNat =>
          text "nat"
        | TArr(t1, t2) =>
          (case t1 of
               TArr _ =>
               parens (pptype t1)
             | _ =>
               pptype t1
          ) <+> text "->" <+> pptype t2

  fun ppktype ty =
      case ty of
          TNat =>
          text "¬nat"
        | TArr _ =>
          text "¬" <> parens (pptype ty)


  fun ppkbinding kvars k t = parens (ppkont kvars k <+> text ":" <+> ppktype t)

  fun ppbinding vars x t = parens (ppvar vars x <+> text ":" <+> pptype t)

  fun ppexp vars kvars e =
      case e of
          ELetVal(t, x, v, e) =>
          text "letval" <+> ppbinding vars x t <+> text "=" <+> ppval vars kvars v </> text "in" </> ppexp vars kvars e
        | EIfz(x,k1,k2) => text "ifz" <> parens (ppvar vars x <> text "," <> ppkont kvars k1 <> text "," <> ppkont kvars k1)
        | ELetFun(f,tk,k,tx,x,e1,e2) =>
          text "letfun" <+> ppvar vars f <+> ppkbinding kvars k tk <+> ppbinding vars x tx <+> text "=" <+> ppexp vars kvars e1 </> text "in" </> (ppexp vars kvars e2)
        | EAppFun(f, k, x) =>
          ppvar vars f <+> ppkont kvars k <+> ppvar vars x
        | ELetCont(k,tx,x,e1,e2) =>
          text "letcont" <+> ppkont kvars k <+> ppbinding vars x tx <+> text "=" <+> ppexp vars kvars e1 </> text "in" </> ppexp vars kvars e2
        | EAppCont(k, x) =>
          ppkont kvars k <+> ppvar vars x

  and ppval vars kvars v =
      case v of
          VZero => text "0"
        | VSucc x => text "S(" <> ppvar vars x <> text ")"
        | VLam(tk, k, tx, x, e) =>
          text ""

  fun prettyprint (e : exp) = pretty 80 (ppexp VarMap.empty KVarMap.empty e)

end
