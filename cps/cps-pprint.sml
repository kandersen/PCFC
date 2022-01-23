structure PPrintCPS =
struct
  open ILCPS
  open PPrintUtils

  structure StringSet = RedBlackSetFn(
      struct
        type ord_key = string
        val compare = String.compare
      end
  )

  infix <>
  infix <+>
  infix </>
  infix <+/>

  fun ppkont scope k =
      text (
          if StringSet.member(scope, KVariable.pprint k)
          then KVariable.disamb k
          else KVariable.disamb k
      )

  fun ppvar scope v =
      text (
          if StringSet.member(scope, Variable.pprint v)
          then Variable.disamb v
          else Variable.disamb v
      )

  fun parens x =
      text "("
      <> x
      <> text ")"

  fun pptype ty =
      case ty of
          TNat =>
          text "nat"
        | TArr(t1, t2) =>
          (case t1 of
               TArr _ =>
               parens (pptype t1)
             | _ =>
               pptype t1)
              <+> text "->"
              <+> pptype t2

  fun ppktype ty =
      case ty of
          TNat =>
          text "¬nat"
        | TArr _ =>
          text "¬"
          <> parens (pptype ty)


  fun ppkbinding scope k t =
      parens (ppkont scope k
                     <+> text ":"
                     <+> ppktype t)

  fun ppbinding scope x t =
      parens (ppvar scope x
                    <+> text ":"
                    <+> pptype t)

  fun ppexp scope e =
      case e of
          ELetVal(t, x, v, e) =>
          text "letval" <+>
               nest 2 (
              ppbinding scope x t
                        <+> text "="
                        <+> ppval scope v
                        <+> text "in"
                        <+> ppexp scope e)
        | EIfz(x,k1,k2) =>
          text "ifz"
          <> parens (ppvar scope x
                     <> text ","
                     <> ppkont scope k1
                     <> text ","
                     <> ppkont scope k1)
        | ELetFun(f,tk,k,tx,x,e1,e2) =>
          text "letfun" <+>
               nest 2(
               ppvar scope f
               <+> ppkbinding scope k tk
               <+> ppbinding scope x tx
               <+> text "="
               <+> ppexp scope e1
               <+> text "in"
               <+> ppexp scope e2)
        | EAppFun(f, k, x) =>
          ppvar scope f
                <+> ppkont scope k
                <+> ppvar scope x
        | ELetCont(k,tx,x,e1,e2) =>
          text "letcont"
               <+> ppkont scope k
               <+> ppbinding scope x tx
               <+> text "="
               <+> ppexp scope e1
               <+> text "in"
               <+> ppexp (StringSet.add(scope,KVariable.pprint k)) e2
        | EAppCont(k, x) =>
          ppkont scope k
                 <+> ppvar scope x

  and ppval scope v =
      case v of
          VZero =>
          text "0"
        | VSucc x =>
          text "S("
          <> ppvar scope x
          <> text ")"
        | VLam(tk, k, tx, x, e) =>
          text "\\"
               <> ppkbinding scope k tx
               <+> ppbinding scope x tx
          <> text "."
          <> ppexp scope e

  fun prettyprint (e : exp) = pretty 80 (ppexp StringSet.empty e)

end
