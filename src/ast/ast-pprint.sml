structure ASTPPrint =
struct
  open PrettyPrint
  infix <>
  infix <+>
  infix </>

  open ILAST

  fun format_variable x =
      text (Variable.pprint x)

  fun format_type (t : ty) : PrettyPrint.t =
      let
          fun positive (t : ty) =
              case t of
                  TNat =>
                  text "nat"
                | TArr(t1, t2) =>
                  negative t1 <+> text "->" <+> positive t2

          and negative (t : ty) =
              case t of
                  TNat =>
                  text "nat"
                | TArr(t1, t2) =>
                  parens (negative t1 <+> text "->" <+> positive t2)
      in
          positive t
      end

  fun format_succ s e =
      case e of
          EZero =>
          text (Int.toString s)
        | ESucc e' =>
          format_succ (s + 1) e'
        | _ => format_exp e <+> text "+" <+> text (Int.toString s)

  and format_exp (e : exp) : PrettyPrint.t =
      case e of
          EZero =>
          text "0"
        | ESucc e =>
          format_succ 1 e
        | ELam(t,(x,e)) =>
          text "\\" <> (format_variable x) <+> text ":" <+> (format_type t) <> text "."
                                      </> format_exp e
        | EApp(e1, e2) =>
          parens (format_exp e1 </> format_exp e2)
        | EFix(t,(x,e)) =>
          text "<efix>"
        | EIfz(n, z, (n',s)) =>
          text "case" <+> format_exp n <+> text "of" </>
          indent 2 (stack [
              text "zero",
              text "succ"
          ])

        | EVar x =>
          text (Variable.pprint x)

  fun print (e : exp) : string =
      run (format_exp e)

end
