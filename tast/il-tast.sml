structure ILTAST =
struct

  structure Variable = ILAST.Variable
  structure VarMap = ILAST.VarMap

  type ty = ILAST.ty

  datatype exp = Exp of { exp : expF, ty : ty }

  and expF
    = EVar of Variable.t
    | EApp of exp * exp
    | EIfz of exp * exp * Variable.t * exp
    | ELam of Variable.t * exp
    | EFix of Variable.t * exp
    | ESucc of exp
    | EZero

  fun var t v =
      Exp { exp = EVar v, ty = t }
  fun app t e1 e2 =
      Exp { exp = EApp(e1, e2), ty = t }
  fun ifz t e1 e2 x e3 =
      Exp { exp = EIfz(e1, e2, x, e3), ty = t }
  fun lam t1 t2 x e =
      Exp { exp = ELam(x, e), ty = ILAST.TArr(t1, t2) }
  fun fix t x e =
      Exp { exp = EFix(x, e), ty = t }
  fun succ e =
      Exp { exp = ESucc e, ty = ILAST.TNat }
  val zero =
      Exp { exp = EZero, ty = ILAST.TNat }

  fun typeof (Exp { ty, ... }) = ty
  fun unexp (Exp { exp, ... }) = exp

end
