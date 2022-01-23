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
    | ELam of ty * Variable.t * exp
    | EFix of ty * Variable.t * exp
    | ESucc of exp
    | EZero

  fun var v t = Exp { exp = EVar v, ty = t }
  fun app e1 e2 t = Exp { exp = EApp(e1, e2), ty = t }
  fun ifz e1 e2 x e3 t = Exp { exp = EIfz(e1, e2, x, e3), ty = t }
  fun lam t1 x e t2 = Exp { exp = ELam(t1, x, e), ty = t2 }
  fun fix t x e = Exp { exp = EFix(t, x, e), ty = t }
  fun succ e = Exp { exp = ESucc e, ty = TNat }
  val zero = Exp { exp = EZero, ty = TNat }

  fun typeof (Exp { ty, ... }) = ty
  fun unexp (Exp { exp, ... }) = exp

end
