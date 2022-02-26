structure CPSTransform : CPS_TRANSFORM =
struct

  structure Var = ILCPS.Variable
  structure KVar = ILCPS.KVariable

  fun transform_type (t : ILTAST.ty) : ILCPS.ty = t

  fun transform_exp (ILTAST.Exp { exp, ty } : ILTAST.exp) (k : Var.t -> ILCPS.exp) : ILCPS.exp =
      case exp of
          ILTAST.EVar x =>
          k x
        | ILTAST.EApp(e1, e2) =>
          transform_exp
              e1
              (fn x1 =>
                  transform_exp
                      e2
                      (fn x2 =>
                          let
                              val k' = KVar.fresh("k")
                              val x' = Var.fresh("x")
                          in
                              ILCPS.ELetCont(k', ILAST.TNat, x', k x', ILCPS.EAppFun(x1, k', x2))
                          end))
        | ILTAST.EIfz(i, z, x, s) =>
          transform_exp
              i
              (fn n =>
                  let
                      val j = KVar.fresh("j")
                      val jx = Var.fresh("x")
                      val kz = KVar.fresh("k")
                      val unused = Var.fresh("_")
                      val ks = KVar.fresh("k")
                  in
                      ILCPS.ELetCont(j, ILAST.TNat, jx, k jx,
                                     ILCPS.ELetCont(kz, ILAST.TNat, unused, transform_val z j,
                                                    ILCPS.ELetCont(ks, ILAST.TNat, x, transform_val s j,
                                                                   ILCPS.EIfz(n, kz, ks))))
                  end
              )
        | ILTAST.ELam(x, e) =>
          let
              val f = Var.fresh("f")
              val k' = KVar.fresh("k")
          in
              ILCPS.ELetFun(f, transform_type ty, k', ILAST.TNat, x, transform_val e k', k f)
          end
        | ILTAST.ESucc e1 =>
          let
              val x = Var.fresh("x")
          in
              transform_exp
                  e1
                  (fn x1 =>
                      ILCPS.ELetVal(ILAST.TNat, x, ILCPS.VSucc x1, k x))
          end
        | ILTAST.EZero =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILAST.TNat, x, ILCPS.VZero, k x)
          end
        | ILTAST.EFix _ =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILAST.TNat, x, ILCPS.VZero, k x)
          end

        (* | ILTAST.EFix(t, e1) => *)
        (*   let *)
        (*       val fixtype = ILTAST.typeof e *)
        (*       val k' = KVar.fresh("k") *)
        (*       val unused = Var.fresh("_") *)
        (*       val x = Var.fresh("x") *)
        (*       val k'' = KVar.fresh("k") *)
        (*       val zero = Var.fresh("zero") *)
        (*   in *)
        (*       ILCPS.ELetCont( *)
        (*           k'', transform_type , x, k x, *)
        (*           ILCPS.ELetFun( *)
        (*               t, *)
        (*               ILAST.TArr(ILAST.TNat, transform_type fixtype, *)
        (*               k', *)
        (*               ILAST.TNat, *)
        (*               unused, *)
        (*               transform_val (ASTInterpreter.subst (ILAST.EApp(ILAST.EVar t, ILAST.EZero)) t e1) k', *)
        (*               ILCPS.ELetVal(ILAST.TNat, zero, ILCPS.VZero, ILCPS.EAppFun(t,k'',zero)))) *)
        (*   end *)


  and transform_val (ILTAST.Exp { exp, ty } : ILTAST.exp) (k : KVar.t) : ILCPS.exp =
      case exp of
          ILTAST.EVar x =>
          ILCPS.EAppCont(k, x)
        | ILTAST.EApp(e1, e2) =>
          transform_exp
              e1
              (fn x1 =>
                  transform_exp
                      e2
                      (fn x2 =>
                          ILCPS.EAppFun(x1, k, x2)))
        | ILTAST.EIfz(i, z, x, s) =>
          transform_exp
              i
              (fn n =>
                  let
                      val kz = KVar.fresh("k")
                      val unused = Var.fresh("_")
                      val ks = KVar.fresh("k")
                  in
                      ILCPS.ELetCont(
                          kz, ILAST.TNat, unused, transform_val z k,
                          ILCPS.ELetCont(
                              ks, ILAST.TNat, x, transform_val s k,
                              ILCPS.EIfz(n, kz, ks)))
                  end
              )
        | ILTAST.ELam(x, e1) =>
          let
              val (ILAST.TArr(t1, t2)) = ty
              val f = Var.fresh("f")
              val j = KVar.fresh("j")
          in
              ILCPS.ELetVal(
                  transform_type ty,
                  f,
                  ILCPS.VLam(transform_type t2, j, transform_type t1, x, transform_val e1 j),
                  ILCPS.EAppCont(k, f)
              )
          end
        | ILTAST.ESucc e1 =>
          let
              val x = Var.fresh("x")
          in
              transform_exp
                  e1
                  (fn x1 =>
                      ILCPS.ELetVal(ILAST.TNat, x, ILCPS.VSucc x1, ILCPS.EAppCont(k, x))
                  )
          end
        | ILTAST.EZero =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILAST.TNat, x, ILCPS.VZero, ILCPS.EAppCont(k, x))
          end
        | ILTAST.EFix _ =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILAST.TNat, x, ILCPS.VZero, ILCPS.EAppCont(k, x))
          end


fun transform (e : ILTAST.exp) : ILCPS.exp =
    transform_exp e (fn v => ILCPS.EAppCont(ILCPS.halt, v))

end
