structure CPSTransform : CPS_TRANSFORM =
struct

  structure Var = ILCPS.Variable
  structure KVar = ILCPS.KVariable

  fun transform_type (t : ILAST.ty) : ILCPS.ty =
    ILCPS.TNat

  fun transform_exp (e: ILAST.exp) (k : Var.t -> ILCPS.exp) : ILCPS.exp =
      case e of
          ILAST.EVar x =>
          k x
        | ILAST.EApp(e1, e2) =>
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
                              ILCPS.ELetCont(k', ILCPS.TNat, x', k x', ILCPS.EAppFun(x1, k', x2))
                          end))
        | ILAST.EIfz(i, z, (x, s)) =>
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
                      ILCPS.ELetCont(j, ILCPS.TNat, jx, k jx,
                                     ILCPS.ELetCont(kz, ILCPS.TNat, unused, transform_val z j,
                                                    ILCPS.ELetCont(ks, ILCPS.TNat, x, transform_val s j,
                                                                   ILCPS.EIfz(n, kz, ks))))
                  end
              )
        | ILAST.ELam(ty, (x, e)) =>
          let
              val f = Var.fresh("f")
              val k' = KVar.fresh("k")
          in
              ILCPS.ELetFun(f, transform_type ty, k', ILCPS.TNat, x, transform_val e k', k f)
          end
        | ILAST.ESucc e1 =>
          let
              val x = Var.fresh("x")
          in
              transform_exp
                  e1
                  (fn x1 =>
                      ILCPS.ELetVal(ILCPS.TNat, x, ILCPS.VSucc x1, k x))
          end
        | ILAST.EZero =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILCPS.TNat, x, ILCPS.VZero, k x)
          end
        | ILAST.EFix(ty, (t, e1)) =>
          let
              val k' = KVar.fresh("k")
              val unused = Var.fresh("_")
              val x = Var.fresh("x")
              val k'' = KVar.fresh("k")
              val zero = Var.fresh("zero")
          in
              ILCPS.ELetCont(
                  k'', transform_type ty, x, k x,
                  ILCPS.ELetFun(
                      t,
                      ILCPS.TArr(ILCPS.TNat, transform_type ty),
                      k',
                      ILCPS.TNat,
                      unused,
                      transform_val (ASTInterpreter.subst (ILAST.EApp(ILAST.EVar t, ILAST.EZero)) t e1) k',
                      ILCPS.ELetVal(ILCPS.TNat, zero, ILCPS.VZero, ILCPS.EAppFun(t,k'',zero))))
          end


  and transform_val (e : ILAST.exp) (k : KVar.t) : ILCPS.exp =
      case e of
          ILAST.EVar x =>
          ILCPS.EAppCont(k, x)
        | ILAST.EApp(e1, e2) =>
          transform_exp
              e1
              (fn x1 =>
                  transform_exp
                      e2
                      (fn x2 =>
                          ILCPS.EAppFun(x1, k, x2)))
        | ILAST.EIfz(i, z, (x, s)) =>
          transform_exp
              i
              (fn n =>
                  let
                      val kz = KVar.fresh("k")
                      val unused = Var.fresh("_")
                      val ks = KVar.fresh("k")
                  in
                      ILCPS.ELetCont(
                          kz, ILCPS.TNat, unused, transform_val z k,
                          ILCPS.ELetCont(
                              ks, ILCPS.TNat, x, transform_val s k,
                              ILCPS.EIfz(n, kz, ks)))
                  end
              )
        | ILAST.ELam(ty, (x, e1)) =>
          let
              val f = Var.fresh("f")
              val j = KVar.fresh("j")
          in
              ILCPS.ELetVal(
                  transform_type ty,
                  f,
                  ILCPS.VLam(transform_type ty, j, ILCPS.TNat, x, transform_val e1 j),
                  ILCPS.EAppCont(k, f)
              )
          end
        | ILAST.ESucc e1 =>
          let
              val x = Var.fresh("x")
          in
              transform_exp
                  e1
                  (fn x1 =>
                      ILCPS.ELetVal(ILCPS.TNat, x, ILCPS.VSucc x1, ILCPS.EAppCont(k, x))
                  )
          end
        | ILAST.EZero =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILCPS.TNat, x, ILCPS.VZero, ILCPS.EAppCont(k, x))
          end
        | ILAST.EFix _ =>
          let
              val x = Var.fresh("x")
          in
              ILCPS.ELetVal(ILCPS.TNat, x, ILCPS.VZero, ILCPS.EAppCont(k, x))
          end


fun transform (e : ILAST.exp) : ILCPS.exp =
    transform_exp e (fn v => ILCPS.EAppCont(ILCPS.halt, v))

end
