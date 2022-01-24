structure Inference : INFERENCE =
struct
  open ILTAST

  exception TypeError

  val emptytenv = VarMap.empty

  fun infer tenv (e : ILAST.exp) : exp =
      case e of
          ILAST.EZero =>
          zero
        | ILAST.ESucc e' =>
          succ (check tenv e' ILAST.TNat)
        | ILAST.EVar x =>
          (case VarMap.find(tenv, x) of
               NONE =>
               raise TypeError
             | SOME t =>
               var t x
          )
        | ILAST.EApp(e1, e2) =>
          let
              val e1' = infer tenv e1
          in
              case typeof e1' of
                  ILAST.TArr(t1, t2) =>
                  app t2 e1' (check tenv e2 t1)
                | _ => raise TypeError
          end
        | ILAST.ELam(t1, (x, e1)) =>
          let val e1' = infer (VarMap.insert(tenv, x, t1)) e1 in
              lam t1 (typeof e1') x e1'
          end
        | ILAST.EFix(t, (x, e1)) =>
          let val e1' = check (VarMap.insert(tenv, x, t)) e1 t in
              fix t x e1'
          end
        | ILAST.EIfz(e1, e2, (n', e3)) =>
          let
              val e1' = check tenv e1 ILAST.TNat
              val e2' = infer tenv e2
              val e3' = check (VarMap.insert(tenv, n', ILAST.TNat)) e3 (typeof e2')
          in
              ifz (typeof e2') e1' e2' n' e3'
          end

  and check tenv (e : ILAST.exp) ty : exp =
      let val e' = infer tenv e in
          if ty <> typeof e'
          then raise TypeError
          else e'
      end

  fun inference (e : ILAST.exp) : exp =
      infer emptytenv e

end
