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
          succ (check tenv e' TNat)
        | ILAST.EVar x =>
          (case VarMap.find(tenv, x) of
               NONE =>
               raise TypeError
             | SOME t =>
               var x t
          )
        | ILAST.EApp(e1, e2) =>
          let
              val e1' = infer tenv e1
          in
              case typeof e1' of
                  TArr(t1, t2) =>
                  app e1' (check tenv e2 t1) t2
                | _ => raise TypeError
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
