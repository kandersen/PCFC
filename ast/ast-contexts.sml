structure ASTContexts =
struct
  open ILAST

  exception RuntimeError of string

  fun subst v x e =
      case e of
          EZero =>
          EZero
        | ESucc e' =>
          ESucc (subst v x e')
        | EVar y =>
          if Variable.eqvar x y
          then v
          else e
        | ELam(t,(y,e')) =>
          if Variable.eqvar x y
          then e
          else ELam(t,(y,subst v x e'))
        | EFix(t,(y,e')) => 
          if Variable.eqvar x y
          then e
          else EFix(t,(y,subst v x e'))
        | EIfz(i,z,(n',s)) =>
          EIfz(
              subst v x i,
              subst v x z,
              (n',
               if Variable.eqvar x n'
               then s
               else subst v x s)
          )
        | EApp(e1,e2) =>
          EApp(subst v x e1, subst v x e2)

  datatype frame
    = FSucc
    | FIfz of exp * (Variable.t * exp)
    | FAppOp of exp
    | FAppArg of ty * (Variable.t * exp)

  type stack = frame list

  datatype state
    = SEval of stack * exp
    | SReturn of stack * exp


  fun step s =
      case s of
          SEval(k, EZero) => SReturn(k, EZero)
        | SEval(k, ESucc(e)) => SEval(FSucc :: k, e)
        | SReturn(FSucc :: k, e) => SReturn(k, ESucc e)

        | SEval(k, EIfz(e, e0, e1)) => SEval(FIfz(e0, e1) :: k, e)
        | SReturn(FIfz(e0,_) :: k, EZero) => SEval(k, e0)
        | SReturn(FIfz(_,(x,e1)) :: k, ESucc e) => SEval(k, subst e x e1)

        | SEval(k, ELam(ty, (x, e))) => SReturn(k, ELam(ty, (x, e)))
        | SEval(k, EApp(e1, e2)) => SEval(FAppOp e2 :: k, e1)
        | SReturn(FAppOp e2 :: k, ELam lam) => SEval(FAppArg lam :: k, e2)
        | SReturn(FAppArg(_, (x, e)) :: k, v) => SEval(k, subst v x e)

        | SEval(k, (e' as EFix(_, (x, e)))) => SEval(k, subst e' x e)

  fun reduce e =
      let fun iter s =
          case s of
              SReturn([], e) => e
            | _ => iter (step s)
      in
          iter (SEval([], e))
      end
end
