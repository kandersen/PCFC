structure ASTInterpreter =
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

  fun value e =
      case e of
          EZero => true
        | ESucc e' => value e'
        | ELam _ => true
        | _ => false

  fun step e =
      case e of
          ESucc e' =>
          ESucc (step e')
        | EIfz(i, z, (n', s)) =>
          (case i of
               EZero =>
               z
             | ESucc i' =>
               subst i' n' s
             | _ => EIfz(step i, z, (n', s))
          )
        | EApp(e1,e2) =>
          if not (value e1)
          then EApp(step e1, e2)
          else if not (value e2)
          then EApp(e1, step e2)
          else (case e1 of
                    ELam(_,(x,e1')) =>
                    subst e2 x e1'
                  | _ => raise RuntimeError "Applying non-lambda"
               )
        | EFix(_,(t,e')) =>
          subst e t e'
        | _ => raise RuntimeError ("Cannot step value: " ^ pprint_exp e)

  fun reduce e =
      let
          fun iter e =
              if value e
              then e
              else iter (step e)
      in
          iter e
          handle RuntimeError err => raise RuntimeError ("In " ^ pprint_exp e ^ ":\n\t" ^ err)
      end

end
