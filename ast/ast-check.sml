signature AST_CHECK =
sig
    type tenv
    exception TypeError of string

    val check : ILAST.exp -> ILAST.ty option
end

structure ASTCheck : AST_CHECK =
struct
  open ILAST
  open Variable

  exception TypeError of string

  structure VarDict = VarMap(structure V = Variable)

  type tenv = ty VarDict.map

  val emptytenv = VarDict.empty

  fun infer tenv e =
      case e of
          ESucc e' =>
          let val () = assert tenv e' TNat in
              SOME TNat
          end
        | EZero =>
          SOME TNat
        | EVar v =>
          (case VarDict.find(tenv, v) of
              NONE =>
              raise TypeError ("Unbound variable: " ^ Variable.disamb v)
            | SOME t =>
              SOME t
          )
        | EApp (f, a) =>
          (case infer tenv f of
              SOME (TArr (t1, t2)) =>
              let val () = assert tenv a t1 in
                  SOME t2
              end
            | _ => raise TypeError "Non-function in function position")
        | ELam(t,(x,e)) =>
          (case infer (VarDict.insert(tenv, x, t)) e of
              SOME t2 =>
              SOME (TArr(t, t2))
            | NONE => NONE
          )
        | EFix(t,(x,e)) =>
          let val () = assert (VarDict.insert(tenv, x, t)) e t in
              SOME t
          end
        | EIfz (e1, e2, (n', e3)) =>
          let
              val () = assert tenv e1 TNat
              val SOME t = infer tenv e2
          in
              assert (VarDict.insert(tenv, n', TNat)) e3 t;
              SOME t
          end

  and assert tenv e t =
      case infer tenv e of
          SOME t' =>
          if t <> t'
          then raise TypeError "Unsatisfied type constraint"
          else ()
        | NONE => raise TypeError "Uninferable type"

  fun check e =
      infer emptytenv e

end
