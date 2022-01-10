structure Tests =
struct
  open ILAST

  val varn = Variable.fresh()
  val varx = Variable.fresh()
  val varrec = Variable.fresh()
  val varm = Variable.fresh()
  val apprecx = EApp (EVar varrec, EVar varx)
  val appapprecxn = EApp(apprecx, EVar varn)
  val succbranch = ESucc appapprecxn
  val zerobranch = EVar varn
  val ifzexp = EIfz(EVar varm, zerobranch, (varx, succbranch))
  val lamn = ELam(TNat, (varn, ifzexp))
  val lamm = ELam(TNat, (varm, lamn))
  val plus = EFix(TArr(TNat,TArr(TNat,TNat)), (varrec, lamm))
  val SOME(TArr (TNat, TArr(TNat, TNat))) = ASTCheck.infer (ASTCheck.emptytenv) plus

  fun embed 0 = EZero
    | embed n = ESucc(embed(n - 1))

  fun extract EZero =
      SOME 0
    | extract (ESucc n) =
      (case extract n of
           SOME n' => SOME (n' + 1)
         | NONE => NONE)
    | extract _ = NONE

  val five = EApp(EApp(plus, embed 2), embed 3)
  val SOME TNat = ASTCheck.infer (ASTCheck.emptytenv) five
  val SOME 5 = extract (ASTInterpreter.reduce five)
               handle
               ASTInterpreter.RuntimeError e =>
               let val () = print e in NONE end
end


structure Main =
struct
  open Tests

  fun main (prog_name, args) =
      let
          val () = print (ILAST.pprint_exp five)
          val () = print (ILAST.pprint_exp (ASTInterpreter.reduce five))
      in
          1
      end
end

val () = print (ILAST.pprint_exp Tests.five)
val () = print (ILAST.pprint_exp (ASTInterpreter.reduce Tests.five))
