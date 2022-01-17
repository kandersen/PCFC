structure Tests =
struct
  open ILAST

  val zero = EZero
  fun succ e = ESucc e
  fun ifz e1 e2 x e3 = EIfz(e1, e2, (x, e3))
  fun lam ty x e = ELam(ty, (x, e))
  fun app e1 e2 = EApp(e1, e2)
  fun fix ty x e = EFix(ty, (x, e))
  fun var x = EVar x

  val nat = TNat
  fun arr t1 t2 = TArr(t1, t2)

  fun embed 0 = zero
    | embed n = succ (embed (n - 1))

  fun extract EZero =
      SOME 0
    | extract (ESucc n) =
      (case extract n of
           SOME n' => SOME (n' + 1)
         | NONE => NONE)
    | extract _ = NONE

  local
      open CPSEval
  in
  fun extract_cps RVZero =
      SOME 0
    | extract_cps (RVSucc n) =
      (case extract_cps n of
           SOME n' => SOME (n' + 1)
         | NONE => NONE)
    | extract_cps _ = NONE
  end

  fun run_test n e =
      let
          val _ =
              print (pprint_exp e ^ "\n")
          val SOME t1 = ASTCheck.check e
          val _ = print (pprint_ty t1 ^ "\n")
          val SOME r1 = extract (ASTContexts.reduce e)
          val true = r1 = n
          val c = CPSTransform.transform e
          val () = CPSCheck.check c
          val _ = print (PPrintCPS.prettyprint c ^ "\n")
          val v = CPSEval.eval c
          val SOME r2 = extract_cps v
          val true = r2 = n
      in
          print "OK\n\n"
      end

  val () = run_test 0 zero

  val () = run_test 5 (embed 5)

  val () =
      let val x = Variable.fresh("x") in
          run_test 0 (app (lam nat x (var x)) zero)
      end

end
