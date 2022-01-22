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

  fun banner s =
      print ("********************\n" ^ "* " ^ s ^ "\n********************\n")

  fun run_test n e =
      let 
          fun run_test n e verbose =
              let
                  val (banner, print) =
                      if verbose
                      then (banner, print)
                      else (fn _ => (), fn _ => ())
              in
                  let
                      val () =
                          (banner "AST Expression";
                           print (pprint_exp e ^ "\n"))
                      val () =
                          banner "AST Checking"
                      val t1 =
                          case ASTCheck.check e of
                              NONE =>  raise Fail "AST expression not well-typed"
                            | SOME t1 => t1
                          handle
                          ASTCheck.TypeError => raise Fail "AST expression not well-typed"
                      val () =
                           print (pprint_ty t1 ^ "\n")
                      val () =
                          banner "AST Reduction"
                      val r1 =
                          case extract (ASTContexts.reduce e) of
                              NONE => raise Fail "AST expression did not reduce to number"
                            | SOME r1 => r1
                      val () =
                          if r1 = n
                          then ()
                          else raise Fail "AST expression did not reduce to expected value"
                      val c =
                          (banner "CPS Transformation";
                           CPSTransform.transform e)
                      val () =
                          (banner "CPS Checking";
                           CPSCheck.check c)
                      val () =
                          (banner "CPS Expression";
                           print (PPrintCPS.prettyprint c ^ "\n"))
                      val v =
                          (banner "CPS Reduction";
                           CPSEval.eval c)
                      val r2 =
                          case extract_cps v of
                              NONE => raise Fail "CPS expression did not reduce to number"
                            | SOME r2 => r2
                      val () =
                          if r2 = n
                          then ()
                          else raise Fail "CPS expression did not reduce to expected value"
                  in
                      banner "OK"
                  end
              end
      in
          run_test n e false
          handle
          _ => (run_test n e true
                handle
                Fail msg => print (msg ^ "\n\n"))
      end


  val () = run_test 0 zero

  val () = run_test 5 (embed 5)

  val () =
      let val x = Variable.fresh("x") in
          run_test 0 (app (lam nat x (var x)) zero)
      end

  val () =
      let val x = Variable.fresh("x") in
          run_test 1 (app (lam nat x (succ (var x))) zero)
      end

  val () =
      let val x = Variable.fresh("x") in
          run_test 1 (ifz zero (succ zero) x (var x))
      end

  val () =
      let val x = Variable.fresh("x") in
          run_test 1 (ifz zero (succ zero) x (var x))
      end

  val () =
      let
          val x = Variable.fresh("x")
          val i = Variable.fresh("i")
      in
          run_test 45
                   (app (lam nat
                             i
                             (ifz (var i)
                                  zero
                                  x (succ (var x))))
                        (succ zero))
      end


end
