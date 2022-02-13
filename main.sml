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
                          (case ASTCheck.check e of
                              NONE =>  raise Fail "AST expression not well-typed"
                            | SOME t1 => t1)
                          handle
                          ASTCheck.TypeError msg =>
                          raise Fail ("AST expression not well-typed:\n  " ^ msg)
                      val () =
                          print (pprint_ty t1 ^ "\n")

                      val e' =
                          (banner "AST Reduction";
                           ASTContexts.reduce e)
                      val r1 =
                          case extract e' of
                              NONE => raise Fail ("AST expression did not reduce to number:\n  " ^ pprint_exp e')
                            | SOME r1 => r1
                      val () =
                          print (Int.toString r1 ^ "\n")
                      val () =
                          if r1 = n
                          then ()
                          else raise Fail "AST expression did not reduce to expected value"


                      val () =
                          banner "Type Inference"
                      val tast =
                          Inference.inference e
                          handle
                          Inference.TypeError => raise Fail "TypeError"
                      val () =
                          print (TASTSExprs.prettyprint tast ^ "\n")



                      val c =
                          (banner "CPS Transformation";
                           CPSTransform.transform tast)
                      val () =
                          (banner "CPS Expression";
                           print (CPSSExprs.prettyprint c ^ "\n"))


                      val () =
                          (banner "CPS Checking";
                           CPSCheck.check c)


                      val v =
                          (banner "CPS Reduction";
                           CPSEval.eval c)
                          handle
                          CPSEval.RuntimeError msg => raise Fail ("CPS expression runtime failure:\n  " ^ msg)
                      val r2 =
                          case extract_cps v of
                              NONE => raise Fail "CPS expression did not reduce to number"
                            | SOME r2 => r2
                      val () =
                          if r2 = n
                          then ()
                          else raise Fail (
                                  "CPS expression did not reduce to expected value:\n"
                                  ^ "  expected: " ^ Int.toString n ^ "\n"
                                  ^ "  actual:   " ^ Int.toString r2 ^ "\n")
                  in
                      banner "OK"
                  end
              end

  fun test n e =
      run_test n e false
      handle
      _ => (run_test n e true
            handle
            Fail msg => print (msg ^ "\n\n"))


  val () = test 0 zero

  val () = test 5 (embed 5)

  val () =
      let val x = Variable.fresh("x") in
          test 0 (app (lam nat x (var x)) zero)
      end

  val () =
      let val x = Variable.fresh("x") in
          test 1 (app (lam nat x (succ (var x))) zero)
      end

  val () =
      let val x = Variable.fresh("x") in
          test 1 (ifz zero (succ zero) x (var x))
      end

  val () =
      let
          val x = Variable.fresh("x")
          val y = Variable.fresh("y")
          val i' = Variable.fresh("i'")
          val i = Variable.fresh("i")
      in
          test 2
                   (app (app (lam nat
                             i
                             (ifz (var i)
                                  (lam nat x (succ (var x)))
                                  i'
                                  (lam nat y (var y))))
                        zero) (succ zero))
      end

  val () =
      let
          val n' = Variable.fresh("n'")
      in
         test 1 (ifz zero
                         (succ zero)
                         n'
                         (succ (var n')))
      end

  val () =
      let
          val n = Variable.fresh("n")
          val n' = Variable.fresh("n'")
      in
          test 1 (app (lam nat
                               n
                               (ifz (var n)
                                    (succ zero)
                                    n'
                                    (succ (var n')))) zero)
      end

  val () =
      let
          val m = Variable.fresh("m")
          val n = Variable.fresh("n")
      in
          test 1 (app (app (lam nat m
                                    (lam nat n
                                         (var m))) (succ zero)) zero)
      end

  val () =
      let
          val m = Variable.fresh("m")
          val n = Variable.fresh("n")
          val n' = Variable.fresh("n'")
      in
          test 0 (app
                      (app
                           (lam nat m
                                (lam nat n
                                     (ifz (var m)
                                          (var m)
                                          n'
                                          (var n))
                                )
                           )
                           (succ zero)
                      )
                      zero
                 )
      end

  val () =
      let
          val m = Variable.fresh("m")
          val m' = Variable.fresh("m'")
          val n = Variable.fresh("n")

      in
          test 1 (app (app
                           (lam nat m
                                (lam nat n
                                     (ifz (var m)
                                          (var n)
                                          m'
                                          (succ (var n)))
                                )
                           )
                           (succ zero)
                      )
                      zero
                 )
      end

  val () =
      let
          val m = Variable.fresh("m")
          val m' = Variable.fresh("m'")
          val n = Variable.fresh("n")
          val x = Variable.fresh("x")
          fun thunk t e = lam t (Variable.fresh("_")) e
          val omega = (thunk nat (thunk nat (fix nat x (var x))))
          val this = Variable.fresh("this")
          val plusF =
              (lam (arr nat (arr nat nat))
                   this
                   (lam nat m
                        (lam nat n
                             (ifz (var m)
                                  (var n)
                                  m'
                                  (succ (app (app (var this) (var m')) (var n))))
                        )
                   )
              )
      in
          run_test 3 (app (app (app plusF omega) zero) (embed 3)) true
      end



  val () =
      let
          fun omega () =
              let
                  val x = Variable.fresh("x")
                  fun thunk t e = lam t (Variable.fresh("_")) e
              in
                  thunk nat (thunk nat (fix nat x (var x)))
              end
          fun plusF () =
              let
                  val m = Variable.fresh("m")
                  val m' = Variable.fresh("m'")
                  val n = Variable.fresh("n")
                  val this = Variable.fresh("this")
              in
                  lam (arr nat (arr nat nat))
                      this
                      (lam nat m
                           (lam nat n
                                (ifz (var m)
                                     (var n)
                                     m'
                                     (succ (app (app (var this) (var m')) (var n))))
                           )
                      )
              end
      in
          (run_test 3 (app (app (app (plusF ())
                                     (omega ()))
                                zero)
                           (embed 3)) false;
           run_test 4 (app (app (app (plusF ())
                                     (app (plusF ())
                                          (omega ())))
                                (succ zero))
                           (embed 3)) false)
      end

end
