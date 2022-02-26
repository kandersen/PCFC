structure Test :> TEST =
struct
  open PrettyPrint

  infix <>
  infix </>
  infix <+>
  val op<> = PrettyPrint.<>
  val op</> = PrettyPrint.</>
  val op<+> = PrettyPrint.<+>

  exception AssertionFailure of PrettyPrint.t

  datatype t
    = LABEL of string * t
    | TEST of unit -> unit
    | SUITE of t list

  fun label l t = LABEL(l,t)

  fun test action =
      TEST action

  fun suite ts = SUITE ts

  fun assert true = ()
    | assert false = raise AssertionFailure (text "assertion failed")

  fun assertEqInt a b =
      if a = b
      then ()
      else raise AssertionFailure (stack [text "expected:" <+> number a,
                                             text "actual:" <+> number b
                                          ]
                                  )

  fun assertEqString a b =
      if a = b
      then ()
      else raise AssertionFailure (stack [text "expected:" <+> text a,
                                          text "actual:" <+> text b
                                         ]
                                  )

  fun intersperse _ [] = []
    | intersperse d [x] = [x]
    | intersperse d (x::xs) = x :: d :: intersperse d xs

  fun format_list (ts : PrettyPrint.t list) : PrettyPrint.t =
      squares (endl <> indent 2 (stack (map (fn t => t <> text ",") ts)) <> endl)

  fun eqList cmp xs ys =
      case (xs, ys) of
          ([], []) =>
          true
        | ([], _) =>
          false
        | (_, []) =>
          false
        | (x :: xs', y :: ys') =>
          cmp x y andalso eqList cmp xs' ys'

  fun assertEqList cmp display a b =
      if eqList cmp a b
      then ()
      else raise AssertionFailure (
              stack [
                  text "expected:"
                       <+> format_list (map display a),
                  text "actual:"
                       <+> format_list (map display b)
              ]
          )

  type test_result = {
      msg : PrettyPrint.t,
      succeeded : int,
      failed : int
  }

  val none : test_result = {
      msg = empty,
      succeeded = 0,
      failed = 0
  }

  val pass : test_result = {
      msg = text "[PASS]",
      succeeded = 1,
      failed = 0
  }

  fun fail msg : test_result = {
      msg = text "[FAIL]" <+> brackets (endl <> indent 2 msg <> endl),
      succeeded = 0,
      failed = 1
  }

  type exp = string list

  fun exec e (LABEL(l, t')) =
      (case e of
          [] =>
          let val { msg, succeeded, failed } = exec e t' in
              { msg = text ":" <> text l <+> brackets (endl <> (indent 2 msg) <> endl),
                succeeded = succeeded,
                failed = failed
              }
          end
        | (l'::e') =>
          if not (l = l')
          then none
          else
              let val { msg, succeeded, failed } = exec e' t' in
        { msg = text ":" <> text l <+> brackets (endl <> (indent 2 msg) <> endl),
                    succeeded = succeeded,
                    failed = failed
                  }
              end)
    | exec e (TEST action) =
      ((action (); pass) handle AssertionFailure errMsg =>
                                fail errMsg
                              | _ =>
                                fail (text "uncaught exception"))
    | exec e (SUITE ts) =
      let val ts' = map (exec e) ts in
          {
            msg = stack (map #msg ts'),
            succeeded = foldr (op+) 0 (map #succeeded ts'),
            failed = foldr (op+) 0 (map #failed ts')
          }
      end

  fun executeTest (e : exp) (t : t) : unit =
      let
          val { msg, succeeded, failed } = exec e t
          val () = print (render (msg <> endl <> endl))
      in
          print (
              render (
                  if failed = 0
                  then text "All"
                            <+> number succeeded
                            <+> text "tests pass!" <> endl
                  else number failed
                              <+> text "failures" <> text ","
                              <+> number (succeeded + failed)
                              <+> text "tests ran." <> endl))
      end
end
