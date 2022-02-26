structure BasicTests =
struct

  open Test

  fun fac 0 = 1
    | fac n = n * fac (n - 1);

  fun fib 0 = 0
    | fib 1 = 1
    | fib n = fib (n - 1) + fib (n - 1)

  fun toplevelTest () =
      let
          val two = fac 2
          val two' = fib 2
      in
          assertEqInt two two'
      end

  val tests =
      label "basic" (suite [
          label "fac" (
              suite [
                  label "three" (test (fn () => assert (fac 3 = 6))),
                  label "five" (test (fn () => assert (fac 5 = 120))),
                  label "fac of four, failing" (test (fn () => assertEqInt 120 (fac 4))),
                  label "fac of four, uncaught exception" (test (fn () => assert (fac 4 = 120 div 0))),
                  label "fib" (test toplevelTest)
          ]),
          label "fib" (
              label "three" (test (fn () => assert (fib 3 = 2)))
          )
      ]);


  fun main (name,args) = let
      val () = executeTest [] tests
  in
      OS.Process.success
  end
end

