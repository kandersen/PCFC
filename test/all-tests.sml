structure AllTests =
struct
open Test

val tests =
    suite [
        PrettyPrintTests.tests,
        BasicTests.tests
    ]


fun main (name,args) =
    let
        val expectations =
            case args of
                [] => []
              | [query] =>
                parseExpectations query
        val () = executeTest expectations tests
    in
        OS.Process.success
    end
end
