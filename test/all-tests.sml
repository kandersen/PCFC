structure AllTests =
struct
open Test

val tests =
    suite [
        PrettyPrintTests.tests,
        BasicTests.tests
    ]

fun main (name,args) = let
    val () = executeTest [] tests
in
    OS.Process.success
end

end
