structure PrettyPrintTests =
struct
  open Test
  open PrettyPrint

  infix <+>
  infix <>

  fun render_test (l, expected, t) =
      label l (test (fn () => assertEqString expected (render t)))

  val tests =
      label "prettyprint-tests" (
          suite [
              label "rendering"
                    (suite
                         (map render_test [
                               ("empty",
                                "",
                                empty),

                               ("indent",
                                "  a",
                                indent 2 (text "a")),

                               ("parens",
                                "(a)",
                                parens (text "a")),

                               ("sexp one line",
                                "(app a b c)",
                                parens (text "app" <+> text "a" <+> text "b" <+> text "c")),

                               ("sexp stack",
                                "(app a\n     b\n     c)",
                                parens (text "app" <+> stack (map text ["a", "b", "c"]))
                               ),

                               ("stack",
                                "a\nb\nc",
                                stack (map text ["a", "b", "c"])),

                               ("indented stack",
                                "  a\n  b\n  c",
                                indent 2 (stack (map text ["a", "b", "c"])))

                    ]))

          ]

      )

  fun main (name,args) = let
      val () = executeTest [] tests
  in
      OS.Process.success
  end
end
