structure PrettyPrintTests =
struct
  open Test
  open PrettyPrint

  infix <+>
  infix <>

  val assertEqRenderCommands =
      assertEqList
          (fn a => fn b => a = b)
          (fn (rc : render_cmd) =>
              case rc of
                  RC_NEWLINE => text "RC_NEWLINE"
                | RC_INDENT i => text "RC_INDENT" <> parens (number i)
                | RC_TEXT s => text "RC_TEXT" <> parens (text s)
          )

  fun render_cmd_test (l, cmds, t) =
      label l (test (fn () => assertEqRenderCommands cmds (render_commands t)))

  fun render_test (l, expected, t) =
      label l (test (fn () => assertEqString expected (render t)))

  val tests =
      label "prettyprint-tests" (
          suite [
              label "render-command-tests"
                    (suite
                         (map render_cmd_test [
                               ("empty",
                                [],
                                empty),

                               ("single",
                                [RC_TEXT "single"],
                                text "single"),

                               ("indent",
                                [RC_INDENT 2, RC_TEXT "a"],
                                indent 2 (text "a")),

                               ("parens",
                                [RC_TEXT "(", RC_TEXT "a", RC_TEXT ")"],
                                parens (text "a")),

                               ("sexp one line",
                                map RC_TEXT ["(", "app", " ", "a", " ", "b", " ", "c", ")"],
                                parens (text "app" <+> text "a" <+> text "b" <+> text "c")),

                               ("sexp stack",
                                [RC_TEXT "(", RC_TEXT "app", RC_TEXT " ", RC_TEXT "a",
                                 RC_NEWLINE, RC_INDENT 5, RC_TEXT "b",
                                 RC_NEWLINE, RC_INDENT 5, RC_TEXT "c", RC_TEXT ")"],
                                parens (text "app" <+> stack (map text ["a", "b", "c"]))
                               ),

                               ("stack",
                                [ RC_TEXT "a",
                                  RC_NEWLINE, RC_TEXT "b",
                                  RC_NEWLINE, RC_TEXT "c" ],
                                stack (map text ["a", "b", "c"])),

                               ("indent stack",
                                [ RC_INDENT 2, RC_TEXT "a",
                                  RC_NEWLINE, RC_INDENT 2, RC_TEXT "b",
                                  RC_NEWLINE, RC_INDENT 2, RC_TEXT "c" ],
                                indent 2 (stack (map text ["a", "b", "c"])))



                    ])),
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
