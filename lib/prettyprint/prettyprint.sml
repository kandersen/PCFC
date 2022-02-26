structure PrettyPrint =
struct
    open Doc
    infix <>

    val number = text o Int.toString

    infix <+>
    fun s1 <+> s2 = s1 <> text " " <> s2

    infix </>
    fun s1 </> s2 = s1 <> endl <> s2

    fun number n = text (Int.toString n)

    fun parens s = text "(" <> s <> text ")"
    fun squares s = text "[" <> s <> text "]"
    fun brackets s = text "{" <> s <> text "}"
end
