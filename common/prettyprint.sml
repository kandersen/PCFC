signature PRETTYPRINT =
sig
    type t

    val run : t -> string

    val text : string -> t
    val endl : t
    val empty : t

    val <> : t * t -> t
    val <+> : t * t -> t
    val </> : t * t -> t

    val foldt : (t -> 'a -> 'a) -> 'a -> t list -> 'a

    val stack : t list -> t
    val parens : t -> t
    val indent : int -> t -> t
end

structure PrettyPrint :> PRETTYPRINT =
struct
    datatype t
      = EMPTY
      | TEXT of string
      | APP of t * t
      | NEWLINE
      | INDENT of int * t

    fun text s = TEXT s

    val empty = EMPTY
    val endl = NEWLINE

    infix <>
    fun s1 <> s2 = APP(s1, s2)

    fun indent i t = INDENT(i, t)

    fun copy 0 c = c
      | copy n c = c ^ copy (n - 1) c

    fun be [] = ""
      | be ((_, EMPTY) :: z) = be z
      | be ((i, (APP(x, y))) :: z) = be ((i, x) :: (i, y) :: z)
      | be ((i, (TEXT(s))) :: z) = s ^ be z
      | be ((i, (INDENT (j, x))) :: z) = be ((i + j, x) :: z)
      | be ((i, NEWLINE) :: z) = "\n" ^ copy i " " ^ be z

    fun best x = be [(0, x)]

    fun run x = best x

    (* Derived forms *)
    infix <+>
    fun s1 <+> s2 = s1 <> text " " <> s2

    infix </>
    fun s1 </> s2 = s1 <> endl <> s2

    fun parens s = text "(" <> s <> text ")"

    fun format_binding n t = parens (n <+> text ":" <+> t)

    fun foldt f acc [] = acc
      | foldt f acc (t :: ts) = f t (foldt f acc ts)

    fun stack [] = empty
      | stack [t] = t
      | stack (t::ts) = t </> stack ts
end

structure PPTests =
struct
  open PrettyPrint
  infix <>
  infix <+>
  infix </>


  val test1 = parens (text "app" <+> indent 4 (stack [text "line 1", text "line 2", text "line 3"]))
  fun foo _ = (print o run) (test1 <> endl)
end
