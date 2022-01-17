signature PPRINT =
sig
    type doc

    val empty : doc
    val <> : doc * doc -> doc
    val nest : int -> doc -> doc
    val text : string -> doc
    val line : doc
    val or : doc -> doc -> doc

    val group : doc -> doc

    val flatten : doc -> doc

    val pretty : int -> doc -> string
end

structure PPrint :> PPRINT =
struct
  datatype doc
    = EMPTY
    | APP of doc * doc
    | NEST of int * doc
    | TEXT of string
    | LINE
    | OR of doc * doc

  datatype idoc
    = IEMPTY
    | ITEXT of string * idoc
    | ILINE of int * idoc

  val empty = EMPTY
  fun x <> y = APP(x, y)
  fun nest i d = NEST(i, d)
  fun text s = TEXT s
  val line = LINE
  fun or x y = OR(x, y)

  fun flatten EMPTY = EMPTY
    | flatten (APP(x,y)) = APP(flatten x, flatten y)
    | flatten (NEST(i,x)) = NEST(i, flatten x)
    | flatten (TEXT s) = TEXT s
    | flatten LINE = TEXT " "
    | flatten (OR(x, y)) = flatten x


  fun group x = OR(flatten x, x)


  fun copy 0 c = c
    | copy n c = c ^ copy (n - 1) c

  fun layout IEMPTY = ""
    | layout (ITEXT(s,x)) = s ^ layout x
    | layout (ILINE(i, x)) = "\n" ^ copy i " "  ^ layout x

  fun fits w x =
      if w < 0
      then false
      else case x of
               IEMPTY => true
             | ITEXT(s, x') => fits (w - String.size s) x'
             | ILINE(_, _) => true


  fun better w k x y = if fits (w - k) x then x else y

  fun be w k [] = IEMPTY
    | be w k ((i, EMPTY) :: z) = be w k z
    | be w k ((i, (APP(x, y))) :: z) = be w k ((i, x) :: (i, y) :: z)
    | be w k ((i, (NEST(j, x))) :: z) = be w k ((i + j, x) :: z)
    | be w k ((i, TEXT(s)) :: z) = ITEXT(s, be w (k + String.size s) z)
    | be w k ((i, LINE) :: z) = ILINE(i, be w i z)
    | be w k ((i, OR(x, y)) :: z) = better w k (be w k ((i, x) :: z))
                                               (be w k ((i, y) :: z))

  fun best w k x = be w k [(0, x)]

  fun pretty w x = layout (best w 0 x)

end

structure PPrintUtils =
struct
  open PPrint

  infix <>
  infix <+>
  infix </>
  infix <+/>
  fun x <+> y = x <> text " " <> y
  fun x </> y = x <> line <> y

  fun folddoc f [] = empty
    | folddoc f [x] = x
    | folddoc f (x :: xs) = f x (folddoc f xs)

  val spread = folddoc (fn x => fn y => x <+> y)
  val stack = folddoc (fn x => fn y => x </> y)

  fun bracket l x r = group (text l <>
                             nest 2 (line <> x) <>
                             line <> text r)

  fun x <+/> y = x <> (or (text " ") line) <> y
end
