structure Doc :> DOC =
struct
  datatype t
    = EMPTY
    | TEXT of string
    | APP of t * t
    | NEWLINE
    | INDENT of int * t
    | STACK of t * t list

  fun text s = TEXT s

  val empty = EMPTY

  val endl = NEWLINE

  infix <>
  fun s1 <> s2 = APP(s1, s2)

  fun indent i t = INDENT(i, t)

  fun stack [] = empty
    | stack [t] = t
    | stack (t::ts) = STACK(t, ts)

  datatype render_cmd
    = RC_NEWLINE
    | RC_INDENT of int
    | RC_TEXT of string

  fun copy 0 c = ""
    | copy n c = c ^ copy (n - 1) c

  fun run_renderer cmds =
      case cmds of
          [] =>
          ""
        | c :: cmds' =>
          case c of
              RC_NEWLINE =>
              "\n" ^ run_renderer cmds'
            | RC_INDENT i =>
              copy i " " ^ run_renderer cmds'
            | RC_TEXT s =>
              s ^ run_renderer cmds'

  fun isEmpty t =
      case t of
          EMPTY =>
          true
        | TEXT s =>
          String.size s = 0
        | APP(t1, t2) =>
          isEmpty t1 andalso isEmpty t2
        | NEWLINE =>
          false
        | INDENT(i, t') =>
          i = 0 andalso isEmpty t'
        | STACK(t',ts) =>
          isEmpty t' andalso List.all isEmpty ts

  fun concatStack   _ _            [] =
      []
    | concatStack col i (t     :: ts) =
      if isEmpty t
      then concatStack col i ts
      else [(col + i, NEWLINE), (col + i, t)] @ concatStack col i ts

  fun layout col [] =
      []

    | layout col ((_, EMPTY) :: z) =
      layout col z

    | layout col ((i, (APP(x, y))) :: z) =
      layout col ((i, x) :: (i, y) :: z)

    | layout col ((i, (TEXT(s))) :: z) =
      if col < i
      then RC_INDENT (i - col) :: RC_TEXT s :: layout (i + String.size s) z
      else RC_TEXT s :: layout (col + String.size s) z

    | layout col ((i, (INDENT (j, x))) :: z) =
      layout col ((i + j, x) :: z)

    | layout col ((i, NEWLINE) :: z) =
      RC_NEWLINE :: layout 0 z

    | layout col ((i, STACK(t, ts)) :: z) =
      layout col ((i, t) :: (concatStack col i ts @ z))

  fun render_commands x = layout 0 [(0, x)]

  val render = run_renderer o render_commands
end
