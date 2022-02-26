signature PRETTYPRINT =
sig
    type t

    val render : t -> string

    val text : string -> t
    val number : int -> t
    val endl : t
    val empty : t

    val <> : t * t -> t
    val <+> : t * t -> t
    val </> : t * t -> t

    val stack : t list -> t
    val parens : t -> t
    val brackets : t -> t
    val squares : t -> t
    val indent : int -> t -> t

    val format_binding : t -> t -> t

    datatype render_cmd
      = RC_NEWLINE
      | RC_INDENT of int
      | RC_TEXT of string

    val render_commands : t -> render_cmd list
end
