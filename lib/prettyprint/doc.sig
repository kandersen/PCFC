signature DOC =
sig
    type t

    val text : string -> t
    val empty : t
    val endl : t
    val indent : int -> t -> t
    val stack : t list -> t
    val <> : t * t -> t

    val render : t -> string
end
