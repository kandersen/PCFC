signature DOC =
sig
    type t

    val text : string -> t
    val number : int -> t
    val empty : t
    val endl : t
    val stack : t list -> t
    val indent : int -> t -> t

    val render : t -> string
end
