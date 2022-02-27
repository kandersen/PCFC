signature TEST =
sig
    val assert : bool -> unit
    val assertEqInt : int -> int -> unit
    val assertEqString : string -> string -> unit
    val assertEqList : ('a -> 'a -> bool) -> ('a -> PrettyPrint.t) -> 'a list -> 'a list -> unit

    type t

    val test : (unit -> unit) -> t
    val label : string -> t -> t
    val suite : t list -> t

    type exp = string list

    val parseExpectations : string -> exp
    val executeTest : exp -> t -> unit
end
