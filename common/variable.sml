signature VARIABLE =
sig
    type t

    val fresh : string -> t
    val eqvar : t -> t -> bool
    val compare : (t * t) -> order
    val pprint : t -> string
    val disamb : t -> string
end

functor Variable() :> VARIABLE =
struct
    type t = int * string

    val nextVar = ref 0
    fun fresh s =
        let
            val v = !nextVar
        in
            nextVar := v + 1;
            (v, s)
        end

    fun eqvar ((n1, _) : t) (n2, _) = n1 = n2

    fun compare ((n1, _), (n2, _) : t)= Int.compare(n1, n2)

    fun pprint (v, s) = s

    fun disamb (v, s) = s ^ Int.toString v
end

functor VarMap(structure V : VARIABLE) : ORD_MAP =
  RedBlackMapFn(type ord_key = V.t
                val compare = V.compare)
