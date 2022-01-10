signature VARIABLE =
sig
    type t

    val fresh : unit -> t
    val eqvar : t -> t -> bool
    val compare : (t * t) -> order
    val pprint : t -> string
end

structure Variable :> VARIABLE =
struct
    type t = int

    val nextVar = ref 0
    fun fresh () =
        let
            val v = !nextVar
        in
            nextVar := v + 1;
            v
        end

    fun eqvar (v1 : int) v2 = v1 = v2

    val compare = Int.compare

    fun pprint v = "[" ^ Int.toString v ^ "]"
end

structure VarOrdKey : ORD_KEY =
struct
  type ord_key = Variable.t
  val compare = Variable.compare
end
