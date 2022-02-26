signature INFERENCE =
sig
    exception TypeError
    val inference : ILAST.exp -> ILTAST.exp
end
