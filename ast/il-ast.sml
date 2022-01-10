structure ILAST =
struct
  datatype ty
    = TArr of ty * ty
    | TNat

  datatype exp
    = EVar of Variable.t
    | EApp of exp * exp
    | EIfz of exp * exp * (Variable.t * exp)
    | ELam of ty * (Variable.t * exp)
    | EFix of ty * (Variable.t * exp)
    | ESucc of exp
    | EZero

  fun pprint_ty ty =
      case ty of
          TNat =>
          "num"
        | TArr(t1, t2) =>
          pprint_ty t1 ^ " -> " ^ pprint_ty t2

  fun pprint_exp e =
      case e of
          EVar x => "var[" ^ Variable.pprint x ^ "]"
        | EApp(e1, e2) => "(" ^ pprint_exp e1 ^ ") (" ^ pprint_exp e2 ^ ")"
        | EIfz(n,z,(n',s)) =>
          "ifz " ^ pprint_exp n ^ " {\n" ^
          "  | 0 => " ^ pprint_exp z ^ "\n" ^
          "  | S " ^ Variable.pprint n' ^ " => " ^ pprint_exp s ^ "\n" ^
          "}"
        | ELam(t,(x,e)) => "\\ " ^ Variable.pprint x ^ " : " ^ pprint_ty t ^ " . " ^ pprint_exp e
        | EFix(t,(x,e)) => "fix " ^ Variable.pprint x ^ " : " ^ pprint_ty t ^ " . " ^ pprint_exp e
        | ESucc e => "S " ^ pprint_exp e
        | EZero => "0"
end
