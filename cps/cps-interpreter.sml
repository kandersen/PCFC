structure CPSEval =
struct
  open ILCPS

  exception RuntimeError of string

  datatype env
    = Env of (runtimevalue VarMap.map) * ((env * (var * exp)) KVarMap.map)

  and runtimevalue
    = RVZero
    | RVSucc of runtimevalue
    | RVLam of env * cont * var * exp

  val emptyenv = Env(VarMap.empty, KVarMap.empty)
  fun vlookup (Env(e, _)) x = VarMap.lookup(e,x)
  fun klookup (Env(_, e)) k = KVarMap.lookup(e,k)

  fun vextend (Env(ev, ek)) x v = Env(VarMap.insert(ev,x,v), ek)
  fun kextend (Env(ev, ek)) k v = Env(ev, KVarMap.insert(ek,k,v))

  fun eval_exp (e : exp) (env : env) : runtimevalue =
      case e of
          EAppCont(k,x) =>
          if KVariable.eqvar halt k
          then vlookup env x
          else let
              val (env', (y, K)) = klookup env k
          in
              eval_exp K (vextend env y (vlookup env x))
          end
        | ELetVal(_,x,v,K) =>
          eval_exp K (vextend env x (eval_value v env))
        | ELetCont(k, _, x, e, K) =>
          eval_exp K (kextend env k (env, (x, e)))
        | EIfz(x, k1, k2) =>
          (case vlookup env x of
              RVZero =>
              let
                  val (env', (y, K)) = klookup env k1
              in
                  eval_exp K (vextend env' y RVZero)
              end
            | RVSucc x =>
              let
                  val (env', (y, K)) = klookup env k2
              in
                  eval_exp K (vextend env' y (RVSucc x))
              end
            | _ => raise RuntimeError "Type-error: non-numeral discriminee in ifz"
          )
        | ELetFun(f, _, k, _, y, e, K) =>
          eval_exp K (vextend env f (RVLam(env, k, y, e)))
        | EAppFun(f, k, x) =>
          (case vlookup env f of
               RVLam(env',j,y,K) =>
               eval_exp K (kextend (vextend env y (vlookup env x)) j (klookup env k))
            | _ => raise RuntimeError "Type-error: non-function in application position"
          )



  and eval_value (v : value) (env : env) : runtimevalue =
      case v of
          VZero =>
          RVZero
        | VSucc x =>
          RVSucc (vlookup env x)
        | VLam(_, k, _, x, e) =>
          RVLam(env, k, x, e)

  fun eval (e : exp) : runtimevalue =
      eval_exp e emptyenv
end
