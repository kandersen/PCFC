signature CPS_CHECK =
sig
    type tenv
    exception TypeError

    val check : ILCPS.exp -> unit
end

structure CPSCheck : CPS_CHECK =
struct
  open ILCPS

  type tenv = ty VarMap.map
  type kenv = ty KVarMap.map

  exception TypeError

  fun extend_var tenv x t = VarMap.insert(tenv,x,t)
  fun infer_var tenv kenv x = VarMap.lookup(tenv, x)
  fun check_var tenv kenv x t =
      let val true = t = VarMap.lookup(tenv, x) in
          ()
      end

  fun extend_cont kenv k t = KVarMap.insert(kenv,k,t)
  fun infer_cont tenv kenv k = KVarMap.lookup(kenv, k)
  fun check_cont tenv kenv k t =
      let val true = t = KVarMap.lookup(kenv, k) in
          ()
      end

  fun infer_exp tenv kenv e = TNat

  and check_val tenv kenv TNat VZero =
      ()
    | check_val tenv kenv TNat (VSucc x) =
      check_var tenv kenv x TNat

  and check_exp tenv kenv (EAppCont(k, x)) =
      let val tv = infer_var tenv kenv x in
          check_cont tenv kenv k tv
      end
    | check_exp tenv kenv (EAppFun(f, k, x)) =
      let val (TArr(t1,t2)) = infer_var tenv kenv f
          val () = check_cont tenv kenv k t2
          val () = check_var tenv kenv x t1
      in
          ()
      end
    | check_exp tenv kenv (ELetVal(ty, x, v, e)) =
      let val () = check_val tenv kenv ty v
          val tenv' = extend_var tenv x ty
      in
          check_exp tenv' kenv e
      end

    | check_exp tenv kenv (ELetFun(f, tk, k, tx, x, e1, e2)) =
      ()
    | check_exp _ _ _  =
      ()

  fun check e =
      check_exp VarMap.empty (KVarMap.singleton(halt, TNat)) e

end
