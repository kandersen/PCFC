structure ILCPS =
struct
  structure Variable = ILTAST.Variable
  structure VarMap = ILTAST.VarMap
  structure KVariable = Variable()
  structure KVarMap = VarMap(structure V = KVariable)

  type var = Variable.t
  type cont = KVariable.t

  val halt : cont = KVariable.fresh("halt")

  type ty = ILTAST.ty

  datatype value
    = VZero
    | VSucc of var
    | VLam of ty * cont * ty * var * exp

  and exp
    =  ELetVal of ty * var * value * exp
     | EIfz of var * cont * cont
     | ELetFun of var * ty * cont * ty * var * exp * exp
     | EAppFun of var * cont * var
     | ELetCont of cont * ty * var * exp * exp
     | EAppCont of cont * var

end
