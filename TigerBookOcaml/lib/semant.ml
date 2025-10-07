
let transProg (exp: Ast.exp) : unit = 
    match exp with 
    | _ -> ()




module type Semant = sig 

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table
type expty = {exp: Translate.exp; ty: Types.ty}
type envs = { venv: venv; tenv: tenv }

val transVar : venv * tenv * Ast.var -> expty
val transExp : venv * tenv * Ast.exp -> expty
val transDec : venv * tenv * Ast.dec -> envs
val transTy : tenv * Ast.ty -> expty

end