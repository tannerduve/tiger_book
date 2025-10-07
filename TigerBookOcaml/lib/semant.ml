module type SEMANT = sig
  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table
  type expty = { exp : Translate.exp; ty : Types.ty }
  type envs = { venv : venv; tenv : tenv }

  val transVar : venv * tenv * Ast.var -> expty
  val transExp : venv * tenv * Ast.exp -> expty
  val transDec : venv * tenv * Ast.dec -> envs
  val transTy  : tenv * Ast.ty -> expty
end

module Semant : SEMANT = struct

  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table
  type expty = { exp : Translate.exp; ty : Types.ty }
  type envs  = { venv : venv; tenv : tenv }

  exception Semantic_error of Ast.pos * string
  let error (p : Ast.pos) (msg : string) = raise (Semantic_error (p, msg))

  let transVar (v, t, x : venv * tenv * Ast.var) : expty =
    match x with 
    | SimpleVar(s, p) ->
       begin match Symbol.look s v with 
        | Some(VarEntry(ty)) -> {exp = (); ty}
        | None -> error p ("undefined variable " ^ Symbol.name s)
        | _ -> error p ("invalid argument")
        end
    | FieldVar(vr, id, pos) -> error pos ("undefined variable " ^ Symbol.name id)
    | SubscriptVar(vr, e, pos) -> error pos ("undefined variable ")

  let transExp (v, t, e : venv * tenv * Ast.exp) : expty =
    failwith "todo"

  let transDec (v, t, d : venv * tenv * Ast.dec) : envs =
    failwith "todo"

  let transTy (t, ty : tenv * Ast.ty) : expty =
    failwith "todo"
end