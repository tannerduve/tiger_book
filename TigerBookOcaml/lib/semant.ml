module type SEMANT = sig
  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table
  type expty = { exp : Translate.exp; ty : Types.ty }
  type envs  = { venv : venv; tenv : tenv }

  val transVar  : venv * tenv * Ast.var -> expty
  val transExp  : venv * tenv * Ast.exp -> expty
  val transDec  : venv * tenv * Ast.dec -> envs
  val transTy   : tenv * Ast.ty -> Types.ty
  val actual_ty : Types.ty -> Types.ty
end

module Semant : SEMANT = struct
  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table
  type expty = { exp : Translate.exp; ty : Types.ty }
  type envs  = { venv : venv; tenv : tenv }

  exception Semantic_error of Ast.pos * string
  let error (p : Ast.pos) msg = raise (Semantic_error (p, msg))

  let rec actual_ty (t : Types.ty) =
    match t with
    | Types.NAME (_, { contents = Some ty }) -> actual_ty ty
    | _ -> t

  let lookupSym (s, p, v : Symbol.symbol * Ast.pos * venv) =
    match Symbol.look s v with
    | Some (Env.VarEntry ty) -> { exp = (); ty = actual_ty ty }
    | Some (Env.FunEntry _)  -> error p ("function used as variable: " ^ Symbol.name s)
    | None                   -> error p ("undefined variable: " ^ Symbol.name s)

  let rec symInList (fields : (Symbol.symbol * Types.ty) list)
                    (name   : Symbol.symbol)
                    (pos    : Ast.pos) : Types.ty =
    match fields with
    | [] -> error pos ("undefined field: " ^ Symbol.name name)
    | (field_sym, field_ty) :: rest ->
        if field_sym = name then actual_ty field_ty
        else symInList rest name pos

  let rec transVar (v_env, t_env, var : venv * tenv * Ast.var) : expty =
    match var with
    | Ast.SimpleVar (s, p) ->
        lookupSym (s, p, v_env)

    | Ast.FieldVar (vr, id, pos) ->
        let { ty = base_ty; _ } = transVar (v_env, t_env, vr) in
        (match actual_ty base_ty with
         | Types.RECORD (fields, _) -> { exp = (); ty = symInList fields id pos }
         | _ -> error pos "record required")

    | Ast.SubscriptVar (vr, e, pos) ->
        let { ty = arr_ty; _ } = transVar (v_env, t_env, vr) in
        (match actual_ty arr_ty with
         | Types.ARRAY (elem_ty, _) ->
             let { ty = idx_ty; _ } = transExp (v_env, t_env, e) in
             (match actual_ty idx_ty with
              | Types.INT -> { exp = (); ty = actual_ty elem_ty }
              | _ -> error pos "integer subscript required")
         | _ -> error pos "array required")

  and transExp (v_env, t_env, e : venv * tenv * Ast.exp) : expty =
    failwith "todo"

  and transDec (v_env, t_env, d : venv * tenv * Ast.dec) : envs =
    failwith "todo"

  let transTy (t_env, ty_ast : tenv * Ast.ty) : Types.ty =
    failwith "todo"
end
