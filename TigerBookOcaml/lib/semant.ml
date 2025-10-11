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
  
  let exptyFromType (typ : Types.ty) = { exp = (); ty = actual_ty typ}

  let checkInt (e, pos : expty * Ast.pos) = 
    match actual_ty e.ty with 
    | INT -> ()
    | _ -> error pos ("integer expected")

  let checkIntOrString(e, pos : expty * Ast.pos) =
    match actual_ty e.ty with 
    | INT -> ()
    | STRING -> ()
    | _ -> error pos ("integer or string expected")

  let compatible (t1, t2 : Types.ty * Types.ty) : bool = 
    match (actual_ty t1, actual_ty t2) with 
    | (INT, INT) -> true 
    | (STRING, STRING) -> true 
    | (RECORD(_, u1), RECORD(_, u2)) -> u1 == u2 
    | (ARRAY(_, u1), ARRAY(_, u2)) -> u1 == u2
    | (NIL, RECORD _) -> true 
    | _ -> false

  let lookupSym (s, p, v : Symbol.symbol * Ast.pos * venv) =
    match Symbol.look s v with
    | Some (Env.VarEntry ty) -> exptyFromType ty
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
         | Types.RECORD (fields, _) -> exptyFromType (symInList fields id pos)
         | _ -> error pos "record required")

    | Ast.SubscriptVar (vr, e, pos) ->
        let { ty = arr_ty; _ } = transVar (v_env, t_env, vr) in
        (match actual_ty arr_ty with
         | Types.ARRAY (elem_ty, _) ->
             let { ty = idx_ty; _ } = transExp (v_env, t_env, e) in
             (match actual_ty idx_ty with
              | Types.INT -> exptyFromType elem_ty
              | _ -> error pos "integer subscript required")
         | _ -> error pos "array required")

  and transExp (v_env, t_env, e : venv * tenv * Ast.exp) : expty =
    match e with 
    | VarExp(v) -> transVar(v_env, t_env, v)
    | NilExp -> exptyFromType NIL
    | IntExp(_) -> exptyFromType INT
    | StringExp(_, _) -> exptyFromType STRING
    | CallExp({func; args; pos}) -> 
      begin match Symbol.look func v_env with 
      | Some FunEntry({formals; result}) ->
        if List.length formals == List.length args then 
          let argTypes = List.map (fun e -> (transExp (v_env, t_env, e)).ty) args in 
          if List.fold_left
          (fun ok (arg_ty, formal_ty) -> ok && compatible (arg_ty, formal_ty))
          true
          (List.combine argTypes formals) then exptyFromType result else error pos "incorrect argument types"
        else error pos "incorrect number of arguments"
      | _ -> error pos "function expected"  
      end
    | OpExp({left; oper; right; pos}) ->
      begin match oper with 
      | PlusOp ->
        checkInt ((transExp (v_env, t_env, left)), pos);
        checkInt ((transExp (v_env, t_env, right)), pos);
        exptyFromType INT
      | MinusOp ->
        checkInt ((transExp (v_env, t_env, left)), pos);
        checkInt ((transExp (v_env, t_env, right)), pos);
        exptyFromType INT
      | TimesOp -> 
        checkInt ((transExp (v_env, t_env, left)), pos);
        checkInt ((transExp (v_env, t_env, right)), pos);
        exptyFromType INT
      | DivideOp ->
        checkInt ((transExp (v_env, t_env, left)), pos);
        checkInt ((transExp (v_env, t_env, right)), pos);
        exptyFromType INT
      | _ -> 
        checkIntOrString ((transExp (v_env, t_env, left)), pos);
        checkIntOrString ((transExp (v_env, t_env, right)), pos);
        exptyFromType INT
      end
    | RecordExp({fields; typ; pos}) -> 
      failwith "todo"
    | SeqExp([]) -> exptyFromType NIL 
    | SeqExp([(exp, _)]) ->
      transExp(v_env, t_env, exp)
    | SeqExp(_ :: tl) ->
      transExp(v_env, t_env, SeqExp(tl))
    | AssignExp({var; exp; pos}) ->
      failwith "todo"
    | _ -> failwith "todo"

  and transDec (v_env, t_env, d : venv * tenv * Ast.dec) : envs =
    failwith "todo"

  let transTy (t_env, ty_ast : tenv * Ast.ty) : Types.ty =
    failwith "todo"
end
