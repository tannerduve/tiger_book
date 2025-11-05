(* Semantic analysis (type c) for Tiger language *)

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

let compatible (t1, t2 : Types.ty * Types.ty) : bool = 
  match (actual_ty t1, actual_ty t2) with 
  | (INT, INT) -> true 
  | (STRING, STRING) -> true 
  | (UNIT, UNIT) -> true
  | (RECORD(_, u1), RECORD(_, u2)) -> u1 == u2 
  | (ARRAY(_, u1), ARRAY(_, u2)) -> u1 == u2
  | (NIL, RECORD _) -> true 
  | (RECORD _, NIL) -> true
  | _ -> false

let lookupSym (s, p, v : Symbol.symbol * Ast.pos * venv) =
  match Symbol.look s v with
  | Some (Env.VarEntry ty) -> exptyFromType ty
  | Some (Env.FunEntry _)  -> error p ("function used as variable: " ^ Symbol.name s)
  | None                   -> error p ("undefined variable: " ^ Symbol.name s)

let lookupType (s, p, t : Symbol.symbol * Ast.pos * tenv) =
  match Symbol.look s t with 
  | Some t -> actual_ty t 
  | None -> error p ("undefined variable: " ^ Symbol.name s)

let fieldsToTy (params, tenv : Ast.field list * tenv) = 
  List.map (fun (f : Ast.field) -> lookupType (f.typ, f.pos, tenv)) params

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
      if List.length formals = List.length args then 
        let argTypes = List.map (fun e -> (transExp (v_env, t_env, e)).ty) args in 
        if List.fold_left
        (fun ok (arg_ty, formal_ty) -> ok && compatible (arg_ty, formal_ty))
        true
        (List.combine argTypes formals) then exptyFromType result else error pos "incorrect argument types"
      else error pos "incorrect number of arguments"
    | _ -> error pos "function expected"  
    end
    | OpExp {left; oper; right; pos} ->
      let lt  = transExp (v_env, t_env, left)  in
      let rt  = transExp (v_env, t_env, right) in
      let lt' = actual_ty lt.ty and rt' = actual_ty rt.ty in
      begin match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp ->
          checkInt (lt, pos); checkInt (rt, pos); exptyFromType Types.INT
      | EqOp | NeqOp ->
          if compatible (lt', rt') then exptyFromType Types.INT
          else error pos "incompatible types for equality"
      | LtOp | LeOp | GtOp | GeOp ->
          (match lt', rt' with
          | Types.INT, Types.INT | Types.STRING, Types.STRING -> exptyFromType Types.INT
          | _ -> error pos "ordering requires int or string")
      end
    | RecordExp({fields=_; typ=_; pos=_}) -> 
      failwith "todo"
    | SeqExp (es) ->
      let rec go = function
      | []      -> exptyFromType NIL
      | [e,_]   -> transExp (v_env, t_env, e)
      | (e,_)::tl -> ignore (transExp (v_env, t_env, e)); go tl
      in go es
    | AssignExp({var; exp; pos}) ->
      let varTy = actual_ty (transVar (v_env, t_env, var)).ty in
      let expTy = actual_ty (transExp (v_env, t_env, exp)).ty in
      begin match varTy, expTy with 
      | _, UNIT | UNIT, _ -> error pos "can not assign unit type"
      | _ -> 
        if compatible (varTy, expTy) 
          then exptyFromType UNIT 
            else error pos "variable and expression have incompatible types"
      end 
    | IfExp ({test; then_; else_; pos}) -> 
      let testTy = actual_ty (transExp (v_env, t_env, test)).ty in 
      let thenTy = actual_ty (transExp (v_env, t_env, then_)).ty in 
      begin match testTy with 
      (* Check INT because there is no boolean type *)
      | INT -> 
        begin match else_ with 
        | Some exp -> 
          let elseTy = actual_ty (transExp (v_env, t_env, exp)).ty in
          if compatible (thenTy, elseTy) then exptyFromType thenTy else 
          error pos "then and else branch must have same type"
        | None -> if thenTy = UNIT then exptyFromType thenTy else error pos "if-then expression must have type unit"
        end
      | _ -> error pos "integer required in `if` branch"
      end
    | LetExp({decs=_; body=_; pos=_}) -> failwith "todo"
    | BreakExp(_) -> exptyFromType UNIT
    | _ -> failwith "todo"
  and transDec (v_env, t_env, d : venv * tenv * Ast.dec) : envs = 
    match d with 
    | FunctionDec l -> 
      let funHelper (v_env : venv) (fd : Ast.fundec) : venv =
        let fdty = actual_ty (transExp (v_env, t_env, fd.body)).ty in
        let fdname = (Env.FunEntry { formals = fieldsToTy(fd.params, t_env); result = fdty }) in
        begin match fd.result with 
        | Some (s, p) -> if fdty = lookupType(s, p, t_env) then v_env |> (Symbol.add fd.name fdname) else error p "function body does not match expected type"
        | None -> v_env |> (Symbol.add fd.name fdname)
      end
      in 
      let new_v_env = List.fold_left funHelper v_env l in
      { venv = new_v_env; tenv = t_env }
    | VarDec ( {name; escape=_; typ; init; pos} ) -> 
      let bodty = actual_ty (transExp (v_env, t_env, init)).ty in 
      let vname = Env.VarEntry (bodty) in 
      let new_v_env = begin match typ with 
      | Some (s, p) -> if bodty = lookupType(s, p, t_env) then v_env |> (Symbol.add name vname) else error pos "expression does not match expected type"
      | None -> v_env |> (Symbol.add name vname)
      end in 
      { venv = new_v_env; tenv = t_env }
    | _ -> failwith ""
let transTy (_t_env, _ty_ast : tenv * Ast.ty) : Types.ty =
  failwith "todo"