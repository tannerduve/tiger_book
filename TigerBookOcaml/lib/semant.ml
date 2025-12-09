(* Semantic analysis (type checking) for Tiger language *)
(*
 * This module performs type checking on Tiger ASTs. It maintains two environments:
 * - venv: variable/function environment
 * - tenv: type environment
 *)

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table
type expty = { exp : Translate.exp; ty : Types.ty }
type envs  = { venv : venv; tenv : tenv }

exception Semantic_error of Ast.pos * string
let error (p : Ast.pos) msg = raise (Semantic_error (p, msg))

(* Helper function to format type names for error messages *)
let rec type_to_string (ty : Types.ty) : string =
  match ty with
  | Types.INT -> "int"
  | Types.STRING -> "string"
  | Types.UNIT -> "unit"
  | Types.NIL -> "nil"
  | Types.RECORD (_, _) -> "record"
  | Types.ARRAY (elem_ty, _) -> "array of " ^ type_to_string elem_ty
  | Types.NAME (sym, _) -> Symbol.name sym

(* Follow NAME type references to get the actual type *)
let rec actual_ty (t : Types.ty) =
  match t with
  | Types.NAME (_, { contents = Some ty }) -> actual_ty ty
  | _ -> t

(* Create an expty from a type (for expressions that don't need translation) *)
let exptyFromType (typ : Types.ty) = { exp = (); ty = actual_ty typ}

let checkInt (e, pos : expty * Ast.pos) = 
  match actual_ty e.ty with 
  | INT -> ()
  | t -> error pos ("type error: expected int, but found " ^ type_to_string t)

(* Check if two types are compatible (can be used together) *)
let compatible (t1, t2 : Types.ty * Types.ty) : bool = 
  match (actual_ty t1, actual_ty t2) with 
  | (INT, INT) -> true 
  | (STRING, STRING) -> true 
  | (UNIT, UNIT) -> true
  | (RECORD(_, u1), RECORD(_, u2)) -> u1 == u2  (* Same unique record type *)
  | (ARRAY(_, u1), ARRAY(_, u2)) -> u1 == u2    (* Same unique array type *)
  | (NIL, RECORD _) -> true                     (* nil can be assigned to records *)
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
  | None -> error p ("undefined type: " ^ Symbol.name s)

(* Add function parameters to the variable environment *)
let addParamstoEnv (v_env : venv) (t_env : tenv) (fields : Ast.field list) =
  List.fold_left ( fun acc (fd : Ast.field) ->
    let fdTy = lookupType(fd.typ, fd.pos, t_env) in
    acc |> Symbol.add fd.name (Env.VarEntry(fdTy))
  ) v_env fields

(* Convert field list to type list *)
let fieldsToTy (params, tenv : Ast.field list * tenv) = 
  List.map (fun (f : Ast.field) -> lookupType (f.typ, f.pos, tenv)) params

(* Look up a field name in a record field list *)
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
       | t -> error pos ("type error: field access requires record, but found " ^ type_to_string t))

  | Ast.SubscriptVar (vr, e, pos) ->
      let { ty = arr_ty; _ } = transVar (v_env, t_env, vr) in
      (match actual_ty arr_ty with
       | Types.ARRAY (elem_ty, _) ->
           let { ty = idx_ty; _ } = transExp (v_env, t_env, e) in
           (match actual_ty idx_ty with
            | Types.INT -> exptyFromType elem_ty
            | t -> error pos ("type error: array subscript requires int, but found " ^ type_to_string t))
       | t -> error pos ("type error: subscript requires array, but found " ^ type_to_string t))

and transExp (v_env, t_env, e : venv * tenv * Ast.exp) : expty =
  match e with 
  | VarExp(v) -> transVar(v_env, t_env, v)
  | NilExp -> exptyFromType NIL
  | IntExp(_) -> exptyFromType INT
  | StringExp(_, _) -> exptyFromType STRING
  | CallExp({func; args; pos}) -> 
    begin match Symbol.look func v_env with 
    | Some FunEntry({formals; result}) ->
      let num_formals = List.length formals in
      let num_args = List.length args in
      if num_formals = num_args then 
        let argTypes = List.map (fun e -> (transExp (v_env, t_env, e)).ty) args in 
        let combined = List.combine argTypes formals in
        let rec check_args idx = function
          | [] -> exptyFromType result
          | (arg_ty, formal_ty) :: rest ->
              if compatible (arg_ty, formal_ty) then check_args (idx + 1) rest
              else error pos (Printf.sprintf "type error in argument %d: expected %s, but found %s" 
                                idx (type_to_string formal_ty) (type_to_string arg_ty))
        in check_args 1 combined
      else error pos (Printf.sprintf "arity mismatch: function %s expects %d argument(s), but %d provided" 
                        (Symbol.name func) num_formals num_args)
    | Some (VarEntry _ty) -> error pos (Printf.sprintf "type error: %s is a variable, not a function" (Symbol.name func))
    | None -> error pos (Printf.sprintf "undefined function: %s" (Symbol.name func))
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
          else error pos (Printf.sprintf "type error: cannot compare %s and %s for equality" 
                           (type_to_string lt') (type_to_string rt'))
      | LtOp | LeOp | GtOp | GeOp ->
          (match lt', rt' with
          | Types.INT, Types.INT | Types.STRING, Types.STRING -> exptyFromType Types.INT
          | _ -> error pos (Printf.sprintf "type error: ordering comparison requires int or string, but found %s and %s" 
                             (type_to_string lt') (type_to_string rt')))
      end
    | RecordExp({fields; typ; pos}) -> 
      let record_ty = actual_ty (lookupType(typ, pos, t_env)) in
      (match record_ty with
       | Types.RECORD (type_fields, unique) ->
           (* Check that all fields are provided and types match *)
           let rec check_fields provided_fields expected_fields =
             match expected_fields with
             | [] -> 
                 if provided_fields <> [] then
                   error pos (Printf.sprintf "type error: extra fields provided in record initialization")
                 else ()
             | (expected_name, expected_ty) :: rest ->
                 match List.find_opt (fun (name, _, _) -> name = expected_name) provided_fields with
                 | Some (_, exp, _) ->
                     let exp_ty = actual_ty (transExp (v_env, t_env, exp)).ty in
                     if compatible (exp_ty, expected_ty) then
                       let remaining = List.filter (fun (name, _, _) -> name <> expected_name) provided_fields in
                       check_fields remaining rest
                     else
                       error pos (Printf.sprintf "type error: field %s expects %s, but found %s"
                                   (Symbol.name expected_name) (type_to_string expected_ty) (type_to_string exp_ty))
                 | None ->
                     error pos (Printf.sprintf "type error: missing field %s in record initialization"
                                 (Symbol.name expected_name))
           in
           check_fields fields type_fields;
           exptyFromType (Types.RECORD (type_fields, unique))
       | t -> error pos (Printf.sprintf "type error: record initialization requires record type, but found %s"
                           (type_to_string t)))
    | SeqExp (es) ->
      (* Sequence: evaluate all expressions, return type of last one *)
      let rec go = function
      | []      -> exptyFromType NIL
      | [e,_]   -> transExp (v_env, t_env, e)
      | (e,_)::tl -> ignore (transExp (v_env, t_env, e)); go tl
      in go es
    | AssignExp({var; exp; pos}) ->
      let varTy = actual_ty (transVar (v_env, t_env, var)).ty in
      let expTy = actual_ty (transExp (v_env, t_env, exp)).ty in
      begin match varTy, expTy with 
      | _, UNIT -> error pos "type error: cannot assign unit type to variable"
      | UNIT, _ -> error pos "type error: cannot assign to unit-typed location"
      | _ -> 
        if compatible (varTy, expTy) 
          then exptyFromType UNIT 
            else error pos (Printf.sprintf "type error: cannot assign %s to variable of type %s" 
                             (type_to_string expTy) (type_to_string varTy))
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
          if compatible (thenTy, elseTy) then 
            exptyFromType thenTy else 
              error pos (Printf.sprintf "type error: if branches must have same type, but then has %s and else has %s" 
                          (type_to_string thenTy) (type_to_string elseTy))
        | None -> 
          if thenTy = UNIT then 
            exptyFromType thenTy else 
              error pos (Printf.sprintf "type error: if-then without else must have unit type, but found %s" 
                          (type_to_string thenTy))
        end
      | t -> error pos (Printf.sprintf "type error: if condition requires int, but found %s" 
                         (type_to_string t))
      end
    | LetExp({decs; body; pos=_}) -> 
      (* Process declarations in order, accumulating environments *)
      let { venv = final_venv; tenv = final_tenv } = 
        List.fold_left (fun envs dec -> transDec (envs.venv, envs.tenv, dec)) 
                       { venv = v_env; tenv = t_env } 
                       decs
      in
      (* Typecheck body with final environments *)
      transExp (final_venv, final_tenv, body)
    | BreakExp(_) -> exptyFromType UNIT
    | WhileExp({ test; body; pos }) -> 
      let testTy = actual_ty (transExp (v_env, t_env, test)).ty in 
      let bodTy = actual_ty (transExp (v_env, t_env, body)).ty in 
      begin match testTy with 
      | INT -> 
        begin match bodTy with 
        | UNIT -> exptyFromType bodTy 
        | t -> error pos (Printf.sprintf "type error: while loop body must have unit type, but found %s" 
                           (type_to_string t))
        end
      | t -> error pos (Printf.sprintf "type error: while condition requires int, but found %s" 
                         (type_to_string t))
      end
    | ForExp({var; escape=_; lo; hi; body; pos}) -> 
      (* Check that bounds are integers *)
      let lo_ty = actual_ty (transExp (v_env, t_env, lo)).ty in
      let hi_ty = actual_ty (transExp (v_env, t_env, hi)).ty in
      (match lo_ty, hi_ty with
       | Types.INT, Types.INT ->
           (* Add loop variable to environment as int *)
           let loop_venv = v_env |> Symbol.add var (Env.VarEntry Types.INT) in
           (* Typecheck body *)
           let body_ty = actual_ty (transExp (loop_venv, t_env, body)).ty in
           (match body_ty with
            | Types.UNIT -> exptyFromType Types.UNIT
            | t -> error pos (Printf.sprintf "type error: for loop body must have unit type, but found %s"
                              (type_to_string t)))
       | _, _ -> 
           error pos (Printf.sprintf "type error: for loop bounds must be int, but found %s and %s"
                       (type_to_string lo_ty) (type_to_string hi_ty)))
    | ArrayExp ( {typ; size; init; pos} ) -> 
      checkInt (transExp(v_env, t_env, size), pos);
      let tTy = lookupType(typ, pos, t_env) in
      let initTy = (transExp(v_env, t_env, init)).ty in
      if tTy == initTy then 
        exptyFromType (ARRAY(tTy, (ref ()))) 
      else 
        error pos (Printf.sprintf "type error: array initialization requires %s, but found %s" 
                    (type_to_string tTy) (type_to_string initTy))
  and transDec (v_env, t_env, d : venv * tenv * Ast.dec) : envs = 
    match d with 
    | FunctionDec l -> 
      (* For mutual recursion: create a local environment of all functions in the declaration with their declared types *)
      let fun_env = List.fold_left 
      (fun acc (fndec : Ast.fundec) ->
        acc |> (Symbol.add fndec.name (Env.FunEntry {formals = fieldsToTy(fndec.params, t_env); result = 
        begin match fndec.result with 
        | Some (s, p) -> lookupType(s, p, t_env) 
        | None -> actual_ty (transExp (v_env, t_env, fndec.body)).ty
        end
        }))
        ) v_env l
      in
      (* Helper: *)
      let funHelper (v_env : venv) (fd : Ast.fundec) : venv =
        (* Create local env with paramters mapped to their declared types (allows recursion) *)
        let local_env = addParamstoEnv v_env t_env fd.params in
        (* Typecheck the body of the function *)
        let fdty = actual_ty (transExp (local_env, t_env, fd.body)).ty in
        (* Construct an entry - look up the types of the params and let the result be the body type *)
        let fdentry = (Env.FunEntry { formals = fieldsToTy(fd.params, t_env); result = fdty }) in
        begin match fd.result with 
        | Some (s, p) -> 
          let expected_ty = lookupType(s, p, t_env) in
          if fdty = expected_ty then 
            v_env |> (Symbol.add fd.name fdentry) 
          else 
            error p (Printf.sprintf "type error: function %s body has type %s, but declared return type is %s" 
                      (Symbol.name fd.name) (type_to_string fdty) (type_to_string expected_ty))
        | None -> v_env |> (Symbol.add fd.name fdentry) 
      end
      in 
      let new_v_env = List.fold_left funHelper fun_env l in
      { venv = new_v_env; tenv = t_env }
    | VarDec ( {name; escape=_; typ; init; pos} ) -> 
      let bodty = actual_ty (transExp (v_env, t_env, init)).ty in 
      let vname = Env.VarEntry (bodty) in 
      let new_v_env = begin match typ with 
      | Some (s, p) -> 
          let expected_ty = lookupType(s, p, t_env) in
          if bodty = expected_ty then 
            v_env |> (Symbol.add name vname) 
          else 
            error pos (Printf.sprintf "type error: variable %s declared as %s, but initialized with %s" 
                        (Symbol.name name) (type_to_string expected_ty) (type_to_string bodty))
      | None -> v_env |> (Symbol.add name vname)
      end in 
      { venv = new_v_env; tenv = t_env }
    | TypeDec l -> 
      (* First pass: create NAME types for all declared types (handles mutual recursion) *)
      let temp_env = 
        List.fold_left 
          (fun (curr_env : tenv) (td : Ast.typedecrec) -> 
            let name_ty = Types.NAME (td.name, ref None) in
            curr_env |> Symbol.add td.name name_ty)
          t_env l
      in
      (* Second pass: fill in the type refs by converting AST types *)
      let new_t_env = 
        List.fold_left 
          (fun (curr_env : tenv) (td : Ast.typedecrec) -> 
            match Symbol.look td.name curr_env with
            | Some (Types.NAME (_, ty_ref)) ->
                let actual_type = transTy (curr_env, td.ty) in
                ty_ref := Some actual_type;
                curr_env
            | _ -> error td.pos "internal error: type declaration not found")
          temp_env l
      in
      { venv = v_env; tenv = new_t_env }

(* Convert AST type to Types.ty *)
and transTy (t_env, ty_ast : tenv * Ast.ty) : Types.ty =
  match ty_ast with 
  | Ast.NameTy(s, p) -> 
      lookupType(s, p, t_env)
  | Ast.RecordTy fields -> 
      let field_list = List.map (fun (f : Ast.field) -> 
        (f.name, lookupType(f.typ, f.pos, t_env))
      ) fields in
      Types.RECORD (field_list, ref ())
  | Ast.ArrayTy(s, p) -> 
      let elem_ty = lookupType(s, p, t_env) in
      Types.ARRAY (elem_ty, ref ())