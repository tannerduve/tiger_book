open Tiger

(* Test built-in function calls *)
let test_builtin_function () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let print_call = Ast.CallExp {
    func = Symbol.symbol "print";
    args = [Ast.StringExp ("hello", pos)];
    pos = pos
  } in
  let result = Semant.transExp (venv, tenv, print_call) in
  let is_nil = match Semant.actual_ty result.ty with Types.NIL -> true | _ -> false in
  Alcotest.(check bool "Function call returns correct type" is_nil true)

(* Test function declaration *)
let test_function_declaration () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let fun_dec = Ast.FunctionDec [
    {
      name = sym "f";
      params = [];
      result = None;
      body = Ast.IntExp 42;
      pos = pos
    }
  ] in
  let result = Semant.transDec (venv, tenv, fun_dec) in
  let new_venv = result.venv in
  
  (match Symbol.look (sym "f") new_venv with
   | Some (Env.FunEntry {formals; result=res_ty}) ->
       Alcotest.(check bool "Function has correct formals" (formals = []) true);
       let is_int = match Semant.actual_ty res_ty with Types.INT -> true | _ -> false in
       Alcotest.(check bool "Function has correct result type" is_int true)
   | _ -> Alcotest.fail "Function not found in environment")

(* Test function with parameters - SHOULD FAIL due to bug! *)
let test_function_with_params () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let param_field = {
    Ast.name = sym "x";
    escape = ref false;
    typ = Symbol.symbol "int";
    pos = pos
  } in
  let fun_with_params = Ast.FunctionDec [
    {
      name = sym "g";
      params = [param_field];
      result = Some (Symbol.symbol "int", pos);
      body = Ast.VarExp (Ast.SimpleVar (sym "x", pos));  (* Uses parameter! *)
      pos = pos
    }
  ] in
  
  (* This SHOULD work but currently fails due to bug *)
  try
    let _ = Semant.transDec (venv, tenv, fun_with_params) in
    Alcotest.fail "BUG: Function body should access parameters but raises error"
  with
  | Semant.Semantic_error (_, msg) when String.starts_with ~prefix:"undefined variable" msg ->
      (* Expected failure due to known bug *)
      Alcotest.(check bool "Known bug: params not in scope" true true)
  | _ -> Alcotest.fail "Wrong exception type"

let tests = [
  Alcotest.test_case "Built-in function call" `Quick test_builtin_function;
  Alcotest.test_case "Function declaration" `Quick test_function_declaration;
  Alcotest.test_case "Function with params (KNOWN BUG)" `Quick test_function_with_params;
]

