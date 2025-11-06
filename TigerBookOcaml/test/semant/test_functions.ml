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

(* Test function with parameters *)
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
  
  (* Function should successfully typecheck with parameter in scope *)
  let result = Semant.transDec (venv, tenv, fun_with_params) in
  let new_venv = result.venv in
  
  (match Symbol.look (sym "g") new_venv with
   | Some (Env.FunEntry {formals; result=res_ty}) ->
       Alcotest.(check bool "Function has one parameter" (List.length formals = 1) true);
       let param_is_int = match List.hd formals with Types.INT -> true | _ -> false in
       Alcotest.(check bool "Parameter has INT type" param_is_int true);
       let result_is_int = match Semant.actual_ty res_ty with Types.INT -> true | _ -> false in
       Alcotest.(check bool "Result has INT type" result_is_int true)
   | _ -> Alcotest.fail "Function with params not found")

(* Test recursive function declaration *)
let test_recursive_function () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let param_field = {
    Ast.name = sym "n";
    escape = ref false;
    typ = Symbol.symbol "int";
    pos = pos
  } in
  
  (* Recursive factorial: function fact(n: int): int = if n = 0 then 1 else n * fact(n-1) *)
  let recursive_fun = Ast.FunctionDec [
    {
      name = sym "fact";
      params = [param_field];
      result = Some (Symbol.symbol "int", pos);
      body = Ast.IfExp {
        test = Ast.OpExp {
          left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
          oper = Ast.EqOp;
          right = Ast.IntExp 0;
          pos = pos
        };
        then_ = Ast.IntExp 1;
        else_ = Some (Ast.OpExp {
          left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
          oper = Ast.TimesOp;
          right = Ast.CallExp {
            func = sym "fact";
            args = [Ast.OpExp {
              left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
              oper = Ast.MinusOp;
              right = Ast.IntExp 1;
              pos = pos
            }];
            pos = pos
          };
          pos = pos
        });
        pos = pos
      };
      pos = pos
    }
  ] in
  
  (* Recursive function should typecheck - function can call itself *)
  let result = Semant.transDec (venv, tenv, recursive_fun) in
  let new_venv = result.venv in
  
  (match Symbol.look (sym "fact") new_venv with
   | Some (Env.FunEntry {formals; result=res_ty}) ->
       Alcotest.(check bool "Recursive function has one parameter" (List.length formals = 1) true);
       let result_is_int = match Semant.actual_ty res_ty with Types.INT -> true | _ -> false in
       Alcotest.(check bool "Recursive function returns INT" result_is_int true)
   | _ -> Alcotest.fail "Recursive function not found")

(* Test mutually recursive functions *)
let test_mutually_recursive_functions () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let param_field = {
    Ast.name = sym "n";
    escape = ref false;
    typ = Symbol.symbol "int";
    pos = pos
  } in
  
  (* Mutually recursive even/odd functions *)
  let mutual_funs = Ast.FunctionDec [
    {
      (* function even(n: int): int = if n = 0 then 1 else odd(n-1) *)
      name = sym "even";
      params = [param_field];
      result = Some (Symbol.symbol "int", pos);
      body = Ast.IfExp {
        test = Ast.OpExp {
          left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
          oper = Ast.EqOp;
          right = Ast.IntExp 0;
          pos = pos
        };
        then_ = Ast.IntExp 1;
        else_ = Some (Ast.CallExp {
          func = sym "odd";
          args = [Ast.OpExp {
            left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
            oper = Ast.MinusOp;
            right = Ast.IntExp 1;
            pos = pos
          }];
          pos = pos
        });
        pos = pos
      };
      pos = pos
    };
    {
      (* function odd(n: int): int = if n = 0 then 0 else even(n-1) *)
      name = sym "odd";
      params = [param_field];
      result = Some (Symbol.symbol "int", pos);
      body = Ast.IfExp {
        test = Ast.OpExp {
          left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
          oper = Ast.EqOp;
          right = Ast.IntExp 0;
          pos = pos
        };
        then_ = Ast.IntExp 0;
        else_ = Some (Ast.CallExp {
          func = sym "even";
          args = [Ast.OpExp {
            left = Ast.VarExp (Ast.SimpleVar (sym "n", pos));
            oper = Ast.MinusOp;
            right = Ast.IntExp 1;
            pos = pos
          }];
          pos = pos
        });
        pos = pos
      };
      pos = pos
    }
  ] in
  
  (* Mutually recursive functions should typecheck *)
  let result = Semant.transDec (venv, tenv, mutual_funs) in
  let new_venv = result.venv in
  
  (* Check both functions are in environment *)
  (match Symbol.look (sym "even") new_venv with
   | Some (Env.FunEntry _) -> Alcotest.(check bool "even function found" true true)
   | _ -> Alcotest.fail "even function not found");
  
  (match Symbol.look (sym "odd") new_venv with
   | Some (Env.FunEntry _) -> Alcotest.(check bool "odd function found" true true)
   | _ -> Alcotest.fail "odd function not found")

let tests = [
  Alcotest.test_case "Built-in function call" `Quick test_builtin_function;
  Alcotest.test_case "Function declaration" `Quick test_function_declaration;
  Alcotest.test_case "Function with params" `Quick test_function_with_params;
  Alcotest.test_case "Recursive function" `Quick test_recursive_function;
  Alcotest.test_case "Mutually recursive functions" `Quick test_mutually_recursive_functions;
]

