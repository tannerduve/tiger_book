open Tiger

(* Test variable lookup *)
let test_simple_var_lookup () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  (* Add a variable to the environment *)
  let x_sym = sym "x" in
  let venv_with_x = Symbol.add x_sym (Env.VarEntry Types.INT) venv in
  
  (* Test SimpleVar lookup *)
  let var_exp = Ast.VarExp (Ast.SimpleVar (x_sym, pos)) in
  let result = Semant.transExp (venv_with_x, tenv, var_exp) in
  let is_int = match Semant.actual_ty result.ty with Types.INT -> true | _ -> false in
  Alcotest.(check bool "SimpleVar lookup returns correct type" is_int true)

let test_undefined_variable () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let x_sym = sym "x" in
  let venv_with_x = Symbol.add x_sym (Env.VarEntry Types.INT) venv in
  
  (* Test undefined variable *)
  let undefined_var = Ast.VarExp (Ast.SimpleVar (sym "undefined", pos)) in
  try
    let _ = Semant.transExp (venv_with_x, tenv, undefined_var) in
    Alcotest.fail "Should raise error for undefined variable"
  with
  | Semant.Semantic_error _ -> Alcotest.(check bool "Undefined variable raises error" true true)
  | _ -> Alcotest.fail "Wrong exception type"

let test_field_access () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let unique = ref () in
  let field_sym = sym "field" in
  let record_type = Types.RECORD ([(field_sym, Types.INT)], unique) in
  let record_var = Ast.SimpleVar (sym "rec", pos) in
  let venv_with_rec = Symbol.add (sym "rec") (Env.VarEntry record_type) venv in
  
  let field_var = Ast.FieldVar (record_var, field_sym, pos) in
  let result = Semant.transVar (venv_with_rec, tenv, field_var) in
  let is_int = match Semant.actual_ty result.ty with Types.INT -> true | _ -> false in
  Alcotest.(check bool "Field access returns correct type" is_int true)

let test_wrong_field_name () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let unique = ref () in
  let field_sym = sym "field" in
  let record_type = Types.RECORD ([(field_sym, Types.INT)], unique) in
  let record_var = Ast.SimpleVar (sym "rec", pos) in
  let venv_with_rec = Symbol.add (sym "rec") (Env.VarEntry record_type) venv in
  
  let wrong_field = Ast.FieldVar (record_var, sym "nonexistent", pos) in
  try
    let _ = Semant.transVar (venv_with_rec, tenv, wrong_field) in
    Alcotest.fail "Should raise error for undefined field"
  with
  | Semant.Semantic_error _ -> Alcotest.(check bool "Wrong field name raises error" true true)
  | _ -> Alcotest.fail "Wrong exception type"

let test_array_subscript () =
  let pos = 0 in
  let sym = Symbol.symbol in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let unique = ref () in
  let array_type = Types.ARRAY (Types.STRING, unique) in
  let array_var = Ast.SimpleVar (sym "arr", pos) in
  let venv_with_arr = Symbol.add (sym "arr") (Env.VarEntry array_type) venv in
  
  let subscript_var = Ast.SubscriptVar (array_var, Ast.IntExp 0, pos) in
  let result = Semant.transVar (venv_with_arr, tenv, subscript_var) in
  let is_string = match Semant.actual_ty result.ty with Types.STRING -> true | _ -> false in
  Alcotest.(check bool "Array subscript returns element type" is_string true)

let tests = [
  Alcotest.test_case "Simple variable lookup" `Quick test_simple_var_lookup;
  Alcotest.test_case "Undefined variable" `Quick test_undefined_variable;
  Alcotest.test_case "Field access" `Quick test_field_access;
  Alcotest.test_case "Wrong field name" `Quick test_wrong_field_name;
  Alcotest.test_case "Array subscript" `Quick test_array_subscript;
]

