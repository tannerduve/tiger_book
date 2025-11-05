open Tiger

(* Test arithmetic operations *)
let test_arithmetic () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let add_exp = Ast.OpExp {
    left = Ast.IntExp 1;
    oper = Ast.PlusOp;
    right = Ast.IntExp 2;
    pos = pos
  } in
  let result = Semant.transExp (venv, tenv, add_exp) in
  let is_int = match Semant.actual_ty result.ty with Types.INT -> true | _ -> false in
  Alcotest.(check bool "Arithmetic operation returns INT" is_int true)

(* Test comparison operations *)
let test_comparison () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let lt_exp = Ast.OpExp {
    left = Ast.IntExp 1;
    oper = Ast.LtOp;
    right = Ast.IntExp 2;
    pos = pos
  } in
  let result = Semant.transExp (venv, tenv, lt_exp) in
  let is_int = match Semant.actual_ty result.ty with Types.INT -> true | _ -> false in
  Alcotest.(check bool "Comparison returns INT" is_int true)

(* Test type mismatch *)
let test_type_mismatch () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let bad_arith_exp = Ast.OpExp {
    left = Ast.IntExp 1;
    oper = Ast.PlusOp;
    right = Ast.StringExp ("bad", pos);
    pos = pos
  } in
  try
    let _ = Semant.transExp (venv, tenv, bad_arith_exp) in
    Alcotest.fail "Should raise error for type mismatch"
  with
  | Semant.Semantic_error _ -> Alcotest.(check bool "Type mismatch raises error" true true)
  | _ -> Alcotest.fail "Wrong exception type"

let tests = [
  Alcotest.test_case "Arithmetic operations" `Quick test_arithmetic;
  Alcotest.test_case "Comparison operations" `Quick test_comparison;
  Alcotest.test_case "Type mismatch" `Quick test_type_mismatch;
]

