open Tiger

(* Test if-then-else *)
let test_if_then_else () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let if_else_exp = Ast.IfExp {
    test = Ast.IntExp 1;
    then_ = Ast.IntExp 2;
    else_ = Some (Ast.IntExp 3);
    pos = pos
  } in
  let result = Semant.transExp (venv, tenv, if_else_exp) in
  let is_int = match Semant.actual_ty result.ty with Types.INT -> true | _ -> false in
  Alcotest.(check bool "If-then-else returns correct type" is_int true)

(* Test if-then (must return UNIT) *)
let test_if_then_unit () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let if_then_exp = Ast.IfExp {
    test = Ast.IntExp 1;
    then_ = Ast.BreakExp pos;
    else_ = None;
    pos = pos
  } in
  let result = Semant.transExp (venv, tenv, if_then_exp) in
  let is_unit = match Semant.actual_ty result.ty with Types.UNIT -> true | _ -> false in
  Alcotest.(check bool "If-then returns UNIT type" is_unit true)

(* Test if-then with non-UNIT body (should fail) *)
let test_if_then_non_unit () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let bad_if_then = Ast.IfExp {
    test = Ast.IntExp 1;
    then_ = Ast.IntExp 42;  (* Returns INT, not UNIT! *)
    else_ = None;
    pos = pos
  } in
  try
    let _ = Semant.transExp (venv, tenv, bad_if_then) in
    Alcotest.fail "Should raise error for if-then with non-UNIT body"
  with
  | Semant.Semantic_error _ -> Alcotest.(check bool "If-then with non-UNIT body raises error" true true)
  | _ -> Alcotest.fail "Wrong exception type"

(* Test if with mismatched branches *)
let test_mismatched_branches () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let bad_if_exp = Ast.IfExp {
    test = Ast.IntExp 1;
    then_ = Ast.IntExp 2;
    else_ = Some (Ast.StringExp ("bad", pos));
    pos = pos
  } in
  try
    let _ = Semant.transExp (venv, tenv, bad_if_exp) in
    Alcotest.fail "Should raise error for mismatched branches"
  with
  | Semant.Semantic_error _ -> Alcotest.(check bool "Mismatched branches raise error" true true)
  | _ -> Alcotest.fail "Wrong exception type"

let tests = [
  Alcotest.test_case "If-then-else" `Quick test_if_then_else;
  Alcotest.test_case "If-then returns UNIT" `Quick test_if_then_unit;
  Alcotest.test_case "If-then with non-UNIT body" `Quick test_if_then_non_unit;
  Alcotest.test_case "Mismatched branches" `Quick test_mismatched_branches;
]

