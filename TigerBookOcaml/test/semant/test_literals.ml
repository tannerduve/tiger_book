open Tiger

(* Test basic literal expressions *)
let test_int_literal () =
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let int_exp = Ast.IntExp 42 in
  let result = Semant.transExp (venv, tenv, int_exp) in
  let is_int = match Semant.actual_ty result.ty with Types.INT -> true | _ -> false in
  Alcotest.(check bool "IntExp returns INT type" is_int true)

let test_string_literal () =
  let pos = 0 in
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let string_exp = Ast.StringExp ("hello", pos) in
  let result = Semant.transExp (venv, tenv, string_exp) in
  let is_string = match Semant.actual_ty result.ty with Types.STRING -> true | _ -> false in
  Alcotest.(check bool "StringExp returns STRING type" is_string true)

let test_nil_literal () =
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  let nil_exp = Ast.NilExp in
  let result = Semant.transExp (venv, tenv, nil_exp) in
  let is_nil = match Semant.actual_ty result.ty with Types.NIL -> true | _ -> false in
  Alcotest.(check bool "NilExp returns NIL type" is_nil true)

let tests = [
  Alcotest.test_case "Int literal" `Quick test_int_literal;
  Alcotest.test_case "String literal" `Quick test_string_literal;
  Alcotest.test_case "Nil literal" `Quick test_nil_literal;
]

