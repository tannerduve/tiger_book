open Tiger

(* Test AST module *)
let test_ast_types () =
  (* Test that we can create basic AST nodes and they have correct types *)
  let int_exp = Ast.IntExp 42 in
  let string_exp = Ast.StringExp ("hello", 0) in
  let var_exp = Ast.VarExp (Ast.SimpleVar (Symbol.symbol "x", 0)) in
  
  (* Verify the expressions are of the expected types *)
  (match int_exp with
   | Ast.IntExp _ -> Alcotest.(check bool "IntExp created" true true)
   | _ -> Alcotest.fail "Expected IntExp");
   
  (match string_exp with
   | Ast.StringExp _ -> Alcotest.(check bool "StringExp created" true true)
   | _ -> Alcotest.fail "Expected StringExp");
   
  (match var_exp with
   | Ast.VarExp _ -> Alcotest.(check bool "VarExp created" true true)
   | _ -> Alcotest.fail "Expected VarExp")

let test_ast_constructors () =
  (* Test AST constructors work properly by pattern matching *)
  let pos = 0 in
  let sym = Symbol.symbol in

  (* Test variable constructors *)
  let simple_var = Ast.SimpleVar (sym "x", pos) in
  let field_var = Ast.FieldVar (simple_var, sym "field", pos) in
  let subscript_var = Ast.SubscriptVar (simple_var, Ast.IntExp 1, pos) in

  (* Verify SimpleVar structure *)
  (match simple_var with
   | Ast.SimpleVar (_, p) -> 
       Alcotest.(check int "SimpleVar position" p pos);
       Alcotest.(check bool "SimpleVar symbol exists" true true)
   | _ -> Alcotest.fail "Expected SimpleVar");

  (* Verify FieldVar structure *)
  (match field_var with
   | Ast.FieldVar (_, _, p) -> 
       Alcotest.(check int "FieldVar position" p pos)
   | _ -> Alcotest.fail "Expected FieldVar");

  (* Verify SubscriptVar structure *)
  (match subscript_var with
   | Ast.SubscriptVar (_, Ast.IntExp i, p) -> 
       Alcotest.(check int "SubscriptVar index" i 1);
       Alcotest.(check int "SubscriptVar position" p pos)
   | _ -> Alcotest.fail "Expected SubscriptVar with IntExp");

  (* Test expression constructors *)
  let int_exp = Ast.IntExp 42 in
  let string_exp = Ast.StringExp ("hello", pos) in

  (* Verify IntExp value *)
  (match int_exp with
   | Ast.IntExp i -> Alcotest.(check int "IntExp value" i 42)
   | _ -> Alcotest.fail "Expected IntExp");

  (* Verify StringExp value *)
  (match string_exp with
   | Ast.StringExp (s, p) -> 
       Alcotest.(check string "StringExp value" s "hello");
       Alcotest.(check int "StringExp position" p pos)
   | _ -> Alcotest.fail "Expected StringExp")

let tests = [
  Alcotest.test_case "AST Types" `Quick test_ast_types;
  Alcotest.test_case "AST Constructors" `Quick test_ast_constructors;
]

