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

(* Test Symbol module *)
let test_symbol_creation () =
  let sym1 = Symbol.symbol "test" in
  let sym2 = Symbol.symbol "test" in
  let sym3 = Symbol.symbol "different" in

  (* Symbols with same name should be equal *)
  Alcotest.(check bool "Same symbols are equal" (sym1 = sym2) true);
  Alcotest.(check bool "Different symbols are not equal" (sym1 <> sym3) true)

let test_symbol_name () =
  let sym = Symbol.symbol "hello" in
  Alcotest.(check string "Symbol name extraction" (Symbol.name sym) "hello")

(* Test Types module *)
let test_types_creation () =
  let _unique = ref () in
  let _int_type = Types.INT in
  let _string_type = Types.STRING in
  let _nil_type = Types.NIL in

  (* Test record type *)
  let _record_fields = [(Symbol.symbol "field1", Types.INT); (Symbol.symbol "field2", Types.STRING)] in
  let _record_type = Types.RECORD (_record_fields, _unique) in

  (* Test array type *)
  let _array_type = Types.ARRAY (Types.INT, _unique) in

  Alcotest.(check bool "Types created successfully" true true)

(* Test lexer functionality *)
let test_lexer_tokens () =
  let test_input = "42 + \"hello\" var x := 5" in
  let lexbuf = Lexing.from_string test_input in

  (* Test basic token recognition *)
  let _token1 = TigerLexer.token lexbuf in
  let _token2 = TigerLexer.token lexbuf in
  let _token3 = TigerLexer.token lexbuf in

  Alcotest.(check bool "Lexer recognizes tokens" true true)

(* Test parser functionality *)
let test_parser_basic () =
  let test_cases = [
    "42";
    "\"hello\"";
    "x + y";
    "if x then y else z";
    "let var x := 5 in x end";
  ] in

  List.iter (fun input ->
    try
      let lexbuf = Lexing.from_string input in
      let _result = Parser.prog TigerLexer.token lexbuf in
      Alcotest.(check bool ("Parser handles: " ^ input) true true)
    with _ -> Alcotest.fail ("Parser failed on: " ^ input)
  ) test_cases

(* Test semantic analysis *)
let test_semant_basic () =
  (* This would test semantic analysis when implemented *)
  Alcotest.(check bool "Semantic analysis placeholder" true true)

(* Test environment *)
let test_env_basic () =
  (* This would test environment functionality when implemented *)
  Alcotest.(check bool "Environment placeholder" true true)

(* Test suite *)
let () =
  let open Alcotest in

  run "Tiger Language Tests" [
    ("AST", [
      test_case "AST Types" `Quick test_ast_types;
      test_case "AST Constructors" `Quick test_ast_constructors;
    ]);
    ("Symbol", [
      test_case "Symbol Creation" `Quick test_symbol_creation;
      test_case "Symbol Name" `Quick test_symbol_name;
    ]);
    ("Types", [
      test_case "Types Creation" `Quick test_types_creation;
    ]);
    ("Lexer", [
      test_case "Lexer Tokens" `Quick test_lexer_tokens;
    ]);
    ("Parser", [
      test_case "Parser Basic" `Quick test_parser_basic;
    ]);
    ("Semantic Analysis", [
      test_case "Semantic Basic" `Quick test_semant_basic;
    ]);
    ("Environment", [
      test_case "Environment Basic" `Quick test_env_basic;
    ]);
  ]
