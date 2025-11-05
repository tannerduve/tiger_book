(* Main test runner for all Tiger compiler tests *)

let () =
  let open Alcotest in
  
  run "Tiger Compiler Tests" [
    ("AST", Test_ast.tests);
    ("Symbol", Test_symbol.tests);
    ("Types", Test_types.tests);
    ("Lexer", Test_lexer.tests);
    ("Parser", Test_parser.tests);
    ("Environment", Test_env.tests);
    ("Semantic Analysis", Test_literals.tests @ Test_variables.tests @ Test_operators.tests @ Test_functions.tests @ Test_control_flow.tests);
  ]

