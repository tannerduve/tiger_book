(* Test the parser with some simple expressions *)
let test_parse input =
  try
    let lexbuf = Lexing.from_string input in
    match Tiger.Parser.prog Tiger.TigerLexer.token lexbuf with
    | Some _expr -> 
        Printf.printf "✓ Parsed: %s\n" input
    | None -> 
        Printf.printf "✓ Empty program: %s\n" input
  with
  | Tiger.Parser.Error ->
      Printf.printf "✗ Parse error in: %s\n" input
  | Tiger.TigerLexer.SyntaxError msg ->
      Printf.printf "✗ Lexer error in '%s': %s\n" input msg
  | e ->
      Printf.printf "✗ Error parsing '%s': %s\n" input (Printexc.to_string e)

let () =
  print_endline "Testing Tiger parser...";
  print_endline "\n=== Basic expressions ===";
  test_parse "42";
  test_parse "\"hello world\"";
  test_parse "x + y";
  test_parse "x * (y + 2)";
  test_parse "nil";
  test_parse "break";
  
  print_endline "\n=== Variables and assignments ===";
  test_parse "x";
  test_parse "x := 5";
  test_parse "arr[i]";
  test_parse "record.field";
  test_parse "arr[i] := x";
  
  print_endline "\n=== Control structures ===";
  test_parse "if x then y else z";
  test_parse "if x then y";
  test_parse "while x do y";
  test_parse "for i := 1 to 10 do i";
  
  print_endline "\n=== Function calls ===";
  test_parse "f()";
  test_parse "f(x, y)";
  
  print_endline "\n=== Complex expressions ===";
  test_parse "(x; y; z)";
  test_parse "let var x := 5 in x + 1 end";
  test_parse "x < y";
  test_parse "x <> y";
  test_parse "x & y";
  test_parse "x | y";
  
  print_endline "\n=== Advanced constructs ===";
  test_parse "let type intArray = array of int in nil end";
  test_parse "let function f(x: int): int = x + 1 in f(5) end";
  test_parse "person{name=\"John\", age=25}";
  test_parse "intArray[10] of 0";
  test_parse "let var x: int := 5 in x end";
  
  print_endline "\n=== Nested expressions ===";
  test_parse "if x > 0 then (y := x; z := y) else break";
  test_parse "for i := 0 to n-1 do arr[i] := i * 2";
  test_parse "while i < n do (sum := sum + arr[i]; i := i + 1)"
