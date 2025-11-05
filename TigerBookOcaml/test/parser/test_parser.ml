open Tiger

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

let tests = [
  Alcotest.test_case "Parser Basic" `Quick test_parser_basic;
]

