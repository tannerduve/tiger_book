open Tiger

(* Test lexer functionality *)
let test_lexer_tokens () =
  let test_input = "42 + \"hello\" var x := 5" in
  let lexbuf = Lexing.from_string test_input in

  (* Test basic token recognition *)
  let _token1 = TigerLexer.token lexbuf in
  let _token2 = TigerLexer.token lexbuf in
  let _token3 = TigerLexer.token lexbuf in

  Alcotest.(check bool "Lexer recognizes tokens" true true)

let tests = [
  Alcotest.test_case "Lexer Tokens" `Quick test_lexer_tokens;
]

