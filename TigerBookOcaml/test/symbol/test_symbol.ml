open Tiger

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

let tests = [
  Alcotest.test_case "Symbol Creation" `Quick test_symbol_creation;
  Alcotest.test_case "Symbol Name" `Quick test_symbol_name;
]

