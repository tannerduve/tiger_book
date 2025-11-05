open Tiger

(* Test environment *)
let test_env_basic () =
  let venv = Env.base_venv in
  let tenv = Env.base_tenv in
  
  (* Verify base environment has expected types *)
  (match Symbol.look (Symbol.symbol "int") tenv with
   | Some Types.INT -> Alcotest.(check bool "int type in base tenv" true true)
   | _ -> Alcotest.fail "int type not found in base tenv");
  
  (match Symbol.look (Symbol.symbol "string") tenv with
   | Some Types.STRING -> Alcotest.(check bool "string type in base tenv" true true)
   | _ -> Alcotest.fail "string type not found in base tenv");
  
  (* Verify base environment has expected functions *)
  (match Symbol.look (Symbol.symbol "print") venv with
   | Some (Env.FunEntry _) -> Alcotest.(check bool "print function in base venv" true true)
   | _ -> Alcotest.fail "print function not found in base venv")

let tests = [
  Alcotest.test_case "Environment Basic" `Quick test_env_basic;
]

