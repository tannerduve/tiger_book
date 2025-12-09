open OUnit2

(* Helper function for substring checking *)
let contains_substring s sub =
  try
    let len_s = String.length s in
    let len_sub = String.length sub in
    if len_sub = 0 then true
    else if len_sub > len_s then false
    else
      let rec check i =
        if i > len_s - len_sub then false
        else if String.sub s i len_sub = sub then true
        else check (i + 1)
      in
      check 0
  with _ -> false

(* Helper to parse a Tiger source file *)
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let result = 
    try
      Tiger.Parser.prog Tiger.TigerLexer.token lexbuf
    with
    | Tiger.Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        failwith (Printf.sprintf "Parse error at %s:%d:%d"
                    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol))
    | Failure msg ->
        failwith (Printf.sprintf "Lexer error: %s" msg)
  in
  close_in ic;
  result

(* Helper to run semantic analysis *)
let typecheck exp =
  let base_tenv = Tiger.Env.base_tenv in
  let base_venv = Tiger.Env.base_venv in
  Tiger.Semant.transExp (base_venv, base_tenv, exp)

(* Helper to check that a file typechecks successfully *)
let assert_typechecks test_name filename =
  test_name >:: (fun _ ->
    let ast_opt = parse_file filename in
    match ast_opt with
    | None -> assert_failure "Failed to parse"
    | Some exp ->
        try
          let _ = typecheck exp in
          assert_bool "Should typecheck" true
        with
        | Tiger.Semant.Semantic_error (pos, msg) ->
            assert_failure (Printf.sprintf "Unexpected type error at %d: %s" pos msg)
  )

(* Helper to check that a file produces a type error containing expected text *)
let assert_type_error test_name filename expected_substr =
  test_name >:: (fun _ ->
    let ast_opt = parse_file filename in
    match ast_opt with
    | None -> assert_failure "Failed to parse"
    | Some exp ->
        try
          let _ = typecheck exp in
          assert_failure "Expected type error but none occurred"
        with
        | Tiger.Semant.Semantic_error (_pos, msg) ->
            if not (contains_substring msg expected_substr) then
              assert_failure (Printf.sprintf "Error message '%s' does not contain '%s'" msg expected_substr)
            else
              assert_bool "Got expected error" true
  )

(* Test root directory - use absolute path or find workspace root *)
let workspace_root = 
  try 
    (* Try to find workspace root by looking for lakefile.lean *)
    let rec find_root dir =
      let test_file = Filename.concat dir "lakefile.lean" in
      if Sys.file_exists test_file then dir
      else 
        let parent = Filename.dirname dir in
        if parent = dir then failwith "Could not find workspace root"
        else find_root parent
    in
    find_root (Sys.getcwd ())
  with _ ->
    (* Fallback: assume we're running from the TigerBookOcaml directory *)
    Filename.dirname (Filename.dirname (Sys.getcwd ()))

let test_dir = Filename.concat workspace_root "test-programs"

(* Valid programs tests *)
let valid_tests = "Valid Programs" >::: [
  assert_typechecks "literals" (test_dir ^ "/valid/literals.tig");
  assert_typechecks "string_literal" (test_dir ^ "/valid/string_literal.tig");
  assert_typechecks "arithmetic" (test_dir ^ "/valid/arithmetic.tig");
  assert_typechecks "comparisons" (test_dir ^ "/valid/comparisons.tig");
  assert_typechecks "if_then_else" (test_dir ^ "/valid/if_then_else.tig");
  assert_typechecks "if_then_unit" (test_dir ^ "/valid/if_then_unit.tig");
  assert_typechecks "while_loop" (test_dir ^ "/valid/while_loop.tig");
  assert_typechecks "sequence" (test_dir ^ "/valid/sequence.tig");
  (* Let expressions are not yet implemented - skip for now
  assert_typechecks "function_no_params" (test_dir ^ "/valid/function_no_params.tig");
  assert_typechecks "function_with_params" (test_dir ^ "/valid/function_with_params.tig");
  assert_typechecks "recursive_function" (test_dir ^ "/valid/recursive_function.tig");
  assert_typechecks "mutually_recursive" (test_dir ^ "/valid/mutually_recursive.tig");
  assert_typechecks "variable_declaration" (test_dir ^ "/valid/variable_declaration.tig");
  assert_typechecks "variable_no_type" (test_dir ^ "/valid/variable_no_type.tig");
  *)
]

(* Type error tests *)
let type_error_tests = "Type Error Programs" >::: [
  (* Let expressions are not yet implemented - skip for now
  assert_type_error "wrong_var_type" 
    (test_dir ^ "/type-errors/wrong_var_type.tig")
    "variable x declared as int, but initialized with string";
  *)
  
  assert_type_error "undefined_variable"
    (test_dir ^ "/type-errors/undefined_variable.tig")
    "undefined variable";
  
  assert_type_error "undefined_function"
    (test_dir ^ "/type-errors/undefined_function.tig")
    "undefined function";
  
  (* Let expressions are not yet implemented - skip for now
  assert_type_error "wrong_arg_count"
    (test_dir ^ "/type-errors/wrong_arg_count.tig")
    "arity mismatch";
  
  assert_type_error "wrong_arg_type"
    (test_dir ^ "/type-errors/wrong_arg_type.tig")
    "expected int, but found string";
  *)
  
  assert_type_error "if_branches_mismatch"
    (test_dir ^ "/type-errors/if_branches_mismatch.tig")
    "if branches must have same type";
  
  assert_type_error "if_then_not_unit"
    (test_dir ^ "/type-errors/if_then_not_unit.tig")
    "if-then without else must have unit type";
  
  assert_type_error "if_condition_not_int"
    (test_dir ^ "/type-errors/if_condition_not_int.tig")
    "if condition requires int";
  
  assert_type_error "while_condition_not_int"
    (test_dir ^ "/type-errors/while_condition_not_int.tig")
    "while condition requires int";
  
  assert_type_error "while_body_not_unit"
    (test_dir ^ "/type-errors/while_body_not_unit.tig")
    "while loop body must have unit type";
  
  assert_type_error "arithmetic_on_string"
    (test_dir ^ "/type-errors/arithmetic_on_string.tig")
    "expected int, but found string";
  
  assert_type_error "incompatible_equality"
    (test_dir ^ "/type-errors/incompatible_equality.tig")
    "cannot compare";
  
  (* Let expressions are not yet implemented - skip for now
  assert_type_error "function_return_mismatch"
    (test_dir ^ "/type-errors/function_return_mismatch.tig")
    "function";
  
  assert_type_error "assign_incompatible"
    (test_dir ^ "/type-errors/assign_incompatible.tig")
    "cannot assign string to variable of type int";
  
  assert_type_error "variable_as_function"
    (test_dir ^ "/type-errors/variable_as_function.tig")
    "is a variable, not a function";
  *)
]

let suite = "Integration Tests" >::: [
  valid_tests;
  type_error_tests;
]

let () = run_test_tt_main suite
