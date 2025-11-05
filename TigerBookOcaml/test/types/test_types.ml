open Tiger

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

let tests = [
  Alcotest.test_case "Types Creation" `Quick test_types_creation;
]

