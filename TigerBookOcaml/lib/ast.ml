(* AST types for Tiger language *)
type ident  = string      (* value/function names *)
type tident = string      (* type names *)

type binop =
  | Plus | Minus | Times | Div
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or

type expr =
  | StringConst of string
  | IntConst of int
  | Nil
  | LValue of lvalue
  | Minus of expr
  | BinOp of expr * binop * expr
  | Assign of { name: lvalue; exp: expr }
  | CallExp of { func: string; exprs: expr list }
  | SeqExp of expr list
  | RecordExp of { typ: string; fields: (string * expr) list }
  | ArrayExp of { typ: string; size: expr; init: expr }
  | IfThen of { test: expr; then_: expr }
  | IfThenElse of { test: expr; then_: expr; else_: expr }
  | WhileExp of { test: expr; body: expr }
  | ForExp of { var: string; lo: expr; hi: expr; body: expr }
  | Break
  | LetExp of { decs: dec list; body: expr list }

and lvalue =
  | SimpleVar of string
  | FieldVar of lvalue * string
  | SubscriptVar of lvalue * expr

and field = { name: ident; typ: tident }

and ty =
  | NameTy   of tident
  | RecordTy of field list
  | ArrayTy  of tident

and typedec = tident * ty

and vardecl = { name: ident; typ: tident option; init: expr }

and fundec =
  { name   : ident
  ; params : field list
  ; result : tident option
  ; body   : expr }

and dec =
  | FunctionDec of fundec list
  | VarDec of vardecl
  | TypeDec of typedec list

(* Pretty printing functions for testing *)
let rec string_of_expr = function
  | IntConst i -> string_of_int i
  | StringConst s -> "\"" ^ s ^ "\""
  | Nil -> "nil"
  | LValue lv -> string_of_lvalue lv
  | Minus e -> "-(" ^ string_of_expr e ^ ")"
  | BinOp (e1, op, e2) -> 
      "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"
  | Break -> "break"
  | _ -> "<complex expression>"

and string_of_lvalue = function
  | SimpleVar s -> s
  | FieldVar (lv, f) -> string_of_lvalue lv ^ "." ^ f
  | SubscriptVar (lv, e) -> string_of_lvalue lv ^ "[" ^ string_of_expr e ^ "]"

and string_of_binop = function
  | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"
  | Eq -> "=" | Neq -> "<>" | Lt -> "<" | Le -> "<="
  | Gt -> ">" | Ge -> ">=" | And -> "&" | Or -> "|"