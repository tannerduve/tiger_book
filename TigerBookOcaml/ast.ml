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

and dec =
  | FunctionDec of fundec list
  | VarDec of { name: string; typ: string option; init: expr }
  | TypeDec of (string * ty) list

and fundec =
  { name: string
  ; params: (string * string) list
  ; result: string option
  ; body: expr }

and ty =
  | NameTy of string
  | RecordTy of (string * string) list
  | ArrayTy of string
