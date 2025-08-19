inductive BinOp where
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  deriving Repr, BEq

mutual

inductive Expr where
  | stringConst (s : String)
  | intConst (n : Int)
  | nil
  | lvalue (lv : LValue)
  | neg (e : Expr)                     -- for unary minus
  | binOp (e1 : Expr) (op : BinOp) (e2 : Expr)
  | assign (name : LValue) (exp : Expr)
  | call (func : String) (args : List Expr)
  | seq (es : List Expr)
  | record (typ : String) (fields : List (String × Expr))
  | array (typ : String) (size : Expr) (init : Expr)
  | ifThen (test : Expr) (then_ : Expr)
  | ifThenElse (test : Expr) (then_ : Expr) (else_ : Expr)
  | whileLoop (test : Expr) (body : Expr)
  | forLoop (var : String) (lo : Expr) (hi : Expr) (body : Expr)
  | break
  | letExp (decs : List Dec) (body : List Expr)
  deriving Repr, BEq

inductive LValue where
  | simple (id : String)
  | field (lv : LValue) (field : String)
  | subscript (lv : LValue) (idx : Expr)
  deriving Repr, BEq

inductive Dec where
  | function (funs : List FunDec)
  | varDec (name : String) (typ : Option String) (init : Expr)
  | typeDec (types : List (String × Ty))
  deriving Repr, BEq

inductive FunDec where
  | mk (name : String) (params : List (String × String))
        (result : Option String) (body : Expr)
  deriving Repr, BEq

inductive Ty where
  | name (id : String)
  | record (fields : List (String × String))
  | array (id : String)
  deriving Repr, BEq

end
