
  type unique = unit ref

  type ty = 
  | RECORD of (Symbol.symbol * ty) list * unique 
  | NIL 
  | INT 
  | UNIT
  | STRING 
  | ARRAY of ty * unique 
  | NAME of Symbol.symbol * (ty option) ref