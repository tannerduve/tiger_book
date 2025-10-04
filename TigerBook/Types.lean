/-
open Symbol

module Types = 
  struct 

  type unique = unit ref

  type ty = 
  | RECORD of (Symbol.symbol * ty) list * unique 
  | NIL 
  | INT 
  | STRING 
  | ARRAY of ty * unique 

end
-/