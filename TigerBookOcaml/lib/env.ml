open Symbol

type ty = Types.ty

type enventry = VarEntry of ty 
                  | FunEntry of {formals : ty list; result : ty}
let base_tenv =
  empty 
  |> add (symbol "int") Types.INT
  |> add (symbol "string") Types.STRING 

let base_venv = 
  empty 
  |> add (symbol "print") (FunEntry {formals = [Types.STRING]; result = Types.NIL})
  |> add (symbol "flush") (FunEntry {formals = []; result = Types.NIL})
  |> add (symbol "getchar") (FunEntry {formals = []; result = Types.STRING})
  |> add (symbol "ord") (FunEntry {formals = [Types.STRING]; result = Types.INT})
  |> add (symbol "concat") (FunEntry {formals = [Types.STRING; Types.STRING]; result = Types.STRING})
  |> add (symbol "chr") (FunEntry {formals = [Types.INT]; result = Types.STRING})
  |> add (symbol "size") (FunEntry {formals = [Types.STRING]; result = Types.INT})
  |> add (symbol "substring") (FunEntry {formals = [Types.STRING; Types.INT; Types.INT]; result = Types.STRING})
  |> add (symbol "not") (FunEntry {formals = [Types.INT]; result = Types.INT})
  |> add (symbol "exit") (FunEntry {formals = [Types.INT]; result = Types.NIL})

module type Env = sig 
  type ty
  type enventry
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

  