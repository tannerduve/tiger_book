open Symbol

type ty = Types.ty

type enventry = VarEntry of ty 
                  | FunEntry of {formals : ty list; result : ty}
let base_tenv =
  empty 
  |> add (symbol "int") Types.INT
  |> add (symbol "string") Types.STRING 

(* TODO: FINISH *)
let base_venv = 
  empty 
  |> add (symbol "print") (FunEntry {formals = [Types.STRING]; result = Types.NIL})
  |> add (symbol "flush") (FunEntry {formals = []; result = Types.NIL})
  |> add (symbol "getchar") (FunEntry {formals = []; result = Types.STRING})
  |> add (symbol "ord") (FunEntry {formals = [Types.STRING]; result = Types.INT})
  |> add (symbol "concat") (FunEntry {formals = [Types.STRING; Types.STRING]; result = Types.STRING})

module type Env = sig 
  type ty
  type enventry
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

  