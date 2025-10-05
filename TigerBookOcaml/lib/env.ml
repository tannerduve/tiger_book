module type Env = sig 

  type access 
  type enventry = VarEntry of Types.ty 
                  | FunEntry of {formals : Types.ty list; result : Types.ty}
  val base_tenv : Types.ty Symbol.table
  val base_venv : enventry Symbol.table
end

  