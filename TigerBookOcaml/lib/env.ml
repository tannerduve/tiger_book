(* 
signature ENV =
sig
type access
type ty
datatype enventry = VarEntry of (ty: ty)
I FunEntry of (formals: ty list, result: ty)
val base_tenv : ty Symbol. t a b l e * predefined types *)
(* val b a s e venv : enventry Symbol. table (* predefined functions *
end *) *)

module type Env = sig 

  type access 
  type enventry = VarEntry of Types.ty 
                  | FunEntry of {formals : Types.ty list; result : Types.ty}
  val base_tenv : Types.ty Symbol.table
  val base_venv : enventry Symbol.table
end

  