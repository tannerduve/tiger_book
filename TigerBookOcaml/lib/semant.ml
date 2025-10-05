
open Ast

module Semant = 
struct
let transProg (exp: Ast.expr) : unit = 
    match exp with 
    | StringConst(_ : string) -> ()
    | IntConst(_ : int) -> ()
    | Nil -> ()
    | LValue(_ : Ast.lvalue) -> ()
    | Minus(_) -> () (* todo *) 
    | _ -> () (* todo *)

end