{
open Parser  
open Lexing
exception Eof

let error lexbuf msg =
  let p = lexbuf.lex_curr_p in
  let line = p.pos_lnum and col = p.pos_cnum - p.pos_bol in
  failwith (Printf.sprintf "Lexer error %d:%d: %s" line col msg)
}

(* Regular Expressions *)
let digit = ['0'-'9']+ 
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (digit | letter | '_')*


(* Lexing Rules *)



