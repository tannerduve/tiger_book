{
open Parser
open Lexing
exception Eof
exception SyntaxError of string

let error lexbuf msg =
  let p = lexbuf.lex_curr_p in
  let line = p.pos_lnum and col = p.pos_cnum - p.pos_bol in
  failwith (Printf.sprintf "Lexer error %d:%d: %s" line col msg)

let keyword_or_id (s : string) = 
  match s with 
  | "array"   -> ARRAY
  | "break"   -> BREAK
  | "do"      -> DO
  | "else"    -> ELSE
  | "end"     -> END
  | "for"     -> FOR
  | "function"-> FUNCTION
  | "if"      -> IF
  | "in"      -> IN
  | "let"     -> LET
  | "nil"     -> NIL
  | "of"      -> OF
  | "then"    -> THEN
  | "to"      -> TO
  | "type"    -> TYPE
  | "var"     -> VAR
  | "while"   -> WHILE
  | _         -> ID s

let add_dec_escape buf n =
  if n < 0 || n > 255 then invalid_arg "byte range";
  Buffer.add_char buf (Char.chr n)

let add_ctrl_escape buf c =
  (* Map @ A ... Z [ \ ] ^ _  to chars 0..31 (ASCII) *)
  let code = (Char.code c) - (Char.code '@') in
  if code < 0 || code > 31 then invalid_arg "ctrl";
  Buffer.add_char buf (Char.chr code)
}

(* Regular Expressions *)
let digit = ['0'-'9']+ 
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (digit | letter | '_')*

(* Escape Sequences *)
let white = [' ' '\t']+
let newline = '\n'

(* Lexing Rules *)
rule token = parse
  | white                           { token lexbuf }
  | newline                         { new_line lexbuf; token lexbuf }
  | "/*"                            { comment 1 lexbuf; token lexbuf }

  (* -------- identifiers & keywords -------- *)
  | ident as s                      { keyword_or_id s }
  (* TODO: try feeding "LET" vs "let" to observe case-sensitivity *)

  (* -------- integers -------- *)
  | digit as s                      { INT (int_of_string s) }
  (* TODO: catch overflow:
           try INT (int_of_string s) with Failure _ -> error lexbuf "integer overflow" *)

  (* -------- strings -------- *)
  | '"'                             { read_string (Buffer.create 32) lexbuf }
  (* TODO: forbid raw newlines inside strings unless via folding escape *)

  (* -------- multi-char operators (order matters) -------- *)
  | ":="                            { ASSIGN }
  | "<="                            { LE }
  | ">="                            { GE }
  | "<>"                            { NE }

  (* -------- single-char punctuation/operators -------- *)
  | ','                             { COMMA  } | ':'     { COLON  } | ';'   { SEMI   }
  | '('                             { LPAREN } | ')'     { RPAREN } | '['   { LBRACK }
  | ']'                             { RBRACK } | '{'     { LBRACE } | '}'   { RBRACE }
  | '.'                             { DOT    } | '+'     { PLUS   } | '-'   { MINUS  }
  | '*'                             { TIMES  } | '/'     { DIVIDE } | '='   { EQ     }
  | '<'                             { LT     } | '>'     { GT     } | '&'   { AND    }
  | '|'                             { OR     }

  (* -------- end of input -------- *)
  | eof                             { EOF }

  (* -------- fallback -------- *)
  | _                               {
      error lexbuf (Printf.sprintf "unexpected char %S" (Lexing.lexeme lexbuf))
    }

and comment depth = parse
  | "/*"                            { comment (depth + 1) lexbuf }
  | "*/"                            { if depth = 1 then () else comment (depth - 1) lexbuf }
  | newline                         { new_line lexbuf; comment depth lexbuf }
  | eof                             { error lexbuf "unterminated comment" }
  | _                               { comment depth lexbuf }
  (* TODO: if you add '\r' handling to [newline], ensure CRLF only counts once *)

and read_string buf = parse
  (* closing quote *)
  | '"'                             { STRING (Buffer.contents buf) }

  (* simple escapes *)
  | "\\n"                           { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\t"                           { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\\""                          { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | "\\\\"                          { Buffer.add_char buf '\\'; read_string buf lexbuf }

  (* decimal escape: \ddd (exactly 3 digits) *)
  | '\\' (['0'-'9']['0'-'9']['0'-'9'] as ddd) {
      (try add_dec_escape buf (int_of_string ddd)
       with _ -> error lexbuf "bad \\ddd escape");
      read_string buf lexbuf
    }

  (* control escape: \^C where C ∈ @, A..Z, [, \, ], ^, _ *)
  | "\\^" (['@'-'Z' '[' '\\' ']' '^' '_'] as c) {
      (try add_ctrl_escape buf c
       with _ -> error lexbuf "bad \\^c escape");
      read_string buf lexbuf
    }

  (* folding escape: backslash, whitespace/newlines, backslash *)
  | '\\' (white | newline)+ '\\' {
      (* Update line numbers for any newlines in the folding *)
      let s = Lexing.lexeme lexbuf in
      String.iter (function '\n' -> new_line lexbuf | _ -> ()) s;
      read_string buf lexbuf
    }

  (* unterminated string *)
  | eof                             { error lexbuf "unterminated string" }

  (* regular characters *)
  | _ as c                          { Buffer.add_char buf c; read_string buf lexbuf }
