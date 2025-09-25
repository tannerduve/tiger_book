%{
open Ast
%}

/* Keywords */
%token ARRAY
%token BREAK
%token DO 
%token ELSE
%token END 
%token FOR 
%token FUNCTION
%token IF 
%token IN 
%token LET
%token NIL
%token OF
%token THEN 
%token TO 
%token TYPE 
%token VAR 
%token WHILE 

/* Punctuation */
%token COMMA      /* ,  */
%token COLON      /* :  */
%token SEMI       /* ;  */
%token LPAREN     /* (  */
%token RPAREN     /* )  */
%token LBRACK     /* [  */
%token RBRACK     /* ]  */
%token LBRACE     /* {  */
%token RBRACE     /* }  */
%token DOT        /* .  */

/* Operators */
%token ASSIGN     /* := */
%token PLUS       /* +  */
%token MINUS      /* -  */
%token TIMES      /* *  */
%token DIVIDE     /* /  */
%token EQ         /* =  */
%token NE         /* <> */
%token LT         /* <  */
%token LE         /* <= */
%token GT         /* >  */
%token GE         /* >= */
%token AND        /* &  */
%token OR         /* |  */

/* Literals */
%token <string> ID
%token <int>    INT
%token <string> STRING

/* End of input */
%token EOF

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%right UMINUS

%type <expr> exp
%type <lvalue> vars
%type <expr list> args seq_elems
%type <string * expr> record_field
%type <(string * expr) list> record_fields
%type <field> type_field
%type <field list> type_fields
%type <ty> ty
%type <field> field
%type <dec list> decs
%type <dec> dec
%type <vardecl> var_decl
%type <typedec list> type_group
%type <fundec> fun_one
%type <fundec list> fun_group

(* Helper rule types *)
%type <expr list> comma_separated_exps comma_separated_nonempty_exps
%type <field list> comma_separated_fields comma_separated_nonempty_fields
%type <(string * expr) list> comma_separated_record_fields comma_separated_nonempty_record_fields
%type <field list> comma_separated_type_fields comma_separated_nonempty_type_fields
%type <expr list> semi_separated_exps semi_separated_nonempty_exps

%start <expr option> prog

%%

(* Helper rules for comma-separated lists *)
comma_separated_exps:
  |                                       { [] }
  | xs = comma_separated_nonempty_exps    { xs }

comma_separated_nonempty_exps:
  | x = exp                               { [x] }
  | x = exp; COMMA; xs = comma_separated_nonempty_exps { x :: xs }

comma_separated_fields:
  |                                       { [] }
  | xs = comma_separated_nonempty_fields  { xs }

comma_separated_nonempty_fields:
  | x = field                             { [x] }
  | x = field; COMMA; xs = comma_separated_nonempty_fields { x :: xs }

comma_separated_record_fields:
  |                                       { [] }
  | xs = comma_separated_nonempty_record_fields { xs }

comma_separated_nonempty_record_fields:
  | x = record_field                      { [x] }
  | x = record_field; COMMA; xs = comma_separated_nonempty_record_fields { x :: xs }

comma_separated_type_fields:
  |                                       { [] }
  | xs = comma_separated_nonempty_type_fields { xs }

comma_separated_nonempty_type_fields:
  | x = type_field                        { [x] }
  | x = type_field; COMMA; xs = comma_separated_nonempty_type_fields { x :: xs }

semi_separated_exps:
  |                                       { [] }
  | xs = semi_separated_nonempty_exps     { xs }

semi_separated_nonempty_exps:
  | x = exp                               { [x] }
  | x = exp; SEMI; xs = semi_separated_nonempty_exps { x :: xs }

/* program */
prog:
  | EOF                        { None }
  | e = exp; EOF              { Some e }

/* lvalues */
vars:
  | x = ID                                   { SimpleVar x }
  | v = vars; DOT; f = ID                    { FieldVar (v, f) }
  | x = ID; LBRACK; e = exp; RBRACK         { SubscriptVar (SimpleVar x, e) }
  | v = vars; LBRACK; e = exp; RBRACK       { SubscriptVar (v, e) }

/* call args and record literal fields (value-level) */
args:
  | xs = comma_separated_exps               { xs }

record_field:
  | k = ID; EQ; e = exp                     { (k, e) }
record_fields:
  | fs = comma_separated_record_fields      { fs }

/* sequences (exp ; exp ; …) */
seq_elems:
  | xs = semi_separated_exps                { xs }

/* type expessions (type RHS) */
type_field:
  | x = ID; COLON; t = ID { { name = x; typ = t } }

type_fields:
  | fs = comma_separated_type_fields       { fs }

/* type rules build a field list now */
ty:
  | t = ID                                 { NameTy t }
  | LBRACE; fs = type_fields; RBRACE       { RecordTy fs }
  | ARRAY; OF; t = ID                      { ArrayTy t }

/* function parameter (same shape as type_field) */
field:
  | x = ID; COLON; t = ID                    { { name = x; typ = t } }

/* declaration groups */
decs:
  |                                          { [] }
  | d = dec; ds = decs                       { d :: ds }

dec:
  | v = var_decl                             { VarDec v }
  | tg = type_group                          { TypeDec tg }
  | fg = fun_group                           { FunctionDec fg }

/* single var decl */
var_decl:
  | VAR; name = ID; ASSIGN; init = exp
      { { name; typ = None; init } }
  | VAR; name = ID; COLON; t = ID; ASSIGN; init = exp
      { { name; typ = Some t; init } }

/* consecutive TYPE decls are one group (mutual recursion) */
type_group:
  | TYPE; tname = ID; EQ; t = ty
      { [ (tname, t) ] }
  | TYPE; tname = ID; EQ; t = ty; rest = type_group
      { (tname, t) :: rest }

/* function decl (one), then group them if consecutive */
fun_one:
  | FUNCTION; fn = ID; LPAREN; params = comma_separated_fields; RPAREN; EQ; bod = exp
      { { name = fn; params; result = None; body = bod } }
  | FUNCTION; fn = ID; LPAREN; params = comma_separated_fields; RPAREN; COLON; rt = ID; EQ; bod = exp
      { { name = fn; params; result = Some rt; body = bod } }

fun_group:
  | f = fun_one                               { [ f ] }
  | f = fun_one; rest = fun_group             { f :: rest }

/* expressions */
exp:
  | i = INT                                  { IntConst i }
  | s = STRING                               { StringConst s }
  | NIL                                      { Nil }
  | v = vars                                 { LValue v }
  | v = vars; ASSIGN; e = exp %prec ASSIGN  { Assign { name = v; exp = e } }

  | f = ID; LPAREN; a = args; RPAREN
      { CallExp { func = f; exprs = a } }

  | LPAREN; es = seq_elems; RPAREN
      { match es with [] -> SeqExp [] | [e] -> e | el -> SeqExp el }

  | t = ID; LBRACE; fs = record_fields; RBRACE
      { RecordExp { typ = t; fields = fs } }

  | t = ID; LBRACK; sz = exp; RBRACK; OF; init = exp
      { ArrayExp { typ = t; size = sz; init } }

  | IF; c = exp; THEN; th = exp
      { IfThen { test = c; then_ = th } }
  | IF; c = exp; THEN; th = exp; ELSE; el = exp
      { IfThenElse { test = c; then_ = th; else_ = el } }

  | WHILE; c = exp; DO; b = exp
      { WhileExp { test = c; body = b } }

  | FOR; v = ID; ASSIGN; lo = exp; TO; hi = exp; DO; b = exp
      { ForExp { var = v; lo; hi; body = b } }

  | BREAK                                   { Break }

  | LET; ds = decs; IN; body = seq_elems; END
      { LetExp { decs = ds; body } }

  | MINUS; e = exp %prec UMINUS
      { Minus e }

  | e1 = exp; PLUS;   e2 = exp            { BinOp (e1, Plus,  e2) }
  | e1 = exp; MINUS;  e2 = exp            { BinOp (e1, Minus, e2) }
  | e1 = exp; TIMES;  e2 = exp            { BinOp (e1, Times, e2) }
  | e1 = exp; DIVIDE; e2 = exp            { BinOp (e1, Div,   e2) }

  | e1 = exp; EQ; e2 = exp                { BinOp (e1, Eq,  e2) }
  | e1 = exp; NE; e2 = exp                { BinOp (e1, Neq, e2) }
  | e1 = exp; LT; e2 = exp                { BinOp (e1, Lt,  e2) }
  | e1 = exp; LE; e2 = exp                { BinOp (e1, Le,  e2) }
  | e1 = exp; GT; e2 = exp                { BinOp (e1, Gt,  e2) }
  | e1 = exp; GE; e2 = exp                { BinOp (e1, Ge,  e2) }

  | e1 = exp; AND; e2 = exp               { BinOp (e1, And, e2) }
  | e1 = exp; OR;  e2 = exp               { BinOp (e1, Or,  e2) }
  
%%