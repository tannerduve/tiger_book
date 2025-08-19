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

%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%right UMINUS
%nonassoc ASSIGN

%start <expr option> prog
%%

prog:
  | EOF          { None }
  | e = expr     { Some e }

vars:
  | x = ID                                   { SimpleVar x }
  | v = vars; DOT; f = ID                    { FieldVar (v, f) }
  | v = vars; LBRACK; e = expr; RBRACK       { SubscriptVar (v, e) }

args:
  | xs = separated_list(COMMA, expr)         { xs }

record_field:
  | k = ID; EQ; e = expr                     { (k, e) }
record_fields:
  | fs = separated_list(COMMA, record_field) { fs }

seq_elems:
  | xs = separated_list(SEMI, expr)          { xs }

decs:
  |                                          { [] }
  | d = dec; ds = decs                       { d :: ds }

dec:
  | VAR; name = ID; ASSIGN; init = expr
      { VarDec { name; typ = None; init } }
  | VAR; name = ID; COLON; t = ID; ASSIGN; init = expr
      { VarDec { name; typ = Some t; init } }
  /* TODO: FUNCTION f(params) = body / with optional result type */

expr:
  | i = INT                                  { IntConst i }
  | s = STRING                               { StringConst s }
  | NIL                                      { Nil }
  | v = vars                                 { LValue v }
  | v = vars; ASSIGN; e = expr %prec ASSIGN  { Assign { name = v; exp = e } }

  | f = ID; LPAREN; a = args; RPAREN
      { CallExp { func = f; exprs = a } }

  | LPAREN; es = seq_elems; RPAREN
      { match es with [] -> SeqExp [] | [e] -> e | el -> SeqExp el }

  | t = ID; LBRACE; fs = record_fields; RBRACE
      { RecordExp { typ = t; fields = fs } }

  | t = ID; LBRACK; sz = expr; RBRACK; OF; init = expr
      { ArrayExp { typ = t; size = sz; init } }

  | IF; c = expr; THEN; th = expr
      { IfThen { test = c; then_ = th } }
  | IF; c = expr; THEN; th = expr; ELSE; el = expr
      { IfThenElse { test = c; then_ = th; else_ = el } }

  | WHILE; c = expr; DO; b = expr
      { WhileExp { test = c; body = b } }

  | FOR; v = ID; ASSIGN; lo = expr; TO; hi = expr; DO; b = expr
      { ForExp { var = v; lo; hi; body = b } }

  | BREAK                                   { Break }

  | LET; ds = decs; IN; body = seq_elems; END
      { LetExp { decs = ds; body } }

  | MINUS; e = expr %prec UMINUS
      { Minus e }

  | e1 = expr; PLUS;   e2 = expr            { BinOp (e1, Plus,  e2) }
  | e1 = expr; MINUS;  e2 = expr            { BinOp (e1, Minus, e2) }
  | e1 = expr; TIMES;  e2 = expr            { BinOp (e1, Times, e2) }
  | e1 = expr; DIVIDE; e2 = expr            { BinOp (e1, Div,   e2) }

  | e1 = expr; EQ; e2 = expr                { BinOp (e1, Eq,  e2) }
  | e1 = expr; NE; e2 = expr                { BinOp (e1, Neq, e2) }
  | e1 = expr; LT; e2 = expr                { BinOp (e1, Lt,  e2) }
  | e1 = expr; LE; e2 = expr                { BinOp (e1, Le,  e2) }
  | e1 = expr; GT; e2 = expr                { BinOp (e1, Gt,  e2) }
  | e1 = expr; GE; e2 = expr                { BinOp (e1, Ge,  e2) }

  | e1 = expr; AND; e2 = expr               { BinOp (e1, And, e2) }
  | e1 = expr; OR;  e2 = expr               { BinOp (e1, Or,  e2) }
