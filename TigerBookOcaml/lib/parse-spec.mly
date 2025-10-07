%{
open Ast
let pos_of (p: Lexing.position) = p.Lexing.pos_cnum
let sym = Symbol.symbol
%}

%token ARRAY BREAK DO ELSE END FOR FUNCTION IF IN LET NIL OF THEN TO TYPE VAR WHILE
%token COMMA COLON SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE DOT
%token ASSIGN PLUS MINUS TIMES DIVIDE EQ NE LT LE GT GE AND OR
%token <string> ID
%token <int> INT
%token <string> STRING
%token EOF

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%nonassoc THEN
%nonassoc ELSE
%left PLUS MINUS
%left TIMES DIVIDE
%right UMINUS

%type <Ast.exp> exp
%type <Ast.var> vars
%type <(Ast.exp * Ast.pos) list> seq_elems
%type <(Ast.symbol * Ast.exp * Ast.pos) list> record_fields
%type <(Ast.symbol * Ast.exp * Ast.pos)> record_field
%type <Ast.field> field
%type <Ast.field list> fields
%type <Ast.ty> ty
%type <Ast.dec list> decs
%type <Ast.dec> dec
%type <Ast.fundec> fun_one
%type <Ast.fundec list> fun_group
%type <Ast.typedecrec list> type_group
%type <Ast.exp list> args
%type <Ast.exp list> args_nonempty
%type <Ast.field list> fields_nonempty
%type <(Ast.symbol * Ast.exp * Ast.pos) list> record_fields_nonempty
%type <(Ast.exp * Ast.pos) list> seq_elems_nonempty
%type <Ast.dec> var_decl


%start <Ast.exp option> prog

%%

prog:
  | EOF                           { None }
  | e = exp; EOF                  { Some e }

vars:
  | x = ID                        { SimpleVar (sym x, pos_of $startpos) }
  | v = vars; DOT; f = ID         { FieldVar (v, sym f, pos_of $startpos) }
  | x = ID; LBRACK; e = exp; RBRACK
                                  { SubscriptVar (SimpleVar (sym x, pos_of $startpos), e, pos_of $startpos) }
  | v = vars; LBRACK; e = exp; RBRACK
                                  { SubscriptVar (v, e, pos_of $startpos) }

args:
  |                              { [] }
  | xs = args_nonempty           { xs }

args_nonempty:
  | e = exp                      { [e] }
  | e = exp; COMMA; xs = args_nonempty { e :: xs }

record_fields:
  |                              { [] }
  | xs = record_fields_nonempty  { xs }

record_fields_nonempty:
  | rf = record_field            { [rf] }
  | rf = record_field; COMMA; xs = record_fields_nonempty { rf :: xs }

record_field:
  | k = ID; EQ; e = exp          { (sym k, e, pos_of $startpos) }

seq_elems:
  |                              { [] }
  | xs = seq_elems_nonempty      { xs }

seq_elems_nonempty:
  | e = exp                      { [ (e, pos_of $startpos) ] }
  | e = exp; SEMI; xs = seq_elems_nonempty
                                 { (e, pos_of $startpos) :: xs }

fields:
  |                              { [] }
  | xs = fields_nonempty         { xs }

fields_nonempty:
  | f = field                    { [f] }
  | f = field; COMMA; xs = fields_nonempty { f :: xs }

field:
  | x = ID; COLON; t = ID        { { name = sym x; escape = ref true; typ = sym t; pos = pos_of $startpos } }

ty:
  | t = ID                       { NameTy (sym t, pos_of $startpos) }
  | LBRACE; fs = fields; RBRACE  { RecordTy fs }
  | ARRAY; OF; t = ID            { ArrayTy (sym t, pos_of $startpos) }

decs:
  |                              { [] }
  | d = dec; ds = decs           { d :: ds }

dec:
  | v = var_decl                 { v }
  | tg = type_group              { TypeDec tg }
  | fg = fun_group               { FunctionDec fg }

var_decl:
  | VAR; name = ID; ASSIGN; init = exp
      { VarDec { name = sym name; escape = ref true; typ = None; init; pos = pos_of $startpos } }
  | VAR; name = ID; COLON; t = ID; ASSIGN; init = exp
      { VarDec { name = sym name; escape = ref true; typ = Some (sym t, pos_of $startpos(t)); init; pos = pos_of $startpos } }

type_group:
  | TYPE; tname = ID; EQ; t = ty
      { [ { name = sym tname; ty = t; pos = pos_of $startpos } ] }
  | TYPE; tname = ID; EQ; t = ty; rest = type_group
      { { name = sym tname; ty = t; pos = pos_of $startpos } :: rest }

fun_one:
  | FUNCTION; fn = ID; LPAREN; params = fields; RPAREN; EQ; bod = exp
      { { name = sym fn; params; result = None; body = bod; pos = pos_of $startpos } }
  | FUNCTION; fn = ID; LPAREN; params = fields; RPAREN; COLON; rt = ID; EQ; bod = exp
      { { name = sym fn; params; result = Some (sym rt, pos_of $startpos(rt)); body = bod; pos = pos_of $startpos } }

fun_group:
  | f = fun_one                  { [f] }
  | f = fun_one; rest = fun_group { f :: rest }

exp:
  | i = INT                      { IntExp i }
  | s = STRING                   { StringExp (s, pos_of $startpos) }
  | NIL                          { NilExp }
  | v = vars                     { VarExp v }
  | v = vars; ASSIGN; e = exp %prec ASSIGN
                                 { AssignExp { var = v; exp = e; pos = pos_of $startpos } }
  | f = ID; LPAREN; a = args; RPAREN
                                 { CallExp { func = sym f; args = a; pos = pos_of $startpos } }
  | LPAREN; es = seq_elems; RPAREN
                                 { SeqExp es }
  | t = ID; LBRACE; fs = record_fields; RBRACE
                                 { RecordExp { typ = sym t; fields = fs; pos = pos_of $startpos } }
  | t = ID; LBRACK; sz = exp; RBRACK; OF; init = exp
                                 { ArrayExp { typ = sym t; size = sz; init; pos = pos_of $startpos } }
  | IF; c = exp; THEN; th = exp %prec THEN
                                 { IfExp { test = c; then_ = th; else_ = None; pos = pos_of $startpos } }
  | IF; c = exp; THEN; th = exp; ELSE; el = exp
                                 { IfExp { test = c; then_ = th; else_ = Some el; pos = pos_of $startpos } }
  | WHILE; c = exp; DO; b = exp
                                 { WhileExp { test = c; body = b; pos = pos_of $startpos } }
  | FOR; v = ID; ASSIGN; lo = exp; TO; hi = exp; DO; b = exp
                                 { ForExp { var = sym v; escape = ref true; lo; hi; body = b; pos = pos_of $startpos } }
  | BREAK                        { BreakExp (pos_of $startpos) }
  | LET; ds = decs; IN; b = exp; END
                                 { LetExp { decs = ds; body = b; pos = pos_of $startpos } }
  | MINUS; e = exp %prec UMINUS
                                 { OpExp { left = IntExp 0; oper = MinusOp; right = e; pos = pos_of $startpos } }
  | e1 = exp; PLUS;   e2 = exp   { OpExp { left = e1; oper = PlusOp;   right = e2; pos = pos_of $startpos } }
  | e1 = exp; MINUS;  e2 = exp   { OpExp { left = e1; oper = MinusOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; TIMES;  e2 = exp   { OpExp { left = e1; oper = TimesOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; DIVIDE; e2 = exp   { OpExp { left = e1; oper = DivideOp; right = e2; pos = pos_of $startpos } }
  | e1 = exp; EQ; e2 = exp       { OpExp { left = e1; oper = EqOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; NE; e2 = exp       { OpExp { left = e1; oper = NeqOp; right = e2; pos = pos_of $startpos } }
  | e1 = exp; LT; e2 = exp       { OpExp { left = e1; oper = LtOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; LE; e2 = exp       { OpExp { left = e1; oper = LeOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; GT; e2 = exp       { OpExp { left = e1; oper = GtOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; GE; e2 = exp       { OpExp { left = e1; oper = GeOp;  right = e2; pos = pos_of $startpos } }
  | e1 = exp; AND; e2 = exp
                                 { IfExp { test = e1; then_ = e2; else_ = Some (IntExp 0); pos = pos_of $startpos } }
  | e1 = exp; OR;  e2 = exp
                                 { IfExp { test = e1; then_ = IntExp 1; else_ = Some e2; pos = pos_of $startpos } }
