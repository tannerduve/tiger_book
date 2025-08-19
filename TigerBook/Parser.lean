import Parser

inductive Token
| array | break_ | do_ | else_ | end_ | for_ | function | if_ | in_ | let_
| nil | of_ | then_ | to | type_ | var | while
| id     : String → Token
| intLit : Nat    → Token
| strLit : String → Token
| comma | colon | semi | lparen | rparen | lbrack | rbrack | lbrace | rbrace | dot
| assign | plus | minus | times | div
| eq | ne | lt | le | gt | ge
| and_ | or_
deriving Repr, Inhabited
