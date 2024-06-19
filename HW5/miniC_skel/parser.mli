type token =
  | NUM of (
# 5 "parser.mly"
        int
# 6 "parser.mli"
)
  | ID of (
# 6 "parser.mly"
        string
# 11 "parser.mli"
)
  | INT
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | EQUALEQUAL
  | LE
  | LT
  | GE
  | GT
  | NOT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | DO
  | READ
  | PRINT
  | SEMICOLON
  | BEGIN
  | END
  | VAR
  | LET
  | IN
  | PROC
  | SKIP
  | LBRACE
  | RBRACE
  | LBLOCK
  | RBLOCK
  | LPAREN
  | RPAREN
  | EOF
  | TRUE
  | FALSE
  | COMMA
  | DOT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> C.program
