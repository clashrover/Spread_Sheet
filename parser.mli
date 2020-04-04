type token =
  | Float of (float)
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Comma
  | Colon
  | Indices of (int*int)
  | Ranges of (int*int*int*int)
  | SUM
  | AVG
  | MIN
  | MAX
  | COUNT
  | ROWCOUNT
  | COLCOUNT
  | ROWSUM
  | COLSUM
  | ROWAVG
  | COLAVG
  | ROWMIN
  | COLMIN
  | ROWMAX
  | COLMAX
  | ADD
  | SUBT
  | MULT
  | DIV
  | Assignment
  | Termination

val line :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
