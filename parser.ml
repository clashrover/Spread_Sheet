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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Bmodule
# 38 "parser.ml"
let yytransl_const = [|
  258 (* LParen *);
  259 (* RParen *);
  260 (* LBracket *);
  261 (* RBracket *);
  262 (* Comma *);
  263 (* Colon *);
  266 (* SUM *);
  267 (* AVG *);
  268 (* MIN *);
  269 (* MAX *);
  270 (* COUNT *);
  271 (* ROWCOUNT *);
  272 (* COLCOUNT *);
  273 (* ROWSUM *);
  274 (* COLSUM *);
  275 (* ROWAVG *);
  276 (* COLAVG *);
  277 (* ROWMIN *);
  278 (* COLMIN *);
  279 (* ROWMAX *);
  280 (* COLMAX *);
  281 (* ADD *);
  282 (* SUBT *);
  283 (* MULT *);
  284 (* DIV *);
  285 (* Assignment *);
  286 (* Termination *);
    0|]

let yytransl_block = [|
  257 (* Float *);
  264 (* Indices *);
  265 (* Ranges *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\001\000\002\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\009\000\012\000\
\015\000\003\000\004\000\005\000\007\000\008\000\010\000\011\000\
\013\000\014\000\016\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\021\000\020\000\022\000\018\000\024\000\026\000\025\000\
\027\000\023\000\029\000\031\000\030\000\032\000\028\000\034\000\
\036\000\035\000\037\000\033\000"

let yydgoto = "\002\000\
\004\000\005\000"

let yysindex = "\019\000\
\014\255\000\000\251\254\000\000\014\255\246\254\000\000\026\255\
\028\255\030\255\040\255\041\255\042\255\043\255\044\255\045\255\
\046\255\047\255\048\255\049\255\050\255\051\255\018\255\020\255\
\022\255\024\255\031\255\032\255\034\255\036\255\038\255\052\255\
\053\255\054\255\055\255\056\255\057\255\058\255\059\255\060\255\
\061\255\062\255\063\255\033\255\064\255\065\255\035\255\066\255\
\067\255\037\255\068\255\069\255\039\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\070\255\071\255\072\255\073\255\
\074\255\075\255\076\255\077\255\078\255\079\255\080\255\081\255\
\082\255\083\255\084\255\085\255\086\255\087\255\088\255\089\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\063\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\060\000\000\000"

let yytablesize = 119
let yytable = "\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\042\000\001\000\045\000\003\000\048\000\006\000\
\051\000\043\000\044\000\046\000\047\000\049\000\050\000\052\000\
\053\000\071\000\027\000\076\000\028\000\081\000\029\000\086\000\
\072\000\073\000\077\000\078\000\082\000\083\000\087\000\088\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\054\000\055\000\001\000\056\000\
\007\000\057\000\000\000\058\000\000\000\000\000\069\000\070\000\
\074\000\075\000\079\000\080\000\084\000\085\000\000\000\000\000\
\000\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000"

let yycheck = "\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\001\001\001\000\001\001\008\001\001\001\029\001\
\001\001\008\001\009\001\008\001\009\001\008\001\009\001\008\001\
\009\001\001\001\009\001\001\001\009\001\001\001\009\001\001\001\
\008\001\009\001\008\001\009\001\008\001\009\001\008\001\009\001\
\009\001\009\001\009\001\009\001\009\001\009\001\009\001\009\001\
\009\001\009\001\009\001\009\001\030\001\030\001\000\000\030\001\
\005\000\030\001\255\255\030\001\255\255\255\255\009\001\009\001\
\009\001\009\001\009\001\009\001\009\001\009\001\255\255\255\255\
\255\255\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
\030\001\030\001\030\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\030\001\030\001\030\001\030\001\030\001\
\030\001\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
\030\001\030\001\030\001\030\001\030\001\030\001\030\001"

let yynames_const = "\
  LParen\000\
  RParen\000\
  LBracket\000\
  RBracket\000\
  Comma\000\
  Colon\000\
  SUM\000\
  AVG\000\
  MIN\000\
  MAX\000\
  COUNT\000\
  ROWCOUNT\000\
  COLCOUNT\000\
  ROWSUM\000\
  COLSUM\000\
  ROWAVG\000\
  COLAVG\000\
  ROWMIN\000\
  COLMIN\000\
  ROWMAX\000\
  COLMAX\000\
  ADD\000\
  SUBT\000\
  MULT\000\
  DIV\000\
  Assignment\000\
  Termination\000\
  "

let yynames_block = "\
  Float\000\
  Indices\000\
  Ranges\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 27 "parser.mly"
                                  ( print_newline() )
# 221 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 28 "parser.mly"
                                  ( print_newline() )
# 229 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 32 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- full_count refarr.(0) _4 _1 in () )
# 237 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 33 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- row_count refarr.(0) _4 _1 in () )
# 245 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 34 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- col_count refarr.(0) _4 _1 in () )
# 253 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 36 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- full_sum refarr.(0) _4 _1 in () )
# 261 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 37 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- row_sum refarr.(0) _4 _1 in () )
# 269 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 38 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- col_sum refarr.(0) _4 _1 in () )
# 277 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 40 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- full_avg refarr.(0) _4 _1 in () )
# 285 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 41 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- row_avg refarr.(0) _4 _1 in () )
# 293 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 42 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- col_avg refarr.(0) _4 _1 in () )
# 301 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 44 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- full_min refarr.(0) _4 _1 in () )
# 309 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 45 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- row_min refarr.(0) _4 _1 in () )
# 317 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 46 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- col_min refarr.(0) _4 _1 in () )
# 325 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 48 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- full_max refarr.(0) _4 _1 in () )
# 333 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 49 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- row_max refarr.(0) _4 _1 in () )
# 341 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 50 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- col_max refarr.(0) _4 _1 in () )
# 349 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 53 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- add_range refarr.(0) _4 _5 _1 in () )
# 358 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 54 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- add_const refarr.(0) _5 _4 _1 in () )
# 367 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 55 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- add_const refarr.(0) _4 _5 _1 in () )
# 376 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 56 "parser.mly"
                                                            ( print_newline() ; let (a1,a2) = _4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- add_const temp _5 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- add_const temp _5 (float_of_int t2) _1 in ()
                                                            )
# 392 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int) in
    Obj.repr(
# 64 "parser.mly"
                                                            ( print_newline() ; let (a1,a2) = _5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- add_const temp _4 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- add_const temp _4 (float_of_int t2) _1 in ()
                                                            )
# 408 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 74 "parser.mly"
                                                             ( print_newline() ; let al1 = refarr.(0) <- subt_range refarr.(0) _4 _5 _1 in () )
# 417 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 75 "parser.mly"
                                                             ( print_newline() ; let al1 = refarr.(0) <- subt_const refarr.(0) _5 _4 _1 in () )
# 426 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 76 "parser.mly"
                                                             ( print_newline() ; let al1 = refarr.(0) <- subt_const refarr.(0) _4 _5 _1 in () )
# 435 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 77 "parser.mly"
                                                             ( print_newline() ; let (a1,a2) = _4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- subt_const temp _5 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- subt_const temp _5 (float_of_int t2) _1 in ()
                                                            )
# 451 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int) in
    Obj.repr(
# 85 "parser.mly"
                                                             ( print_newline() ; let (a1,a2) = _5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- subt_const temp _4 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- subt_const temp _4 (float_of_int t2) _1 in ()
                                                            )
# 467 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 95 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- mult_range refarr.(0) _4 _5 _1 in () )
# 476 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 96 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- mult_const refarr.(0) _5 _4 _1 in () )
# 485 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 97 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- mult_const refarr.(0) _4 _5 _1 in () )
# 494 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 98 "parser.mly"
                                                            ( print_newline() ; let (a1,a2) = _4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- mult_const temp _5 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- mult_const temp _5 (float_of_int t2) _1 in ()
                                                            )
# 510 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int) in
    Obj.repr(
# 106 "parser.mly"
                                                            ( print_newline() ; let (a1,a2) = _5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- mult_const temp _4 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- mult_const temp _4 (float_of_int t2) _1 in ()
                                                            )
# 526 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 116 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- div_range refarr.(0) _4 _5 _1 in () )
# 535 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 117 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- div_const refarr.(0) _5 _4 _1 in () )
# 544 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 118 "parser.mly"
                                                            ( print_newline() ; let al1 = refarr.(0) <- div_const refarr.(0) _4 _5 _1 in () )
# 553 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int*int*int) in
    Obj.repr(
# 119 "parser.mly"
                                                            ( print_newline() ; let (a1,a2) = _4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- div_const temp _5 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- div_const temp _5 (float_of_int t2) _1 in ()
                                                            )
# 569 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int*int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int*int*int*int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int*int) in
    Obj.repr(
# 127 "parser.mly"
                                                            ( print_newline() ; let (a1,a2) = _5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- div_const temp _4 t1 _1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- div_const temp _4 (float_of_int t2) _1 in ()
                                                            )
# 585 "parser.ml"
               : 'formula))
(* Entry line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
