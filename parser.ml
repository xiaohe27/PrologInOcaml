type token =
  | STRING of (string)
  | VARIABLE of (string)
  | NAME of (string)
  | FLOAT of (float)
  | INT of (int)
  | DOT
  | DOUBLEDOT
  | COLONHYPHEN
  | QUESTIONHYPHEN
  | ARROW
  | NOT
  | TERM_EQ
  | TERM_INEQ
  | IS
  | AS
  | TERM_DECOMP
  | TERM_UNIFY
  | TERM_NOTUNIFY
  | ARITH_EQ
  | ARITH_INEQ
  | ARITH_LESS
  | ARITH_GREATER
  | ARITH_GEQ
  | ARITH_LEQ
  | TERM_ORDER_EQ
  | TERM_ORDER_INEQ
  | TERM_ORDER_GREATER
  | TERM_ORDER_LESS
  | TERM_ORDER_GEQ
  | TERM_ORDER_LEQ
  | DOUBLECOLON
  | PLUS
  | MINUS
  | MULT
  | DIV
  | BITWISE_AND
  | BITWISE_OR
  | BITWISE_NOT
  | VAR_INSTANTIATED
  | SEMICOLON
  | COMMA
  | COLON
  | UMINUS
  | UPLUS
  | CUT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | PIPE
  | EOF
  | BOOL of (bool)
  | DOLLAR

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

        open ProjCommon
# 62 "parser.ml"
let yytransl_const = [|
  262 (* DOT *);
  263 (* DOUBLEDOT *);
  264 (* COLONHYPHEN *);
  265 (* QUESTIONHYPHEN *);
  266 (* ARROW *);
  267 (* NOT *);
  268 (* TERM_EQ *);
  269 (* TERM_INEQ *);
  270 (* IS *);
  271 (* AS *);
  272 (* TERM_DECOMP *);
  273 (* TERM_UNIFY *);
  274 (* TERM_NOTUNIFY *);
  275 (* ARITH_EQ *);
  276 (* ARITH_INEQ *);
  277 (* ARITH_LESS *);
  278 (* ARITH_GREATER *);
  279 (* ARITH_GEQ *);
  280 (* ARITH_LEQ *);
  281 (* TERM_ORDER_EQ *);
  282 (* TERM_ORDER_INEQ *);
  283 (* TERM_ORDER_GREATER *);
  284 (* TERM_ORDER_LESS *);
  285 (* TERM_ORDER_GEQ *);
  286 (* TERM_ORDER_LEQ *);
  287 (* DOUBLECOLON *);
  288 (* PLUS *);
  289 (* MINUS *);
  290 (* MULT *);
  291 (* DIV *);
  292 (* BITWISE_AND *);
  293 (* BITWISE_OR *);
  294 (* BITWISE_NOT *);
  295 (* VAR_INSTANTIATED *);
  296 (* SEMICOLON *);
  297 (* COMMA *);
  298 (* COLON *);
  299 (* UMINUS *);
  300 (* UPLUS *);
  301 (* CUT *);
  302 (* LPAREN *);
  303 (* RPAREN *);
  304 (* LBRACKET *);
  305 (* RBRACKET *);
  306 (* PIPE *);
    0 (* EOF *);
  308 (* DOLLAR *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* VARIABLE *);
  259 (* NAME *);
  260 (* FLOAT *);
  261 (* INT *);
  307 (* BOOL *);
    0|]

let yylhs = "\255\255\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
\005\000\006\000\006\000\007\000\007\000\007\000\008\000\008\000\
\008\000\008\000\008\000\009\000\009\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\011\000\011\000\012\000\013\000\013\000\004\000\004\000\
\014\000\014\000\014\000\015\000\015\000\016\000\016\000\016\000\
\017\000\017\000\018\000\018\000\019\000\001\000\002\000\002\000\
\000\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\001\000\
\002\000\001\000\003\000\001\000\003\000\003\000\001\000\003\000\
\003\000\003\000\003\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\001\000\002\000\003\000\001\000\001\000\
\001\000\003\000\003\000\004\000\001\000\001\000\003\000\003\000\
\002\000\004\000\001\000\002\000\003\000\001\000\002\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\065\000\000\000\000\000\062\000\
\000\000\000\000\066\000\064\000\000\000\057\000\000\000\060\000\
\000\000\000\000\063\000\004\000\001\000\006\000\003\000\002\000\
\000\000\000\000\005\000\000\000\008\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\044\000\048\000\047\000\000\000\
\000\000\000\000\000\000\061\000\000\000\045\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\000\058\000\
\056\000\055\000\007\000\046\000\050\000\051\000\011\000\013\000\
\014\000\000\000\000\000\000\000\000\000\021\000\031\000\032\000\
\033\000\034\000\035\000\029\000\030\000\023\000\024\000\028\000\
\027\000\025\000\026\000\040\000\041\000\038\000\039\000\036\000\
\037\000\043\000"

let yydgoto = "\003\000\
\005\000\011\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\017\000\018\000\
\007\000\008\000\012\000"

let yysindex = "\061\000\
\012\255\005\255\000\000\232\254\000\000\049\255\012\255\000\000\
\012\255\019\255\000\000\000\000\008\255\000\000\012\255\000\000\
\108\255\027\255\000\000\000\000\000\000\000\000\000\000\000\000\
\008\255\001\255\000\000\022\255\000\000\244\254\253\254\000\000\
\148\255\105\255\111\000\031\255\000\000\000\000\000\000\255\254\
\045\255\012\255\012\255\000\000\011\255\000\000\044\255\000\000\
\008\255\008\255\015\255\015\255\015\255\015\255\015\255\015\255\
\015\255\015\255\015\255\015\255\015\255\015\255\015\255\015\255\
\015\255\015\255\015\255\015\255\015\255\015\255\015\255\015\255\
\015\255\015\255\015\255\015\255\015\255\015\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\148\255\148\255\148\255\148\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\024\255\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\095\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\255\062\255\000\000\
\103\255\052\000\246\254\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\144\255\185\255\226\255\011\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\108\000\000\000\086\000\109\000\000\000\018\000\051\000\000\000\
\045\001\000\000\065\000\000\000\000\000\251\255\041\000\248\255\
\000\000\139\000\141\000"

let yytablesize = 397
let yytable = "\022\000\
\059\000\020\000\021\000\022\000\023\000\024\000\041\000\004\000\
\020\000\021\000\022\000\023\000\024\000\009\000\004\000\020\000\
\021\000\022\000\023\000\024\000\047\000\013\000\020\000\021\000\
\022\000\023\000\024\000\009\000\049\000\053\000\022\000\053\000\
\044\000\081\000\082\000\051\000\022\000\050\000\022\000\022\000\
\078\000\006\000\006\000\085\000\086\000\079\000\025\000\006\000\
\026\000\046\000\080\000\027\000\028\000\025\000\014\000\026\000\
\015\000\083\000\027\000\028\000\025\000\001\000\002\000\053\000\
\053\000\027\000\028\000\025\000\087\000\088\000\089\000\010\000\
\027\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\084\000\010\000\010\000\010\000\
\010\000\010\000\010\000\049\000\054\000\049\000\010\000\010\000\
\090\000\091\000\092\000\093\000\010\000\010\000\010\000\010\000\
\015\000\048\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\045\000\015\000\015\000\
\054\000\055\000\015\000\015\000\056\000\057\000\114\000\015\000\
\015\000\016\000\058\000\042\000\043\000\015\000\019\000\015\000\
\015\000\016\000\000\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
\016\000\000\000\000\000\016\000\016\000\052\000\053\000\000\000\
\016\000\016\000\000\000\000\000\000\000\000\000\016\000\000\000\
\016\000\016\000\017\000\000\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\000\000\
\017\000\017\000\000\000\000\000\017\000\017\000\000\000\000\000\
\000\000\017\000\017\000\000\000\000\000\000\000\000\000\017\000\
\000\000\017\000\017\000\018\000\000\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\000\000\018\000\018\000\000\000\000\000\018\000\018\000\000\000\
\000\000\059\000\018\000\018\000\000\000\000\000\000\000\000\000\
\018\000\000\000\018\000\018\000\019\000\000\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\000\000\019\000\019\000\000\000\000\000\019\000\019\000\
\000\000\000\000\000\000\019\000\019\000\000\000\000\000\000\000\
\000\000\019\000\000\000\019\000\019\000\020\000\000\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\042\000\000\000\000\000\000\000\000\000\000\000\
\042\000\000\000\042\000\042\000\020\000\000\000\000\000\000\000\
\000\000\000\000\020\000\000\000\020\000\020\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\109\000\110\000\111\000\
\112\000\113\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000"

let yycheck = "\010\001\
\000\000\001\001\002\001\003\001\004\001\005\001\015\000\003\001\
\001\001\002\001\003\001\004\001\005\001\009\001\003\001\001\001\
\002\001\003\001\004\001\005\001\026\000\046\001\001\001\002\001\
\003\001\004\001\005\001\009\001\041\001\006\001\041\001\008\001\
\006\001\042\000\043\000\039\001\047\001\050\001\049\001\050\001\
\010\001\001\000\002\000\049\000\050\000\047\001\046\001\007\000\
\048\001\049\001\006\001\051\001\052\001\046\001\006\001\048\001\
\008\001\047\001\051\001\052\001\046\001\001\000\002\000\040\001\
\041\001\051\001\052\001\046\001\051\000\052\000\053\000\010\001\
\051\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\049\001\032\001\033\001\034\001\
\035\001\036\001\037\001\047\001\006\001\049\001\041\001\042\001\
\054\000\055\000\056\000\057\000\047\001\002\000\049\001\050\001\
\010\001\028\000\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\025\000\032\001\033\001\
\032\001\033\001\036\001\037\001\036\001\037\001\078\000\041\001\
\042\001\007\000\042\001\040\001\041\001\047\001\010\000\049\001\
\050\001\010\001\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\255\255\032\001\
\033\001\255\255\255\255\036\001\037\001\034\001\035\001\255\255\
\041\001\042\001\255\255\255\255\255\255\255\255\047\001\255\255\
\049\001\050\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\255\255\
\032\001\033\001\255\255\255\255\036\001\037\001\255\255\255\255\
\255\255\041\001\042\001\255\255\255\255\255\255\255\255\047\001\
\255\255\049\001\050\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\255\255\255\255\036\001\037\001\255\255\
\255\255\009\001\041\001\042\001\255\255\255\255\255\255\255\255\
\047\001\255\255\049\001\050\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\033\001\255\255\255\255\036\001\037\001\
\255\255\255\255\255\255\041\001\042\001\255\255\255\255\255\255\
\255\255\047\001\255\255\049\001\050\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\041\001\255\255\255\255\255\255\255\255\255\255\
\047\001\255\255\049\001\050\001\041\001\255\255\255\255\255\255\
\255\255\255\255\047\001\255\255\049\001\050\001\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001"

let yynames_const = "\
  DOT\000\
  DOUBLEDOT\000\
  COLONHYPHEN\000\
  QUESTIONHYPHEN\000\
  ARROW\000\
  NOT\000\
  TERM_EQ\000\
  TERM_INEQ\000\
  IS\000\
  AS\000\
  TERM_DECOMP\000\
  TERM_UNIFY\000\
  TERM_NOTUNIFY\000\
  ARITH_EQ\000\
  ARITH_INEQ\000\
  ARITH_LESS\000\
  ARITH_GREATER\000\
  ARITH_GEQ\000\
  ARITH_LEQ\000\
  TERM_ORDER_EQ\000\
  TERM_ORDER_INEQ\000\
  TERM_ORDER_GREATER\000\
  TERM_ORDER_LESS\000\
  TERM_ORDER_GEQ\000\
  TERM_ORDER_LEQ\000\
  DOUBLECOLON\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  BITWISE_AND\000\
  BITWISE_OR\000\
  BITWISE_NOT\000\
  VAR_INSTANTIATED\000\
  SEMICOLON\000\
  COMMA\000\
  COLON\000\
  UMINUS\000\
  UPLUS\000\
  CUT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  PIPE\000\
  EOF\000\
  DOLLAR\000\
  "

let yynames_block = "\
  STRING\000\
  VARIABLE\000\
  NAME\000\
  FLOAT\000\
  INT\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
                ( Var _1 )
# 376 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
             ( ConstTerm(IntConst _1) )
# 383 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 45 "parser.mly"
              ( ConstTerm(FloatConst _1) )
# 390 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
               ( ConstTerm(StringConst _1) )
# 397 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 47 "parser.mly"
             ( ConstTerm(BoolConst _1) )
# 404 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
             ( ConstTerm(StringConst _1) )
# 411 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 49 "parser.mly"
                         ( _2 )
# 418 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_term) in
    Obj.repr(
# 52 "parser.mly"
                 ( _1 )
# 425 "parser.ml"
               : 'compound_term_1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_term) in
    Obj.repr(
# 53 "parser.mly"
                       ( CompoundTerm ("$",[_2]))
# 432 "parser.ml"
               : 'compound_term_1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_1) in
    Obj.repr(
# 56 "parser.mly"
                   (_1)
# 439 "parser.ml"
               : 'compound_term_200))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_200) in
    Obj.repr(
# 57 "parser.mly"
                                                      ( CompoundTerm ("^",[_1;_3]))
# 447 "parser.ml"
               : 'compound_term_200))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_200) in
    Obj.repr(
# 60 "parser.mly"
                     (_1)
# 454 "parser.ml"
               : 'compound_term_400))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_400) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_200) in
    Obj.repr(
# 61 "parser.mly"
                                            ( CompoundTerm ("*",[_1;_3]))
# 462 "parser.ml"
               : 'compound_term_400))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_400) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_200) in
    Obj.repr(
# 62 "parser.mly"
                                            ( CompoundTerm ("/",[_1;_3]))
# 470 "parser.ml"
               : 'compound_term_400))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_400) in
    Obj.repr(
# 65 "parser.mly"
                     (_1)
# 477 "parser.ml"
               : 'compound_term_500))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_500) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_400) in
    Obj.repr(
# 66 "parser.mly"
                                            ( CompoundTerm ("+",[_1;_3]))
# 485 "parser.ml"
               : 'compound_term_500))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_500) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_400) in
    Obj.repr(
# 67 "parser.mly"
                                             ( CompoundTerm ("-",[_1;_3]))
# 493 "parser.ml"
               : 'compound_term_500))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_500) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_400) in
    Obj.repr(
# 68 "parser.mly"
                                                    ( CompoundTerm ("\\/",[_1;_3]))
# 501 "parser.ml"
               : 'compound_term_500))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_500) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_400) in
    Obj.repr(
# 69 "parser.mly"
                                                   ( CompoundTerm ("/\\",[_1;_3]))
# 509 "parser.ml"
               : 'compound_term_500))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_500) in
    Obj.repr(
# 72 "parser.mly"
                     (_1)
# 516 "parser.ml"
               : 'compound_term_600))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_500) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 73 "parser.mly"
                                             ( CompoundTerm (":",[_1;_3]))
# 524 "parser.ml"
               : 'compound_term_600))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 76 "parser.mly"
                     (_1)
# 531 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 77 "parser.mly"
                                                   ( CompoundTerm ("=:=",[_1;_3]))
# 539 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 78 "parser.mly"
                                                     ( CompoundTerm ("=\\=",[_1;_3]))
# 547 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 79 "parser.mly"
                                                    ( CompoundTerm (">=",[_1;_3]))
# 555 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 80 "parser.mly"
                                                    ( CompoundTerm ("=<",[_1;_3]))
# 563 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 81 "parser.mly"
                                                       ( CompoundTerm (">",[_1;_3]))
# 571 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 82 "parser.mly"
                                                     ( CompoundTerm ("<",[_1;_3]))
# 579 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 84 "parser.mly"
                                                     ( CompoundTerm ("=",[_1;_3]))
# 587 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 85 "parser.mly"
                                                       ( CompoundTerm ("\\=",[_1;_3]))
# 595 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 86 "parser.mly"
                                                   ( CompoundTerm ("==",[_1;_3]))
# 603 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 87 "parser.mly"
                                                    ( CompoundTerm ("\\==",[_1;_3]))
# 611 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 88 "parser.mly"
                                               ( CompoundTerm ("is",[_1;_3]))
# 619 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 89 "parser.mly"
                                               ( CompoundTerm ("as",[_1;_3]))
# 627 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 90 "parser.mly"
                                                      ( CompoundTerm ("=..",[_1;_3]))
# 635 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 92 "parser.mly"
                                                       ( CompoundTerm ("@>=",[_1;_3]))
# 643 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 93 "parser.mly"
                                                        ( CompoundTerm ("@=<",[_1;_3]))
# 651 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 94 "parser.mly"
                                                           ( CompoundTerm ("@>",[_1;_3]))
# 659 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 95 "parser.mly"
                                                         ( CompoundTerm ("@<",[_1;_3]))
# 667 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 96 "parser.mly"
                                                       ( CompoundTerm ("=@=",[_1;_3]))
# 675 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_600) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_600) in
    Obj.repr(
# 97 "parser.mly"
                                                         ( CompoundTerm ("\\=@=",[_1;_3]))
# 683 "parser.ml"
               : 'compound_term_700))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_700) in
    Obj.repr(
# 100 "parser.mly"
                     (_1)
# 690 "parser.ml"
               : 'compound_term_1050))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_700) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_1050) in
    Obj.repr(
# 101 "parser.mly"
                                              ( CompoundTerm ("->",[_1;_3]))
# 698 "parser.ml"
               : 'compound_term_1050))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_1050) in
    Obj.repr(
# 105 "parser.mly"
                      (_1)
# 705 "parser.ml"
               : 'compound_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                        ( ListTerm [] )
# 711 "parser.ml"
               : 'list_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 109 "parser.mly"
                               ( ListTerm _2 )
# 718 "parser.ml"
               : 'list_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_term) in
    Obj.repr(
# 112 "parser.mly"
                ( _1 )
# 725 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term) in
    Obj.repr(
# 113 "parser.mly"
                   ( _1 )
# 732 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 116 "parser.mly"
            ( [_1] )
# 739 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 117 "parser.mly"
                         ( _1::_3 )
# 747 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 118 "parser.mly"
                        ( _1::_3 )
# 755 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 121 "parser.mly"
                                   ( Predicate (_1,_3) )
# 763 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
              ( Identifier _1 )
# 770 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 125 "parser.mly"
                   ( ([_1],[]) )
# 777 "parser.ml"
               : 'predicate_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'predicate_list) in
    Obj.repr(
# 126 "parser.mly"
                                   ( (_1::_3, ","::snd(_3)) )
# 785 "parser.ml"
               : 'predicate_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'predicate_list) in
    Obj.repr(
# 127 "parser.mly"
                                     ( (_1::_3, ";"::snd(_3)) )
# 793 "parser.ml"
               : 'predicate_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    Obj.repr(
# 130 "parser.mly"
                       ( Fact _1 )
# 800 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'predicate_list) in
    Obj.repr(
# 131 "parser.mly"
                                            ( Rule (_1,_3) )
# 808 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 134 "parser.mly"
             ( [_1] )
# 815 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 135 "parser.mly"
                      ( _1::_2 )
# 823 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'predicate_list) in
    Obj.repr(
# 138 "parser.mly"
                                     ( Query _2 )
# 830 "parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 141 "parser.mly"
                ( RuleList _1 )
# 837 "parser.ml"
               : ProjCommon.rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ProjCommon.rules) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'query) in
    Obj.repr(
# 144 "parser.mly"
                ( Prog (_1,_2) )
# 845 "parser.ml"
               : ProjCommon.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'query) in
    Obj.repr(
# 145 "parser.mly"
            ( ProgFromQuery _1 )
# 852 "parser.ml"
               : ProjCommon.program))
(* Entry rules *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry program *)
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
let rules (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ProjCommon.rules)
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : ProjCommon.program)
