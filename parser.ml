type token =
  | STRING of (string)
  | VARIABLE of (string)
  | NAME of (string)
  | UNSIGNEDFLOAT of (float)
  | UNSIGNEDINTEGER of (int)
  | SIGNEDFLOAT of (float)
  | SIGNEDINTEGER of (int)
  | DOT
  | DOUBLEDOT
  | COLONHYPHEN
  | ARROW
  | NOT
  | TERM_EQ
  | TERM_INEQ
  | IS
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
  | UNIT

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

        open projCommon
# 62 "parser.ml"
let yytransl_const = [|
  264 (* DOT *);
  265 (* DOUBLEDOT *);
  266 (* COLONHYPHEN *);
  267 (* ARROW *);
  268 (* NOT *);
  269 (* TERM_EQ *);
  270 (* TERM_INEQ *);
  271 (* IS *);
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
  308 (* UNIT *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* VARIABLE *);
  259 (* NAME *);
  260 (* UNSIGNEDFLOAT *);
  261 (* UNSIGNEDINTEGER *);
  262 (* SIGNEDFLOAT *);
  263 (* SIGNEDINTEGER *);
  307 (* BOOL *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\004\000\004\000\004\000\005\000\005\000\
\005\000\006\000\007\000\007\000\008\000\008\000\001\000\001\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\002\000\002\000\001\000\002\000\002\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\003\000\
\003\000\001\000\002\000\003\000\001\000\003\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\001\000\006\000\003\000\000\000\000\000\
\000\000\010\000\002\000\025\000\011\000\012\000\000\000\000\000\
\024\000\023\000\008\000\005\000\007\000\004\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\013\000\
\014\000\000\000\000\000\022\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\024\000\018\000\025\000"

let yysindex = "\255\255\
\007\255\000\000\000\000\000\000\000\000\000\000\019\255\021\255\
\012\255\000\000\000\000\000\000\000\000\000\000\249\254\253\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\208\254\
\211\254\017\255\017\255\017\255\017\255\017\255\000\000\000\000\
\000\000\249\254\249\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\007\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\213\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\005\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\006\000\008\000\000\000\009\000\000\000\241\255"

let yytablesize = 313
let yytable = "\001\000\
\015\000\030\000\016\000\031\000\017\000\021\000\018\000\003\000\
\004\000\017\000\005\000\006\000\003\000\004\000\036\000\005\000\
\006\000\003\000\004\000\000\000\005\000\006\000\019\000\020\000\
\021\000\022\000\026\000\027\000\028\000\029\000\000\000\032\000\
\033\000\000\000\000\000\034\000\035\000\000\000\007\000\008\000\
\000\000\000\000\000\000\007\000\008\000\000\000\000\000\000\000\
\007\000\008\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\010\000\011\000\000\000\023\000\000\000\010\000\011\000\
\000\000\000\000\000\000\010\000\011\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\015\000\016\000\016\000\017\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\015\000\016\000\016\000\017\000\017\000\018\000\
\018\000"

let yycheck = "\001\000\
\000\000\050\001\000\000\049\001\000\000\049\001\000\000\001\001\
\002\001\001\000\004\001\005\001\001\001\002\001\030\000\004\001\
\005\001\001\001\002\001\255\255\004\001\005\001\004\001\005\001\
\004\001\005\001\034\001\035\001\032\001\033\001\255\255\026\000\
\027\000\255\255\255\255\028\000\029\000\255\255\032\001\033\001\
\255\255\255\255\255\255\032\001\033\001\255\255\255\255\255\255\
\032\001\033\001\255\255\255\255\255\255\255\255\048\001\255\255\
\255\255\051\001\052\001\255\255\049\001\255\255\051\001\052\001\
\255\255\255\255\255\255\051\001\052\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001\033\001\032\001\033\001\032\001\033\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\049\001\050\001\049\001\050\001\049\001\050\001\049\001\
\050\001"

let yynames_const = "\
  DOT\000\
  DOUBLEDOT\000\
  COLONHYPHEN\000\
  ARROW\000\
  NOT\000\
  TERM_EQ\000\
  TERM_INEQ\000\
  IS\000\
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
  UNIT\000\
  "

let yynames_block = "\
  STRING\000\
  VARIABLE\000\
  NAME\000\
  UNSIGNEDFLOAT\000\
  UNSIGNEDINTEGER\000\
  SIGNEDFLOAT\000\
  SIGNEDINTEGER\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
               ( Var _1 )
# 312 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
           ( ConstTerm(UnitConst) )
# 318 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
                     ( ConstTerm(IntConst _1) )
# 325 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 47 "parser.mly"
                          ( ConstTerm(IntConst (-_2)) )
# 332 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "parser.mly"
                         ( ConstTerm(IntConst (_2)) )
# 339 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 49 "parser.mly"
                   ( ConstTerm(FloatConst _1) )
# 346 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 50 "parser.mly"
                         ( ConstTerm(FloatConst (-_2)) )
# 353 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 51 "parser.mly"
                        ( ConstTerm(FloatConst (_2)) )
# 360 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
             ( ConstTerm(StringConst _1) )
# 367 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 53 "parser.mly"
           ( ConstTerm(BoolConst _1) )
# 374 "parser.ml"
               : 'atomic_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_term) in
    Obj.repr(
# 56 "parser.mly"
                 ( _1 )
# 381 "parser.ml"
               : 'compound_term_1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_1) in
    Obj.repr(
# 59 "parser.mly"
                   (_1)
# 388 "parser.ml"
               : 'compound_term_2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_1) in
    Obj.repr(
# 60 "parser.mly"
                                        ( CompoundTerm ("*",[_1;_3]))
# 396 "parser.ml"
               : 'compound_term_2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_1) in
    Obj.repr(
# 61 "parser.mly"
                                       ( CompoundTerm ("/",[_1;_3]))
# 404 "parser.ml"
               : 'compound_term_2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_2) in
    Obj.repr(
# 64 "parser.mly"
                   (_1)
# 411 "parser.ml"
               : 'compound_term_3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_2) in
    Obj.repr(
# 65 "parser.mly"
                                        ( CompoundTerm ("+",[_1;_3]))
# 419 "parser.ml"
               : 'compound_term_3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term_3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_2) in
    Obj.repr(
# 66 "parser.mly"
                                         ( CompoundTerm ("-",[_1;_3]))
# 427 "parser.ml"
               : 'compound_term_3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term_3) in
    Obj.repr(
# 69 "parser.mly"
                   (_1)
# 434 "parser.ml"
               : 'compound_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                      ( ListTerm [] )
# 440 "parser.ml"
               : 'list_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_body) in
    Obj.repr(
# 73 "parser.mly"
                               ( ListTerm _2 )
# 447 "parser.ml"
               : 'list_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term) in
    Obj.repr(
# 76 "parser.mly"
                   ( [_1] )
# 454 "parser.ml"
               : 'list_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compound_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_body) in
    Obj.repr(
# 77 "parser.mly"
                                 ( _1::_3 )
# 462 "parser.ml"
               : 'list_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_term) in
    Obj.repr(
# 80 "parser.mly"
                ( _1 )
# 469 "parser.ml"
               : projCommon.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_term) in
    Obj.repr(
# 81 "parser.mly"
                  ( _1 )
# 476 "parser.ml"
               : projCommon.term))
(* Entry term *)
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
let term (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : projCommon.term)
