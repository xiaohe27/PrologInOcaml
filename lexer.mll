{
open ProjCommon;;
open Parser;;

exception EndInput;;
}

(* definitions section *)
let capital = ['A'-'Z'] (* capital letters *)
let small = ['a'-'z'] (* small letters *)
let digit = ['0'-'9']
let underline = ['_'] (* underline character *)
let alpha = capital | small | digit | underline (* any alphanumeric character*)
let word = small alpha* (* prolog words *)
let quoted_name = '\'' [^ '\''] '\'' (* quoted names *)
let symbol = ['+' '-' '*' '/' '\\' '^' '<' '>' '=' '~' ':' '?' '@' '#' '$' '&']
let solo_char = ['!' ';' '.' '[' ']' '(' ')' ',' '|']
let name = quoted_name | word | symbol+ | solo_char (* valid prolog names *)
let variable = (capital | underline) alpha* (* prolog variables *)

 
let int = digit+
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = int frac? exp?

let whitespace = [' ' '\t' '\n']
let open_comment = "(*"
let close_comment = "*)"

rule token = parse
| eof							{ EOF }
| whitespace					{ token lexbuf }
|"not"                          { NOT } (* boolean negation *)
|"=:="                          { ARITH_EQ } (* arithmetical equality *)
|"=\\="                         { ARITH_INEQ } (* arithmetical inequality *)
|"->"                           { ARROW } (* if then [else] *)
|"\\="                          { TERM_NOTUNIFY } (* terms do not unify *)
|"=.."                          { TERM_DECOMP } (* term composition/decomposition *)
|"=="                           { TERM_EQ } (* term equality *)
|"\\=="                         { TERM_INEQ } (* term inequality *)
|"@=<"                          { TERM_ORDER_LEQ } (* term less or equal to (order of terms) *)
|"@>="                          { TERM_ORDER_GEQ } (* term greater or equal to (order of terms) *)
|"=@="                          { TERM_ORDER_EQ }  (* term equality (order of terms) *)
|"\\=@="                        { TERM_ORDER_INEQ } (* term inequality (order of terms) *)
|"@<"                           { TERM_ORDER_LESS } (* term less than (order of terms) *)
|"@>"                           { TERM_ORDER_GREATER } (* term greater than (order of terms) *)
|">="                           { ARITH_GEQ } (* arithmetical greater or equal to *)
|"=<"                           { ARITH_LEQ } (* arithmetical less or equal to *)
|"is"                           { IS } (* variable instantiation *)
|"::"                           { DOUBLECOLON } (* module(database) specifier *)
|"\\/"                          { BITWISE_AND } (* bitwise and *)
|"/\\"                          { BITWISE_OR } (* bitwise or *)
|"\\"                           { BITWISE_NOT } (* bitwise not *)
|"^"                            { VAR_INSTANTIATED } (* is variable instantiated? *)
|"+"                            { PLUS } (* arithmetical plus *)
|"-"                            { MINUS } (* arithmetical minus *)
|"*"                            { MULT } (* arithmetical multiplication *)
|"/"                            { DIV } (* arithmetical division *)
|"("                            { LPAREN } (* left parenthesis *)
|")"                            { RPAREN } (* right parenthesis *)
|":"                            { COLON } (* else *)
|","                            { COMMA } (* logical and *)
|";"                            { SEMICOLON } (* logical or *)
|"="                            { TERM_UNIFY } (* unify terms *)
|"<"                            { ARITH_LESS } (* arithmetical less than *)
|">"                            { ARITH_GREATER } (* arithmetical greater than *)
|"!"                            { CUT } (* cut operator *)
|":-"                           { COLONHYPHEN } (* logical implication *)
|"?-"                           { QUESTIONHYPHEN } 
|"["                            { LBRACKET } (* left bracket for lists *)
|"]"                            { RBRACKET } (* right bracket for lists *)
|"|"                            { PIPE } (* head-tail delimiter for lists *)
| ".."				{ DOUBLEDOT }
| "."				{ DOT }
|"%"[^'\n']* 			{ token lexbuf }
| open_comment			{ comment 1 lexbuf }
| close_comment 		{ raise (Failure "unmatched closed comment") }
| '"' 				{stringToken "" lexbuf}
| "'" 				{singleStringToken "'" lexbuf}
| name as id			{ NAME (id) }
| int      			{ INT (int_of_string (Lexing.lexeme lexbuf)) }
| float  			{ FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| variable			{ VARIABLE (Lexing.lexeme lexbuf) }

and comment depth = parse
open_comment		{ comment (depth+1) lexbuf }
| close_comment		{ if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
| eof				{ raise (Failure "unmatched open comment") }
| _					{ comment depth lexbuf }

and stringToken content = parse
  | eof {raise (Failure "unexpected end of file")}

  | '"' {STRING content}

  | "\\\\" {let newContent= content ^ "\\" in stringToken newContent lexbuf}

  | "\\'" {let newContent= content ^ "'" in stringToken newContent lexbuf}

  | "\\\"" {let newContent= content ^ "\"" in stringToken newContent lexbuf}

  | "\\t" {let newContent= content ^ "\t" in stringToken newContent lexbuf}

  | ("\\n")[' ' '\t']* {let newContent= content ^ "\n " in stringToken newContent lexbuf}

  | "\\r" {let newContent= content ^ "\r" in stringToken newContent lexbuf}

  | "\\b" {let newContent= content ^ "\b" in stringToken newContent lexbuf}

  | "\\ " {let newContent= content ^ "\ " in stringToken newContent lexbuf}

  | "\\" (digit as x) (digit as y) (digit as z) {let n= 
		let x=String.make 1 x in let y=String.make 1 y in let z=String.make 1 z in
		 (int_of_string z) + 10*(int_of_string y)
		 + 100*(int_of_string x) in if n > 255 then 
				raise (Failure "unknown char") else
	   let newContent= content ^ (String.make 1 (char_of_int n))
	       in stringToken newContent lexbuf } 
	       
  | [^'"']+ as pstr {let newContent= content ^ pstr in stringToken newContent lexbuf}

and singleStringToken content = parse
  | eof {raise (Failure "unexpected end of file")}

  | "'" {STRING (content ^ "'") }

  | "\\\\" {let newContent= content ^ "\\" in singleStringToken newContent lexbuf}

  | "\\'" {let newContent= content ^ "'" in singleStringToken newContent lexbuf}

  | "\\\"" {let newContent= content ^ "\"" in singleStringToken newContent lexbuf}

  | "\\t" {let newContent= content ^ "\t" in singleStringToken newContent lexbuf}

  | ("\\n")[' ' '\t']* {let newContent= content ^ "\n " in singleStringToken newContent lexbuf}

  | "\\r" {let newContent= content ^ "\r" in singleStringToken newContent lexbuf}

  | "\\b" {let newContent= content ^ "\b" in singleStringToken newContent lexbuf}

  | "\\ " {let newContent= content ^ "\ " in singleStringToken newContent lexbuf}

  | "\\" (digit as x) (digit as y) (digit as z) {let n= 
		let x=String.make 1 x in let y=String.make 1 y in let z=String.make 1 z in
		 (int_of_string z) + 10*(int_of_string y)
		 + 100*(int_of_string x) in if n > 255 then 
				raise (Failure "unknown char") else
	   let newContent= content ^ (String.make 1 (char_of_int n))
	       in singleStringToken newContent lexbuf } 
	       
  | [^''']+ as pstr {let newContent= content ^ pstr in singleStringToken newContent lexbuf}


{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () = 
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }



