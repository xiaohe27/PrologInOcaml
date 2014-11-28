{
open Mp8common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let numerics = numeric +
let opnumerics = numeric *
let lowercase = ['a' - 'z']
let letter = ['a' - 'z' 'A' - 'Z' '_']
let opletters = letter *
let open_comment = "(*"
let close_comment = "*)"
let hex = numeric| ['a' - 'f']

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF } 
  |'~'		{NEG}
  |'+'		{PLUS}
  |'-'		{MINUS}
  |'*'		{TIMES}
  |'/'		{DIV}
  |"+."		{DPLUS}
  |"-."		{DMINUS}
  |"*."		{DTIMES}
  |"/."		{DDIV}
  |'^'		{CARAT}
  |'<'		{LT}
  |'>'		{GT}
  |"<="		{LEQ}
  |">="		{GEQ}
  |'='		{EQUALS}
  |"<>"		{NEQ}
  |'|'		{PIPE}
  |"->"		{ARROW}
  |';'		{SEMI}
  |";;"		{DSEMI}
  |"::"		{DCOLON}
  |'@'		{AT}
  |"[]"		{NIL}
  |"let"	{LET}
  |"rec"	{REC}
  |"and"	{AND}
  |"end"	{END}
  |"in"		{IN}
  |"if"		{IF}
  |"then"	{THEN}
  |"else"	{ELSE}
  |"fun"	{FUN}
  |"mod"	{MOD}
  |"raise"	{RAISE}
  |"try"	{TRY}
  |"with"	{WITH}
  |"not"	{NOT}
  |"&&"		{LOGICALAND}
  |"||"		{LOGICALOR}
  |'['		{LBRAC}
  |']'		{RBRAC}
  |'('		{LPAREN}
  |')'		{RPAREN}
  |','		{COMMA}
  |'_'		{UNDERSCORE}
  |numerics as n	{ INT (int_of_string n) }
  |"0x" (hex +) as n		{ INT (int_of_string n) }
  |(numerics) '.' opnumerics as f	{ FLOAT (float_of_string f) }
  |(numerics) '.' opnumerics 'e' numerics as f		{ FLOAT (float_of_string f) }

  |"true" as t		{BOOL (bool_of_string t)}
  |"false" as f		{BOOL (bool_of_string f)}
  |"()"			{UNIT}
  |(lowercase)(numerics|letter)* as i {IDENT i}
  |"//"[^'\n']* 		{ token lexbuf }
  | open_comment		{ comment 1 lexbuf }
  | close_comment 		{ raise (Failure "unmatched closed comment") }
  |'"'			 	{ stringcompo "" lexbuf }

and comment depth = parse
open_comment	{ comment (depth+1) lexbuf }
| close_comment { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
| eof		{ raise (Failure "unmatched open comment") }
| _		{ comment depth lexbuf }

and stringcompo str = parse
'"'		{STRING str}
|"\\\\"		{stringcompo (str^"\\") lexbuf }
|"\\'"		{stringcompo (str^"\'") lexbuf }
|"\\\""		{stringcompo (str^"\"") lexbuf }
|"\\t"		{stringcompo (str^"\t") lexbuf }
|"\\n"[' ' '\t']*		{stringcompo (str^"\n ") lexbuf }
|"\\r"		{stringcompo (str^"\r") lexbuf }
|"\\b"		{stringcompo (str^"\b") lexbuf }
|"\\ "		{stringcompo (str^" ") lexbuf }
| eof		{ raise (Failure "unmatched open quote") }
|"\\"((numeric)(numeric)(numeric) as n)		{ stringcompo (str^(String.make 1 (char_of_int (int_of_string n)))) lexbuf }
|_ as s		{stringcompo (str^(String.make 1 s)) lexbuf }
 

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

