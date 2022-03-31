{ open Parser }
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = letter (letter|digit)*
let integer = ['-']?digit+
let float = ['-']?digit+['.']digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule tokenize = parse
  white    { tokenize lexbuf }
| newline  { tokenize lexbuf }
| "//" 		{ comment lexbuf }
| '{'  		{ LBRACE }
| '}'  		{ RBRACE }
| '('  		{ LPAREN }
| ')'  		{ RPAREN }
| '['  		{ LBRACKET }
| ']'  		{ RBRACKET }
| ':'     { COLON }
| ';'  		{ SEMI }
| ','  		{ COMMA }
| '.'	    { DOT }
(* operators *)
| '+'  		{ PLUS }
| '-'  		{ MINUS }
| '*'  		{ TIMES }
| '/'  		{ DIVIDE }
| '='  		{ ASSIGN }
| '>'  		{ GT }
| ">=" 		{ GEQ }
| '<'  		{ LT }
| "<="		{ LEQ }
| "==" 		{ EQ }
| "!=" 		{ NEQ }
| "&&" 		{ AND }
| "||" 		{ OR }
| '!'  		{ NOT }
(* data types *)
| "int"		  { INT } 
| "float"  	{ FLOAT }
| "bool"  	{ BOOL }
| "string" 	{ STRING }
| "void"   	{ VOID }
| "True"   	{ BOOLLIT(true) }
| "False"   { BOOLLIT(false) }
| "null"   	{ NULL }
(* control flow *)
| "if"   	  { IF }
| "else"  	{ ELSE }
| "for"   	{ FOR }
| "while" 	{ WHILE }
| "return" 	{ RETURN }
| id as lem { ID(lem) }
| integer as lem { INTLIT(int_of_string lem) }
| float as lem {FLOATLIT(float_of_string lem)}
| "\'" [^''']+ "\'" as lem { STRINGLIT(lem) }
| "\"" [^''']+ "\"" as lem { STRINGLIT(lem) }
| eof { EOF }
| _ as ch { raise (Failure("Unexpected char: " ^ Char.escaped ch)) }

and comment = parse
  newline { tokenize lexbuf }
| _       { comment lexbuf }
