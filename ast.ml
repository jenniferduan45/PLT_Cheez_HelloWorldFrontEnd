
type operator = Add | Sub | Mul | Div | Sep | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type assignment = Assign

type typ = 
	  Int 
	| String 
	| Float 
	| Bool 
	| Void 
(* 	| ArrayType of typ   *)     

type expr =
	  StringLit of string
	| FloatLit of float
	| IntLit of int
	| BoolLit of bool
	| Id of string
(* 	| ArrayLit of expr list
	| ArrayIndex of expr * expr *)
	| Binop of expr * operator * expr
	| Not of expr
	| AssignOp of expr * expr
	| Call of string * expr list

type stmt = 
	  Block of stmt list
	| Expr of expr
	| If of expr * stmt * stmt
	| For of stmt * expr * expr * stmt
	| While of expr * stmt
	| VarDecl of typ * string * expr
	| VarInitial of typ * string
(* 	| ArrDecl of typ * string * expr *)
	| Return of expr
	| Print of expr

type bind = typ * string

type fdecl = {
		typ : typ;
		fname : string;
		formals : bind list;
		fstmts : stmt list;
	}

type program = fdecl list

let rec string_of_typ = function
	Int -> "int"
	| Bool -> "bool"
	| Float -> "float"
	| Void -> "void"
	| String -> "string"
	(* | _ -> "@TODO" *)
	

let string_of_op = function
	Add -> "+"
	| Sub -> "-"
	| Mul -> "*"
	| Div -> "/"
	| Eq -> "=="
	| Neq -> "!="
	| Lt -> "<"
	| Leq -> "<="
	| Gt -> ">"
	| Geq -> ">="
	| And -> "&&"
	| Or -> "||"
	| Sep -> ";"

let rec string_of_expr = function
	| IntLit(i) -> string_of_int i
	| StringLit(s) -> s
	| Call(f, el) ->
			f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| Binop(e1, o, e2) ->
		string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
	| FloatLit(f) -> string_of_float f
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| Id(s) -> s
	| AssignOp(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
	| _ -> "no expression matched*******"

let string_of_vdecl = function
	VarInitial(t, id) -> string_of_typ t ^ " " ^ id
	| VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e
	| _ -> raise (Failure "err in string_of_vdecl in ast.ml liine 83")

let rec string_of_stmt = function
	Block(stmts) 		-> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) 		-> string_of_expr expr ^ ";\n";
	| VarInitial(t, s1) ->  string_of_typ t ^" " ^s1 ^ ";\n" 
	| VarDecl(t, s1, e1) 		-> string_of_typ t ^" " ^s1 ^ " = " ^ string_of_expr e1 ^ ";\n"
	| If(e, s1, s2) 	->  "if (" ^ string_of_expr e ^ ")\n" ^ 
							string_of_stmt s1  ^ 
							"else\n" ^ string_of_stmt s2
	| For(e1, e2, e3, s) ->
			"for (" ^ string_of_stmt e1  ^ " ; " ^ string_of_expr e2 ^
			" ; " ^ string_of_expr e3  ^ ") " ^
			string_of_stmt s
	| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
	| Return(e) -> "return " ^ string_of_expr e
	| _ -> "Statement Not Matched??"


let string_of_fdecl fdecl =
	string_of_typ fdecl.typ ^ " " ^
	fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
	")\n{\n" ^
	String.concat "" (List.map string_of_stmt fdecl.fstmts) ^
	"}\n"


let string_of_program (funcs) =
	String.concat "\n" (List.map string_of_fdecl funcs)
