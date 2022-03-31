open Ast

type sexpr = typ * sx 
and sx = 
	| SVar of string
	| SStringLit of string
	| SFloatLit of float
	| SIntLit of int
	| SBoolLit of bool
	| SId of string
	(* | SArrayLit of sexpr list *)
	| SBinop of sexpr * operator * sexpr
	| SNot of sexpr
	| SAssignOp of sexpr * sexpr
	| SCall of string * sexpr list

type sstmt = 
	SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	| SFor of sstmt * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt
	| SVarDecl of typ * string * sexpr
	| SVarInitial of typ * string
	| SReturn of sexpr
	| SPrint of sexpr

type sfunc_decl = {
	styp : typ;
	sfname : string;
	sformals : bind list;
	sfstmts : sstmt list;
}

type sprogram = sfunc_decl list

(* pretty printing function*)

let rec string_of_sexpr (sex:sexpr) = match snd sex with 
	| SIntLit(i) -> string_of_int i
	| SFloatLit(f) -> string_of_float f
	| SBoolLit(b) -> string_of_bool b
	| SStringLit(s) -> s
	| SBinop(e1, o, e2) -> 
		string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
	| SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
	| SId(s) -> s
	| SAssignOp(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
	| _ -> "NOT FOUND"

let string_of_svdecl = function
	VarInitial(t, id) -> string_of_typ t ^ " " ^ id
	| VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = "
	| _ -> raise (Failure "vdecl not supported")


let rec string_of_sstmt = function
	SBlock(stmts) ->
		"{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
	| SExpr(expr) -> string_of_sexpr expr ^ ";\n";
	| SVarInitial(t, s1) -> string_of_typ t ^" " ^s1 ^ ";\n"
	| SVarDecl(t, s1, e1) -> string_of_typ t ^" " ^s1 ^ " = " ^ string_of_sexpr e1 ^ ";\n"
	| SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
	| SFor(e1, e2, e3, s) ->
		"for (" ^ string_of_sstmt e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
	| SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
	| SReturn(e) -> "return " ^ string_of_sexpr e
	| _ -> raise (Failure "unsupported expr to string")

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sfstmts) ^
  "}\n"


let string_of_sprogram  (funcs) =
  String.concat "\n" (List.map string_of_sfdecl funcs) ^ "\n" 
  (* String.concat "\n" (List.map string_of_sfdecl funcs) *)
