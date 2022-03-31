(* Semantic checking for the Cheez compiler. Modeled from MicroC. *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (functions) =
  
	(*build in fucntions*)
	let built_in_decls = StringMap.add "print"
		{	
			typ = Void; 
			fname = "print"; 
			formals = [(Int, "args")];  
			fstmts = [] 
		} 
		StringMap.empty;
 	 in
	(* Add function name to symbol table *)
	let add_func map fd = 
		let built_in_err = "function " ^ fd.fname ^ "may not be redefined"
		and dup_err = "duplicate function " ^ fd.fname
		and raise_err err = raise (Failure err)
		and n = fd.fname 
		in match fd with
			| _ when StringMap.mem n built_in_decls -> raise_err built_in_err
			| _ when StringMap.mem n map -> raise_err dup_err  
			| _ ->  StringMap.add n fd map 
	in
	(* Collect all other function names into one symbol table *)
	let function_decls = List.fold_left add_func built_in_decls functions
	in

	(* Return a function from our symbol table *)
	let find_func s = 
	try StringMap.find s function_decls
	with Not_found -> raise (Failure ("unrecognized function " ^ s))
	in
	let _ = find_func "main" in (* Ensure "main" is defined *)
	let check_function func =
		let add_var map (tp, name, len) = 
			let dup_err = "Variable: " ^ name ^" is a duplicate." in
			match (tp, name) with
			_ when StringMap.mem name map -> raise (Failure dup_err)
			| _ -> StringMap.add name (tp, name, len) map
		in
		let check_assign lvaluet rvaluet err =
			if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in 
		let type_of_identifier s symbols =  
			let (ty, _, _) = try StringMap.find s symbols with Not_found -> raise( Failure("ID not found: " ^ s)) 
		in ty in
		let rec check_expr map e = match e with
			IntLit  l 	-> (Int, SIntLit l, map)
		| FloatLit l 	-> (Float, SFloatLit l, map)
		| BoolLit l  	-> (Bool, SBoolLit l, map)
		| StringLit l -> (String, SStringLit l, map)
		| Id s       	-> (type_of_identifier s map, SId s, map)
		(* | ArrayLit l -> let array_ele = List.map (check_expr map) l in
						let rec type_consist = function
					        [] -> ()
					      |	((n1,_,_) :: (n2,_,_) :: _) when n1 != n2 ->
					        raise (Failure ("Type inconsistent " ^ n1 ^ " " ^ n2))
					      | _ :: t -> type_consist t
					    in type_consist array_ele *)
		| AssignOp(v, e)	-> 
					let lt, vname, map1 = find_name v map "assignment error" in
					let rt, ex, map2 = check_expr map1 e in
					(check_assign lt rt "type miss match", SAssignOp((lt, vname), (rt, ex)), map2)
		| Call(fname, args) as call -> 
					let fd = find_func fname in
					let param_length = List.length fd.formals in
					if List.length args != param_length then
						raise (Failure ("expecting " ^ string_of_int param_length ^ 
										" arguments in " ^ string_of_expr call))
					else let check_call (ft, _) e = 
					let (et, e', map') = check_expr map e in 
					let err = "illegal argument found " ^ string_of_typ et ^
							" expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
					in (check_assign ft ft err, e')
					in 
					let args' = List.map2 check_call fd.formals args
					in 
					(fd.typ, SCall(fname, args'), map)

		| Not(e) as notEx-> let (t, e', map') = check_expr map e in
					if t != Bool then 
						raise (Failure ("expecting bool expression in " ^ string_of_expr notEx))
					else (Bool, SNot((t, e')), map')
		| Binop(e1, op, e2) as ex ->
					let (t1, e1', map') = check_expr map e1 in 
					let (t2, e2', map'') = check_expr map' e2 in
					if t1 = t2 then
					let ty = 
						match t1 with
							_ -> match op with
								| Add | Sub | Mul | Div    when (t1 = Int || t1 = Float) -> t1
								| Add                      when t1 = String -> String
								| Eq | Neq                 -> Bool
								| Lt | Leq | Gt | Geq      when (t1 = Int || t1 = Float) -> Bool
								| And | Or                 when t1 = Bool -> Bool
								| _ -> raise (Failure ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr ex))
							in
					(ty, SBinop((t1, e1'), op, (t2, e2')), map'')
					else
						raise (Failure "binary oprands types do not match.") 

		and find_name (name) map err = 
			match name with
			Id _ -> check_expr map name
			| _ -> raise (Failure ("find name error"))
		in
		let check_bool_expr map e = 
			let (t, e', map') = check_expr map e
			and err = "expected Boolean expression in " ^ string_of_expr e in
			match t with
			| Bool -> (t, e')
			| _ -> raise (Failure (err))
		in


		let rec check_stmt_list map = function
			  [] -> ([], map)
			| Block sl :: sl' -> check_stmt_list map (sl @ sl') (* Flatten blocks *)
			| s :: sl -> let (s1, map1) = check_stmt map s in 
						 let (s2, map2) = check_stmt_list map1 sl in 
						 (s1 :: s2, map2)
		and check_stmt map = function 
			  Block sl -> (SBlock(fst (check_stmt_list map sl)), map)
			| Expr e -> let (typ, sexpr, new_map) = check_expr map e in 
						(SExpr (typ, sexpr), new_map)
			| If(e, st1, st2) -> 
				let sthen, _ = check_stmt map st1 in
				let selse, _ = check_stmt map st2 in
				(SIf(check_bool_expr map e, sthen, selse), map)
			| While(e, stList) -> SWhile(check_bool_expr map e, fst (check_stmt map stList)), map
			| For(e1, e2, e3, stList) -> let (st1, m') = check_stmt map e1 in 
										   let (typ3, s3, m'') = check_expr m' e3 in
										   (SFor(st1, check_bool_expr m'' e2, (typ3, s3), fst (check_stmt m'' stList)), m'')
			| Return e -> let (t, e', map') = check_expr map e in
						  if t = func.typ then (SReturn (t, e'), map')
						  else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
										string_of_typ func.typ ^ " in " ^ string_of_expr e))
			| VarInitial(tp, id) -> let new_map = add_var map (tp, id, 0) in
									(SVarInitial(tp, id), new_map)
			| VarDecl(tp, id, e) -> let (right_ty, sexpr, map') = check_expr map e in
									let new_map = add_var map' (tp, id, 0) in
									let right = (right_ty, sexpr) in
									(SVarDecl(tp, id, right), new_map)
			| _ -> raise (Failure "Match failure")
		in 

		let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty (func.formals)
		in
		{
			styp = func.typ;
			sfname = func.fname;
			sformals = func.formals;
			sfstmts = match fst (check_stmt symbols (Block(func.fstmts))) with
			SBlock(sl) -> sl
			| _ -> let err = "internal error: block didn't become a block?"
			in raise (Failure err)
		}
      
  in 
  let sfunc = List.map check_function functions in
  sfunc
