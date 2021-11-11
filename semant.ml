open Ast
open Sast

module StringMap = Map.Make(String)

let check (program) =
  let functions = program.decls.funcs in
  let globals = program.decls.vars
  in
  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	  (Null, b) -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      fname = name; 
      formals = [(ty, "x")];
      stmts = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
			                         ("printbig", Int) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)
  (* Build local symbol table of variables for this function *)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
  StringMap.empty (globals @ func.formals ) in
(* Return a variable from our local symbol table *)
  let type_of_identifier s =
  try StringMap.find s symbols
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
   (* Return a semantically-checked expression, i.e., with a type *)
  let rec expr = function
    ILit  l -> (Int, SILit l)
  | FLit l -> (Float, SFLit l)
  | Id s       -> (type_of_identifier s, SId s)
  | Binop(e1, op, e2) as e -> 
    let (t1, e1') = expr e1 
    and (t2, e2') = expr e2 in
    (* All binary operators require operands of the same type *)
    let same = t1 = t2 in
    (* Determine expression type based on operator and operand types *)
    let ty = match op with
      Add | Sub | Mul | Div when same && t1 = Int   -> Int
    | Add | Sub | Mul | Div when same && t1 = Float -> Float
    | _ -> raise (Failure ("illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e))
    in (ty, SBinop((t1, e1'), op, (t2, e2')))
  in

  (* Return a semantically-checked statement i.e. containing sexprs *)
  let rec check_stmt = function
    Expr e -> SExpr (expr e)
  | Return e -> SReturn(expr e)
  | Assign(var, e) -> SAssign(var, expr e)
  | VDeAssign(ty, id, ex) -> SVDeAssign(ty, id, expr ex)
  | VDeclare(vdecl) -> SVDeclare(vdecl)
  | Block bl -> SBlock(check_stmt_list bl)
  (* | Assign(var, e) as ex -> 
    let lt = type_of_identifier var
    and (rt, e') = expr e in
    let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
      string_of_typ rt ^ " in " ^ string_of_stmt ex
    in (check_assign lt rt err, SAssign(var, (rt, e'))) *)
  (* A block is correct if each statement is correct and nothing
      follows any Return statement.  Nested blocks are flattened. *)
  (* | VDeclare(vdecl) -> SVDeclare(vdecl)
  | VDeAssign(ty, id, expr) -> SVDeAssign(ty, id, expr) *)
  and check_stmt_list = function
  | [ (Return _ as s) ] -> [ check_stmt s ]
  | Return _ :: _ -> raise (Failure "Unreachable statments after return")
  | Block sl :: ss -> check_stmt_list (sl @ ss)
  | s :: ss -> check_stmt s :: check_stmt_list ss
  | [] -> []
  in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    (* let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in    *)

    in (* body of check_function *)
    {
      sfname = func.fname;
      sformals = func.formals;
      sstmts = check_stmt_list func.stmts
    }    

  let sdecls = { 
    svars = globals;
    sfuncs = List.map check_function functions
  }
  and smain = {
    sstmts = check_stmt_list program.main.stmts
  }
  in
    {
      sdecls = sdecls;
      smain = smain
    }