(* Semantic checking for the BLAStoff compiler *)
open Ast
open Sast
module StringMap = Map.Make (String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check program =
  let main = program.main 
  and vars = program.decls.vars
  and funcs = program.decls.funcs in

  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)
  check_binds "vars" vars;


  (* let check_vars loc stmt_lst =
       let add_decl lst = function
         | Expr e ->
           (match e with
           | Id var -> var :: lst
           | _ -> lst)
         | _ -> lst
       in
       let decls = List.fold_left add_decl [] stmt_lst in
       let rec check_dups = function
         | [] -> ()
         | n1 :: n2 :: _ when n1 = n2 -> raise (Failure ("duplicate " ^ n1 ^ " in " ^ loc))
         | _ :: tl -> check_dups tl
       in
       check_dups (List.sort compare decls)
     in *)

  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet || (lvaluet == Float && rvaluet == Int) then lvaluet
    else raise (Failure err)
  in
  let symbols =
    List.fold_left
      (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty program.decls.vars
  in
  (* Return a variable from our local symbol table *)
  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) =
      StringMap.add name
        { fname = name; formals = [ (ty, "x") ]; stmts = [] }
        map
    in
    List.fold_left add_bind StringMap.empty
      [ ("print", Int); ("printb", Bool); ("printf", Float); ("printbig", Int) ]
    (* Add function name to symbol table *)
  in
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls funcs in
  let find_func fname =
    try StringMap.find fname function_decls
    with Not_found -> raise (Failure ("Undeclared function " ^ fname))
  in
  let rec check_expr = function
    | Id n -> (type_of_identifier n, SId n)
    | ILit l -> (Int, SILit l)
    | FLit l -> (Float, SFLit l)
    (* | MLit m -> SMLit m *)
    | Binop (e1, op, e2) as e ->
        let t1, e1' = check_expr e1 and t2, e2' = check_expr e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty =
          match op with
          | (Add | Sub | Mul | Div) when same && t1 = Int -> Int
          | (Add | Sub | Mul | Div) when same && t1 = Float -> Float
          | _ ->
              raise
                (Failure
                   ("illegal binary operator " ^ string_of_typ t1 ^ " "
                  ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in "
                  ^ string_of_expr e))
        in
        (ty, SBinop ((t1, e1'), op, (t2, e2')))
  in

  (*let rec check_stmt = function
      | Assign (n, e) -> SAssign (n, check_expr e)
      | Expr e -> SExpr (check_expr e)
      | Block bl -> SBlock (check_stmt_list bl)
      | Return e -> SReturn (check_expr e)
      | VDeclare (ty, n) -> SVDeclare(ty, n)
      | VDeAssign (ty, n, e) -> SVDeAssign(ty, n, check_expr e)
    and check_stmt_list = function
      | [ (Return _ as s) ] -> [ check_stmt s ]
      | Return _ :: _ -> raise (Failure "Unreachable statments after return")
      | Block sl :: ss -> check_stmt_list (sl @ ss)
      | s :: ss -> check_stmt s :: check_stmt_list ss
      | [] -> []
    in*)

  (* Check stmts - Return a semantically-checked statement i.e. containing sexprs *)
  let rec check_stmt env stmt =
    match stmt with
    | Expr e -> (SExpr (expr e env), env)
    | Return e ->
        let t, e' = expr e env in
        (SReturn (t, e'), env)
    | VDeclare (t, s) -> (SVDeclare (t, s), StringMap.add s t env)
    | AssignStmt astmt -> (
        match astmt with
        | VDeAssign (t, s, e) ->
            let t', e' = expr e env in
            let decl_type = check_assign t t' "Type not correct" in
            (SVDeAssign (t, s, expr e env), StringMap.add s decl_type env)
        | Assign (s, e) ->
            let left_typ = type_of_identifier s env
            and right_typ, e' = expr e env in
            let error = "Illegal assignment!" in
            ( check_assign left_typ right_typ error,
              (SAssign (s, (right_typ, e')), env) ))
  in

  let check_decls vars funcs =
    let check_function func =
      {
        sfname = func.fname;
        sformals = func.formals;
        sstmts =
          List.fold_left check_stmt symbols func.stmts
          (*sstmts = check_stmt_list func.stmts *);
      }
    in
    { svars = vars; sfuncs = List.map check_function funcs }
  in
  {
    sdecls = check_decls vars funcs;
    smain =
      { sstmts = List.fold_left check_stmt symbols program.main.stmts }
      (*smain = { sstmts = check_stmt_list program.main.stmts }*);
  }

(* 
module StringMap = Map.Make(String)


(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong. *)

(* Check each global variable, then check each function *)
let check (globals, functions) =

(* Check binds - Verify a list of bindings has no void types or duplicate names *)

(* Check functions  - Add function name to symbol table 
                    - Make sure no formals or locals are void or duplicates
                    - Build local symbol table of variables for this function
 *)

(* let check_function func = *)

    (* Raise an exception 
        if the given rvalue type cannot be assigned to the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    (*let symbols = 

    *)

    (* Return a variable from our local symbol table *)
    
    let type_of_identifier s env =
      try StringMap.find s env
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

(* Check exprs - Return a semantically-checked expression, i.e., with a type *)
    let rec expr e env = match e with
        Ilit l -> (Int, SIlit l)
        | FLit l -> (Float, SFLit l)
        | BLit l -> (Bool, SBLit l)
        (* | MLit l ->  some matrix checking? *)
        | Id s -> (type_of_identifier s, SId s)
        | Binop (e1, op, e2) as e ->
            let (tp1, c1) = expr e1 and (tp2, c2) = expr e2 in
            (* check if two expr connected by binary op with same type *)
            let same = (t1 = t2) in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
              Add | Sub | Mult | Div when same && tp1 = Int   -> Int
            | Add | Sub | Mult | Div when same && tp1 = Float -> Float
            | Equal | Neq            when same               -> Bool
            | Less | Leq | Greater | Geq
                        when same && (tp1 = Int || tp1 = Float) -> Bool
            | And | Or when same && tp1 = Bool -> Bool
            (* REQ specified for nonpremitive type or matrix? *)
            | _ -> raise (
            Failure ("illegal binary operator " ^
                        string_of_typ tp1 ^ " " ^ string_of_op op ^ " " ^
                        string_of_typ tp2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((tp1, c1), op, (tp2, c2)))
        | Unary(op, e1) as e -> 
            let (tp, c) = expr e1 in
            let ty = match op with
            t = Int || t = Float -> t
            | _ -> raise (Failure ("illegal unary operator " ^ 
                                    string_of_uop op ^ string_of_typ tp ^
                                    " in " ^ string_of_expr e))
            in (ty, SUnary(op, (tp, c)))
        | Negate(op, e1) as e -> 
            let (tp, c) = expr e1 in
            let ty = match op with
            t = Bool -> Bool
            | _ -> raise (Failure ("illegal negate operator " ^ 
                                    string_of_uop op ^ string_of_typ tp ^
                                    " in " ^ string_of_expr e))
            in (ty, SNegate(op, (tp, c)))

        (* Incomplete function call part *)
        | Func(fname, args) as call -> 
            let fd = find_func fname in
            let param_length = List.length fd.formals in
            if List.length args != param_length then
                raise (Failure ("expecting " ^ string_of_int param_length ^ 
                                " arguments in " ^ string_of_expr call))
            else let check_call (ft, _) e = 
                let (et, e') = expr e in 
                let err = "illegal argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
                in (check_assign ft et err, e')
            in 
            let fdFormals = List.map (fun (tp, vName, _) -> (tp, vName) ) fd.formals in
            let args' = List.map2 check_call fdFormals args
            in (fd.typ, SFunc(fname, args'))
    in 
(* Check stmts - Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt env stmt = match stmt with
        Expr e -> (SExpr (expr e env), env)
        | Return e -> let (t, e') = expr e env in (SReturn (t, e'), env)
        | VDeclare(t,s) ->  (SVDeclare(t, s), StringMap.add s t env)
        | AssignStmt astmt -> match astmt with
                                VDeAssign(t,s,e) ->  
                                    let (t', e') = expr e env in
                                    let decl_type = check_assign t t' "Type not correct" in
                                    (SVDeAssign(t, s, expr e env), StringMap.add s decl_type env)
                                | Assign(s,e) -> 
                                    let left_typ = type_of_identifier s env
                                    and (right_typ, e') = expr e env in
                                    let error = "Illegal assignment!"
                                    in (check_assign left_typ right_typ error, (SAssign(s, (right_typ, e')), env))


    in (* body of check_function *)
    {
        sfname = func.fname;
        sformals = func.formals;
        sbody = List.fold_left check_stmt symbols func.stmts
    } *)
