open Ast
open Sast
module StringMap = Map.Make (String)

let check (globals, functions) =
  let check_binds kind binds =
    List.iter
      (function
        | Null, b -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
        | _ -> ())
      binds;
    let rec dups = function
      | [] -> ()
      | (_, n1) :: (_, n2) :: _ when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a) (_, b) -> compare a b) binds)
  in

  (**** Check global variables ****)
  check_binds "global" globals;

  (**** Check functions ****)
  let built_in_decls =
    let add_bind map (name, ftypes, ret) =
      StringMap.add name
        {
          return = ret;
          fname = name;
          (* Handles an list of formals types*)
          formals = List.map (fun t -> (t, "PLACEHOLDER")) ftypes;
          stmts = [];
        }
        map
    in
    List.fold_left add_bind StringMap.empty
      [
        ("print", [ Int ], Null);
        ("printb", [ Bool ], Null);
        ("printf", [ Float ], Null);
        ("printmf", [ Matrix ], Null);
        ("rows", [ Matrix ], Int);
        ("cols", [ Matrix ], Int);
        (* TODO: add something like this if needed *)
        (* ("zeros", [Int; Int], Matrix); *)
      ]
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
  let function_decls = List.fold_left add_func built_in_decls functions in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in

  let check_function func =
    check_binds "formal" func.formals;

    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let symbols =
      List.fold_left
        (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals)
    in

    let type_of_identifier s env =
      try StringMap.find s env
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let rec check_expr e env = match e with
    Id n -> (type_of_identifier n env, SId n)
   | ILit l -> (Int, SILit l)
   | FLit l -> (Float, SFLit l)
   | BLit l -> (Bool, SBLit l)
   | MLit l -> 
     let find_inner_type l = match l with
      (* tl e *)
         hd::_ -> let (t,_) = (check_expr hd env) in t
       | _ -> Null
     in      
     let find_type mat = match mat with
        (* tl *)
         hd::_ -> find_inner_type hd
       | _ -> Null
     in
     let my_type = find_type l in
     let rec matrix_expr l =  match l with
         hd::tl -> let (ty,e) = check_expr hd env in
           if ty != my_type then raise (Failure ("Types in matrix do not match."));
           (ty, e) :: (matrix_expr tl)
       | _ -> []
     in
     (Matrix, SMLit(List.map matrix_expr l)) 
   | Binop(e1, op, e2) -> 
     let (t1, e1') = check_expr e1 env
     and (t2, e2') = check_expr e2 env in
     (* All binary operators require operands of the same type *)
     let same = t1 = t2 in
     (* Determine expression type based on operator and operand types *)
     let ty = match op with
       Add | Sub | Mul | Div | Mod when same && t1 = Int   -> Int
     | Add | Sub | Mul | Div | Mod when same && t1 = Float   -> Float
     | Add | Sub when same && t1 = Matrix -> Matrix
     | Eq | Neq  when same -> Bool
     | Less | Leq | Greater | Geq when same && (t1 = Int || t1 = Float) -> Bool
     | And | Or when same && t1 = Bool -> Bool
     | Mul when same && t1 = Matrix -> Matrix
     | Mul when t1 = Matrix && (t2 = Float || t2 = Int) -> Matrix
     | Mul when t2 = Matrix && (t1 = Float || t1 = Int) -> Matrix    
     | _ -> raise (
       Failure ("illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr e))
         in (ty, SBinop((t1, e1'), op, (t2, e2')))
   | Unary(op, e1) as e -> 
         let (tp, c) = check_expr e1 env in
         let ty = match op with
           Neg when (tp = Int || tp = Float) -> tp
         | Not when tp = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ tp ^
                                 " in " ^ string_of_expr e))
         in (ty, SUnary(op, (tp, c)))

    | Func (id, inputs) as func ->
          let fd = find_func id in
          let param_length = List.length fd.formals in

          if List.length inputs != param_length then
            raise
              (Failure
                 ("expecting " ^ string_of_int param_length ^ " arguments in "
                ^ string_of_expr func))
          else
            let check_call (ft, _) e =
              let et, e' = check_expr e env in
              let err =
                "illegal argument found " ^ string_of_typ et ^ " expected "
                ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in
              (check_assign ft et err, e')
            in

            let args' = List.map2 check_call fd.formals inputs in
            (fd.return, SFunc (id, args'))
   | Access(m, r, c) -> 
         let (r_t, _) = check_expr r env in
         let (c_t, _) = check_expr c env in
         if r_t != Int || c_t != Int 
           then raise(Failure ("index must be of type int"));
         (* m_t e *)
         let (_, _) = check_expr m env in
         (Float, SAccess(check_expr m env, check_expr r env, check_expr c env))
 in  

    (* Check stmts - Return a semantically-checked statement i.e. containing sexprs *)
let rec check_stmt env stmt =
  match stmt with
  | Expr e -> (SExpr (check_expr e env), env)
  | Return e ->
      let t, e' = check_expr e env in
      if t = func.return then (SReturn (t, e'), env)
      else
        raise
          (Failure
              ("return gives " ^ string_of_typ t ^ " expected "
            ^ string_of_typ func.return ^ " in " ^ string_of_expr e))
  | VDeclare (t, s) -> (SVDeclare (t, s), StringMap.add s t env)
  | AssignStmt astmt -> (
      match astmt with
      | VDeAssign (t, s, e) ->
          (* e' *)
          let t', _ = check_expr e env in
          let decl_type = check_assign t t' "Type not correct" in
          ( SAssignStmt (SVDeAssign (t, s, check_expr e env)),
            StringMap.add s decl_type env )
      | Assign (s, e) ->
          let left_typ = type_of_identifier s env
          and right_typ, e' = check_expr e env in
          let error = "Illegal assignment!" in
          ignore (check_assign left_typ right_typ error);
          (SAssignStmt (SAssign (s, (right_typ, e'))), env)
      | MAssign (m, r, c, e) ->
          let r_t, _ = check_expr r env in
          let c_t, _ = check_expr c env in
          let e_t, _ = check_expr e env in
          if r_t != Int || c_t != Int then
            raise (Failure "index must be of type int");
          if e_t != Float then raise (Failure "value must be of type float");
          ( SAssignStmt
              (SMAssign
                  ( check_expr m env,
                    check_expr r env,
                    check_expr c env,
                    check_expr e env )),
            env ))

    | While(e,stmts) -> 
        (* styp *)
        let (typ, _) = check_expr e env in
          if typ != Bool then raise (Failure ("Expect to have a Bool type here."));
          (SWhile(check_expr e env, check_stmts env stmts), env)
    | For(astmt, e2, astmt2, stmts) -> 
        let (sastmt, env2)  = check_assignstmt env astmt
        in
        (* env3 *)
        let (sastmt2, _) = check_assignstmt env2 astmt2
        in
        (* styp *)
        let (typ, _) = check_expr e2 env2 in
          if typ != Bool then raise (Failure ("Expect to have a Bool type here."));
          (SFor(sastmt, check_expr e2 env2, sastmt2, check_stmts env2 stmts), env2)
    | If(e, stmts) -> 
        (* styp *)
        let (typ, _) = check_expr e env in
          if typ != Bool then raise (Failure ("Expect to have a Bool type here."));
        (SIf(check_expr e env, check_stmts env stmts), env)
    | IfElse(e, stmts1, stmts2) -> 
        (* styp *)
        let (typ, _) = check_expr e env in
          if typ != Bool then raise (Failure ("Expect to have a Bool type here."));
        (SIfElse(check_expr e env, check_stmts env stmts1, check_stmts env stmts2), env)  

  and check_assignstmt env astmt = match astmt with
    | VDeAssign(t,s,e) -> 
        (* e' *) 
        let (t', _) = check_expr e env in
        let decl_type = check_assign t t' "Type not correct" in
        (SVDeAssign(t, s, check_expr e env), StringMap.add s decl_type env)
    | Assign(s,e) -> 
        let left_typ = type_of_identifier s env
        and (right_typ, e') = check_expr e env in
        let error = "Illegal assignment!"
        in 
          ignore(check_assign left_typ right_typ error);
          (SAssign(s, (right_typ, e')), env)
    | _ -> raise (Failure ("Illegal assignment in for-loop header!"))
  
   and check_stmts env stmts =
          match stmts with
          | [ (Return _ as s) ] ->
              (* env2 *)
              let st, _ = check_stmt env s in
              [ st ]
          | Return _ :: _ -> raise (Failure "Unreachable statments after return")
          | s :: ss ->
              let st, env2 = check_stmt env s in
              st :: check_stmts env2 ss
          | [] -> []
        in

    {
      sreturn = func.return;
      sfname = func.fname;
      sformals = func.formals;
      sstmts = check_stmts symbols func.stmts;
    }
  in
  (globals, List.map check_function functions)