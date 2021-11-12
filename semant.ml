(* Semantic checking for Marble compiler *)
open Ast
open Sast

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
    }


