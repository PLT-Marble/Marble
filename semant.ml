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
    (* let rec expr e env = match e with

    in *)
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


