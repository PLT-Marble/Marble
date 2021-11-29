(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 


module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (program) =
  let globals = program.sdecls.svars in
  let functions = program.sdecls.sfuncs in
  let main_fdecl = { sfname="main"; sformals=[]; sstmts=program.smain.sstmts } in
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Marble" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
    and i8_t       = L.i8_type     context
    and i1_t       = L.i1_type     context
    and float_t    = L.double_type context
    and void_t     = L.void_type   context in

  (* Return the LLVM type for a Marble type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool -> i1_t
    | A.Matrix -> L.pointer_type float_t
    | A.Null  -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Int -> L.const_int (ltype_of_typ t) 0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printmf_t : L.lltype =
      L.function_type i32_t [| L.pointer_type float_t |] in
  let printmf_func : L.llvalue =
      L.declare_function "printmf" printmf_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfdecl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
        (* TODO: remove hardcoded Int, strict function type? *)
      in let ftype = L.function_type (ltype_of_typ A.Int) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    let decls_with_main = List.fold_left function_decl StringMap.empty functions in
    StringMap.add
      "main"
      ( L.define_function
          "main"
          (L.function_type i32_t (Array.of_list []))
          the_module
      , main_fdecl )
      decls_with_main
    in
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    let local_vars = Hashtbl.create 20 in
			let add_formal (t, n) p = 
				L.set_value_name n p;
				let local = L.build_alloca (ltype_of_typ t) n builder in
					ignore (L.build_store p local builder);
					ignore (Hashtbl.add local_vars n local)

			in
			List.iter2 add_formal fdecl.sformals (Array.to_list (L.params the_function));

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try Hashtbl.find local_vars n
                   with Not_found -> 
                    try StringMap.find n global_vars 
                     with Not_found -> raise (Failure ("undeclared identifier " ^ n))
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) =
      match e with
      | SILit i -> L.const_int i32_t i
      | SFLit f -> L.const_float float_t f
      | SBLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SMLit l -> 
        let find_inner_type l = match l with
            hd::tl -> let (t,e) = hd in t
          | _ -> A.Int
        in        
        let find_type mat = match mat with
            hd::tl -> find_inner_type hd
          | _ -> A.Int
        in
        let my_type = find_type l in 
        let make_matrix = match my_type with
          A.Int ->
            (* extract rows and column info here *)
            let count a = List.fold_left (fun x _ -> x + 1) 0 a in
            let rows = count l in 
            let cols = count (List.hd l) in
            let rec valid_dims m = match m with
                hd::tl -> if count hd == count (List.hd l) 
                            then valid_dims tl
                          else false
              | _ -> true
            in 
            if not (valid_dims l) then
              raise(Failure "all rows of matrices must have the same number of elemens")
            else

            (* allocate space 2 + rows * cols*)
            let matrix = L.build_alloca (L.array_type i32_t (2+rows*cols)) "matrix" builder in
            
            let eval_row row 
              = List.fold_left (fun eval_row x -> eval_row @ [expr builder x]) [] row in 
            let unfolded = List.fold_left (fun unfld row -> unfld @ (eval_row row)) [] l in
            let unfolded = [L.const_int i32_t rows; L.const_int i32_t cols] @ unfolded in
            
            let rec store idx lst = match lst with
              hd::tl -> let ptr = L.build_in_bounds_gep matrix [| L.const_int i32_t 0; L.const_int i32_t idx|] "ptr" builder in
                        ignore(L.build_store hd ptr builder);
                        store (idx + 1) tl;
              | _ -> ()
            in
            store 0 unfolded;
            L.build_in_bounds_gep matrix [|L.const_int i32_t 0; L.const_int i32_t 0|] "matrix" builder 
        | A.Float ->
          let count a = List.fold_left (fun x _ -> x + 1) 0 a in
          let rows = float_of_int (count l) in 
          let cols = float_of_int (count (List.hd l)) in
          let rec valid_dims m = match m with
              hd::tl -> if count hd == count (List.hd l) 
                          then valid_dims tl
                        else false
            | _ -> true
          in 
          if not (valid_dims l) then
            raise(Failure "all rows of matrices must have the same number of elemens")
          else

          (* allocate space 2 + rows * cols*)
          let matrix = L.build_alloca (L.array_type float_t (2+(int_of_float rows)*(int_of_float cols))) "matrix" builder in

          let eval_row row 
            = List.fold_left (fun eval_row x -> eval_row @ [expr builder x]) [] row in 
          let unfolded = List.fold_left (fun unfld row -> unfld @ (eval_row row)) [] l in
          let unfolded = [L.const_float float_t rows; L.const_float float_t cols] @ unfolded in
          let rec store idx lst = match lst with
            hd::tl -> let ptr = L.build_in_bounds_gep matrix [| L.const_int i32_t 0; L.const_int i32_t idx|] "ptr" builder in
                      ignore(L.build_store hd ptr builder);
                      store (idx + 1) tl;
            | _ -> ()
          in
          store 0 unfolded;
          L.build_in_bounds_gep matrix [| L.const_int i32_t 0; L.const_int i32_t 0|] "matrix" builder 
        | _ -> raise (Failure "invalid matrix type")
        in make_matrix
      (* null? | SNoexpr     -> L.const_int i32_t 0 *)
      | SId s -> L.build_load (lookup s) s builder
      (* Matrix | SMatrixLit (contents, rows, cols) -> *)
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with 
          A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mul    -> L.build_fmul
        | A.Div     -> L.build_fdiv 
        )
        e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
          let e1' = expr builder e1 and e2' = expr builder e2 in
          (match op with
          | A.Add -> L.build_add
          | A.Sub -> L.build_sub
          | A.Mul -> L.build_mul
          | A.Div -> L.build_sdiv
          )
        e1' e2' "tmp" builder
      (* Unary and Negate *)
      (* Function call *)
      | SFunc ("print", [e])
      | SFunc ("printb", [e]) ->
	      L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder      
      | SFunc ("printf", [e]) -> 
	      L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SFunc ("printmf", [e]) ->
        L.build_call printmf_func [| (expr builder e)|] "printmf" builder
      (* | SFunc (f, args) ->
        let fdef, fdecl = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = "_result"
          (* match fdecl.styp with A.Void -> "" | _ -> f ^ "_result" *)
        in
        L.build_call fdef (Array.of_list llargs) result builder
        in
        ignore (List.map (fun (_, _, v) -> expr builder v) fdecl.sformals);             *)
      in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
    	Some _ -> ()
      | None -> ignore (instr builder) in
    let rec stmt builder = function
      | SExpr e ->
          ignore (expr builder e);
          builder
      | SReturn e ->
          ignore (L.build_ret (expr builder e) builder);
          builder
      | SVDeclare (t, s) ->
          let local_var = L.build_alloca (ltype_of_typ t) s builder in 
            Hashtbl.add local_vars s local_var;
            builder
      | SAssignStmt sastmt -> (
          match sastmt with
          | SVDeAssign (t, s, se) ->
              let e' = expr builder se in L.set_value_name s e';
              let local_var = L.build_alloca (ltype_of_typ t) s builder 
              in 
                ignore (L.build_store e' local_var builder);
                Hashtbl.add local_vars s local_var;
                builder
          | SAssign (s, se) ->
              let e' = expr builder se in
                ignore (L.build_store e' (lookup s) builder);
                builder
        )
      | SWhile (se, sstmts) ->
          let se_bb = L.append_block context "while" the_function in 
              ignore(L.build_br se_bb builder);
          let body_bb = L.append_block context "while_body" the_function in
              add_terminal (List.fold_left stmt (L.builder_at_end context body_bb) sstmts) (L.build_br se_bb);
          let pred_builder = L.builder_at_end context se_bb in
          let bool_val = expr pred_builder se in
          let merge_bb = L.append_block context "merge" the_function in
            ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
            L.builder_at_end context merge_bb
      | SFor (sastmt, se2, se3, sstmts) -> 
          ignore(stmt builder (SAssignStmt sastmt));
          let body = List.rev (SExpr se3 :: sstmts) in stmt builder (SWhile(se2, body))
      | SIf (se, sstmts) -> 
          let bool_val = expr builder se in 
          let merge_bb = L.append_block context "merge" the_function in 
          let b_br_merge = L.build_br merge_bb in
          let then_bb = L.append_block context "then" the_function in
          add_terminal (List.fold_left stmt (L.builder_at_end context then_bb) sstmts) b_br_merge;
          let else_bb = L.append_block context "else" the_function in
          add_terminal (List.fold_left stmt (L.builder_at_end context else_bb) []) b_br_merge;
          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      | SIfElse (se, sstmts1, sstmts2) ->
          let bool_val = expr builder se in 
          let merge_bb = L.append_block context "merge" the_function in 
          let b_br_merge = L.build_br merge_bb in
          let then_bb = L.append_block context "then" the_function in
          add_terminal (List.fold_left stmt (L.builder_at_end context then_bb) sstmts1) b_br_merge;
          let else_bb = L.append_block context "else" the_function in
          add_terminal (List.fold_left stmt (L.builder_at_end context else_bb) sstmts2) b_br_merge;
          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
     in
    (* Build the code for each statement in the function *)
    let builder = List.fold_left stmt builder fdecl.sstmts in
    add_terminal builder L.build_ret_void
    (* Add a return if the last block falls off the end *)
    (* add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) *)
  in
  List.iter build_function_body (main_fdecl::functions);

  the_module