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

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
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

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

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

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    (*let local_vars = 
      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          Hashtbl.add m n local 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n builder
          in Hashtbl.add m n local_var
      in
      let formals = Hashtbl.create 20 in 
        List.fold_left2 add_formal formals fdecl.sformals
          (Array.to_list (L.params the_function));
        List.fold_left add_local formals [] 
    in*)

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
      (* null? | SNoexpr     -> L.const_int i32_t 0 *)
      | SId s -> L.build_load (lookup s) s builder
      (* Matrix | SMatrixLit (contents, rows, cols) -> *)
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
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (*let rec stmt builder = function
      | SExpr e ->
          ignore (expr builder e);
          builder
      | SReturn e ->
          ignore (L.build_ret (expr builder e) builder);
          builder
      | SVDeAssign (t, s, se) ->
        let e' = expr builder se 
        in L.set_value_name s e';
        let local_var = L.build_alloca (ltype_of_typ t) s builder
        in ignore (L.build_store e' local_var builder);
        Hashtbl.add local_vars s local_var;
        builder
      | SVDeclare (t, s) ->
        let local_var = L.build_alloca (ltype_of_typ t) s builder
        in Hashtbl.add local_vars s local_var;
        builder
        (* What is the default? *)      
     in*)

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