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

module StringMap = Map.Make(String)

(* Called in microc.ml: Codegen.translate ast where ast -> **decls** EOF -> **(vdecls, fdecls)** EOF *)
let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  (* match types in our language to LLVM types *)
  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =     (* m is a StringMap; fdecl is a function declaration record in functions *)
      let name = fdecl.A.fname      (* get the name field of fdecl *)
      and formal_types =
       (* Apply a function (fun (t,_) -> ltype_of_typ t) to each element of a list (fdecl.A.formals) to produce another 
       list (which is then converted to an array and stored under the name formal_types) 
       Basically, get the LLVM type of each formal parameter using the ltype_of_typ function we declared above *)
	    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) (* a formal element is a pair (typ, ID); we want the typ, hence ID is _ (it doesn't matter) *)
      
      (* get LLVM function type of our function
      from llvm.moe:
        function_type ret_ty param_tys - returns the function type returning ret_ty (ltype_of_typ fdecl.A.typ - the LLVM version of the MS fdecl type) and taking 
        param_tys (formal_types) as parameters
      *)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in

      (* each <key, value> in StringMap m is:
          key = function name
          value = an ocaml PAIR, (LLVM_function_declaration * original_ocaml_fdecl) where:
            -LLVM_function_declaration
              from llvm.moe:
                define_function name ty m creates a new function with name name and type ty in module m
                  name = name
                  ty = ftype (LLVM function type of our function)
                  m = our MatchaScript module
            -original_ocaml_fdecl:
              original ocaml parser representation of the entire function declaration
      *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in

    (* List.fold_left applies a function (function_decl) to a partial result (StringMap.empty) and an element
    of the list (an element of the list ~functions~) to produce the next partial result; continue until
    all elements of the list have been processed and added to the partial result *)
    List.fold_left function_decl StringMap.empty functions in
  
(* ----------------------- call to build_function_body functions  ------------------------------ *)

  (* Fill in the body of the function fdecl *)
  (* for each function in functions, do this. At this point, because of the previous let .. in 
  statements, all of the function_decls have been stored into a StringMap *)
  let build_function_body fdecl =
    (* the_function, _) is the <key,value> pair in StringMap function_decls (created above) where:
        -key = fdecl.A.fname (= the_function)
        -value = an ocaml pair (LLVM_function_declaration * original_ocaml_fdecl)
    *)
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    
    (* 
    from llvm.moe:
      builder_at_end llcontext bb : 
          val builder_at_end : llcontext -> llbasicblock -> llbuilder (accepts two inputs, first of type 
            llcontext and the second of type llbasic block)
          creates an instruction builder positioned at the end of the basic block bb.
      entry_block fn : returns the entry basic block of the function f (the_function)

      So the line below creates an instruction builder positioned at the end of the basic block of the function 
    *)
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* 
    val build_global_stringptr : string -> string -> llbuilder -> llvalue
      build_global_stringptr str name b creates a series of instructions that adds a global string pointer at the
      position specified by the instruction builder b.
        str = "%d\n"
        name = "fmt"
        b = builder
    ???
    *)
    let int_format_str = 
      L.build_global_stringptr "%d\n" "fmt" builder in
    
    (* ----------------- Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map ----------------*)
    let local_vars =
      (* 
      val set_value_name : string -> llvalue -> unit
        set_value_name n p sets the name of the value p to n (assigns the name of the formal 
        to the value p)
          -n = formals
          -p = params
      m = StringMap
      (t, n) = a formal vdecl pair, where t= type, n= formal name
      p = parameter value
      *)
      let add_formal m (t, n) p = L.set_value_name n p;

  (* build_alloca ty name b creates a %name = alloca %ty instruction at the position specified by
      the instruction builder b
    So this statement allocates size(%ty (e.g. i32_t)) bytes of stack space for the local variable named n
  *)
	let local = L.build_alloca (ltype_of_typ t) n builder in

  (* build_store p local builder
    creates a ~ store %p, %local ~ instruction at the position specified by the instruction builder builder
      - p is the function name
      - local is the 
      We store the value of %p at the variable %local (which was stack-allocated above)
  *)
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in
      (* L.params f returns the parameters of function f *)
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	      A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	  let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	  let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
  	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	  A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	    | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb
      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb
      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in
    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  
  (* 
  call to translate starts here 
  functions is from param ast = (globals, functions) above (the arguments to translate)
  the first thing you do is build_function_body for each function in functions
  *)
  in List.iter build_function_body functions;   (* List.iter build_function_body on every function in functions *)
  
(* creates the LLVM module, with the above translate function defined,
even if translate is never called *)
the_module