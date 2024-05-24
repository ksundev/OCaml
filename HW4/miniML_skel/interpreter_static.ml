open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem ->
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x -> (apply_env env x, mem)
  | ADD (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match (v1, v2) with
       | (Int n1, Int n2) -> (Int (n1 + n2), mem2)
       | _ -> raise UndefinedSemantics)
  | SUB (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match (v1, v2) with
       | (Int n1, Int n2) -> (Int (n1 - n2), mem2)
       | _ -> raise UndefinedSemantics)
  | MUL (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match (v1, v2) with
       | (Int n1, Int n2) -> (Int (n1 * n2), mem2)
       | _ -> raise UndefinedSemantics)
  | DIV (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match (v1, v2) with
       | (Int n1, Int n2) ->
           if n2 = 0 then raise UndefinedSemantics
           else (Int (n1 / n2), mem2)
       | _ -> raise UndefinedSemantics)
  | EQ (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match (v1, v2) with
       | (Int n1, Int n2) -> (Bool (n1 = n2), mem2)
       | _ -> raise UndefinedSemantics)
  | LT (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match (v1, v2) with
       | (Int n1, Int n2) -> (Bool (n1 < n2), mem2)
       | _ -> raise UndefinedSemantics)
  | ISZERO e ->
      let (v, mem1) = eval e env mem in
      (match v with
       | Int n -> (Bool (n = 0), mem1)
       | _ -> raise UndefinedSemantics)
  | READ ->
      let n = read_int () in
      (Int n, mem)
  | IF (e1, e2, e3) ->
      let (v, mem1) = eval e1 env mem in
      (match v with
       | Bool b -> if b then eval e2 env mem1 else eval e3 env mem1
       | _ -> raise UndefinedSemantics)
  | LET (x, e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let new_env = extend_env (x, v1) env in
      eval e2 new_env mem1
  | LETREC (f, x, e1, e2) ->
      let rec_env = extend_env (f, RecProcedure (f, x, e1, env)) env in
      eval e2 rec_env mem
  | PROC (x, e) -> (Procedure (x, e, env), mem)
  | CALL (e1, e2) ->
      let (proc, mem1) = eval e1 env mem in
      (match proc with
       | Procedure (x, body, decl_env) ->
           let (arg_val, mem2) = eval e2 env mem1 in
           let new_env = extend_env (x, arg_val) decl_env in
           eval body new_env mem2
       | RecProcedure (f, x, body, decl_env) ->
           let (arg_val, mem2) = eval e2 env mem1 in
           let new_env = extend_env (x, arg_val) (extend_env (f, proc) decl_env) in
           eval body new_env mem2
       | _ -> raise UndefinedSemantics)
  | NEWREF e ->
      let (v, mem1) = eval e env mem in
      let loc = new_location () in
      (Loc loc, extend_mem (loc, v) mem1)
  | DEREF e ->
      let (v, mem1) = eval e env mem in
      (match v with
       | Loc loc -> (apply_mem mem1 loc, mem1)
       | _ -> raise UndefinedSemantics)
  | SETREF (e1, e2) ->
      let (v1, mem1) = eval e1 env mem in
      let (v2, mem2) = eval e2 env mem1 in
      (match v1 with
       | Loc loc -> (v2, extend_mem (loc, v2) mem2)
       | _ -> raise UndefinedSemantics)
  | SEQ (e1, e2) ->
      let (_, mem1) = eval e1 env mem in
      eval e2 env mem1
  | BEGIN e -> eval e env mem
  | _ -> raise NotImplemented
  
(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
