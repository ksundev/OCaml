open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp
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
  | Procedure (x,e) -> "Procedure "

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

(* represent the given environment as a string (use this for debugging) *)
let rec string_of_env env = 
	(List.fold_left (fun acc (x,v) -> Printf.sprintf "%s, %s |-> %s" acc x (value2str v)) "{" env) ^ "}" 

(* represent the given memory as a string (use this for debugging) *)
let rec string_of_mem mem = 
	(List.fold_left (fun acc (l,v) -> Printf.sprintf "%s, %d |-> %s" acc l (value2str v)) "{" mem) ^ "}" 
		 

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem -> 
	print_endline (Printf.sprintf "eval: %s" (Lang.string_of_exp exp));
	print_endline (Printf.sprintf "env: %s" (string_of_env env));
	print_endline (Printf.sprintf "mem: %s" (string_of_mem mem));
  match exp with 
	| CONST n -> (Int n, mem) 
  | VAR x -> (apply_env env x, mem) 
	| EQ (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(Bool (v1 = v2), mem2)
	| LT (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Bool (n1 < n2), mem2) 
		| _ -> raise UndefinedSemantics) 
  | ADD (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 + n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | SUB (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 - n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | MUL (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 * n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | DIV (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> if n2 = 0 then raise UndefinedSemantics else (Int (n1 / n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | ISZERO e -> 
		let v1, mem1 = eval e env mem in
		(match v1 with 
		| Int n1 -> (Bool (n1 = 0), mem1) 
		| _ -> raise UndefinedSemantics)  
  | READ -> 
		let n = read_int () in 
		(Int n, mem)  
  | IF (e1, e2, e3) -> 
		let v1, mem1 = eval e1 env mem in
		(match v1 with 
		| Bool b -> 
			if b then 
				eval e2 env mem1  
			else 
				eval e3 env mem1
		| _ -> raise UndefinedSemantics) 
  | LET (x, e1, e2) -> 
		let v1, mem1 = eval e1 env mem in
		let env' = extend_env (x, v1) env in 
		eval e2 env' mem1
  | LETREC (f, x, e1, e2) -> 
		eval (LET (f, PROC(x, e1), e2)) env mem
	| PROC (x, e) -> (Procedure(x, e), mem)
  | CALL (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in
		let v, mem2 = eval e2 env mem1 in
		(match v1 with 
		| Procedure (x, e) ->
			let env' = (extend_env (x, v) env) in  
			eval e env' mem2
		| _ -> raise UndefinedSemantics
		)
  | NEWREF e -> 
		let v, mem1 = eval e env mem in
		let l = new_location () in 
		(Loc l, extend_mem (l, v) mem1)
  | DEREF e ->
		let v, mem1 = eval e env mem in
		(match v with
		| Loc l -> (apply_mem mem1 l, mem1)
		| _ -> raise UndefinedSemantics)
  | SETREF (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in
		let v2, mem2 = eval e2 env mem1 in
		(match v1 with
		| Loc l -> (v2, extend_mem (l, v2) mem2)
		| _ -> raise UndefinedSemantics)
  | SEQ (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in
		let v2, mem2 = eval e2 env mem1 in
		(v2, mem2) 
  | BEGIN e -> eval e env mem 


(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
