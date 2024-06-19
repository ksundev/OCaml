type program = exp
and exp =
	| SKIP
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| LE of exp * exp
	| EQ of exp * exp
	| NOT of exp 
  | IF of exp * exp * exp
	| WHILE of exp * exp 
	| LET of var * exp * exp
	| PROC of var list * exp 
	| CALLV of exp * exp list 
	| CALLR of exp * var list
	| ASSIGN of var * exp 
	| RECORD of (var * exp) list 
	| FIELD of exp * var
	| ASSIGNF of exp * var * exp 
  | READ of var
	| PRINT of exp 
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int
  | Bool of bool
	| Unit
  | Procedure of var list * exp * env
	| Record of record
  | Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
  | Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1; (!counter)

(* conversion of env to string *)
let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
(* conversion of mem to string *)
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		
		
exception NotImplemented
exception UndefinedSemantics
(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
let remove_garbage = ref false 

let gc: env * mem -> mem
= fun (env, mem) ->
	if not !remove_garbage then mem
	else
		let rec get_reachable_locs_from_env env =
			let rec worker env acc =
				match env with
				| [] -> acc
				| (_, loc) :: tl -> worker tl (loc :: acc)
			in
			worker env []
		in
		let rec get_reachable_locs_from_value v =
			match v with
			| Int _ | Bool _ | Unit -> []
			| Loc loc -> [loc]
			| Procedure (_, _, env) -> get_reachable_locs_from_env env
			| Record fields ->
				let rec worker fields acc =
					match fields with
					| [] -> acc
					| (_, loc) :: tl -> worker tl (loc :: acc)
				in
				worker fields []
		in
		let rec collect_reachable_locs locs mem =
			match mem with
			| [] -> locs
			| (loc, v) :: tl ->
				let rec mem_loc locs loc =
					match locs with
					| [] -> false
					| h :: t -> if h = loc then true else mem_loc t loc
				in
				if mem_loc locs loc then collect_reachable_locs locs tl
				else collect_reachable_locs (locs @ (get_reachable_locs_from_value v)) tl
		in
		let initial_locs = get_reachable_locs_from_env env in
		let reachable_locs = collect_reachable_locs initial_locs mem in
		let rec filter mem acc =
			match mem with
			| [] -> acc
			| (loc, v) :: tl ->
				let rec mem_loc locs loc =
					match locs with
					| [] -> false
					| h :: t -> if h = loc then true else mem_loc t loc
				in
				if mem_loc reachable_locs loc then filter tl ((loc, v) :: acc)
				else filter tl acc
		in
		filter mem []

let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem ->  
	match pgm with
	| SKIP -> (Unit, mem)
	| TRUE -> (Bool true, mem)
	| FALSE -> (Bool false, mem)
	| CONST n -> (Int n, mem)
	| VAR x -> (apply_mem mem (apply_env env x), mem)
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
			| (Int n1, Int n2) -> if n2 = 0 then raise UndefinedSemantics else (Int (n1/n2), mem2)
			| _ -> raise UndefinedSemantics)
	| LE (e1, e2) ->
		let (v1, mem1) = eval e1 env mem in
		let (v2, mem2) = eval e2 env mem1 in
		(match (v1, v2) with
			| (Int n1, Int n2) -> (Bool (n1 <= n2), mem2)
			| _ -> raise UndefinedSemantics)
	| EQ (e1, e2) ->
		let (v1, mem1) = eval e1 env mem in
		let (v2, mem2) = eval e2 env mem1 in
		(match (v1, v2) with
			| (Int n1, Int n2) -> (Bool (n1 = n2), mem2)
			| (Bool b1, Bool b2) -> (Bool (b1 = b2), mem2)
			| _ -> raise UndefinedSemantics)
	| NOT e ->
		let (v, mem') = eval e env mem in
		(match v with
			| Bool b -> (Bool (not b), mem')
			| _ -> raise UndefinedSemantics)
	| IF (e1, e2, e3) ->
		let (v, mem1) = eval e1 env mem in
		(match v with
			| Bool true -> eval e2 env mem1
			| Bool false -> eval e3 env mem1
			| _ -> raise UndefinedSemantics)
	| WHILE (e1, e2) ->
		let rec loop env mem =
			let (v, mem') = eval e1 env mem
		in
			match v with (* 조건문 값 *)
			| Bool true ->
				let (_, mem'') = eval e2 env mem'
			in loop env mem''
			| Bool false -> (Unit, mem')
			| _ -> raise UndefinedSemantics
		in loop env mem
	| LET (x, e1, e2) ->
		let (v1, mem1) = eval e1 env mem in
		let loc = new_location () in
		let env' = extend_env (x, loc) env in
		let mem2 = extend_mem (loc, v1) mem1 in
		eval e2 env' mem2
	| PROC (params, body) -> (Procedure (params, body, env), mem)
	| CALLV (proc, args) ->
		let (v, mem1) = eval proc env mem in
		(match v with
			| Procedure (params, body, proc_env) ->
				let arg_vals = List.map (fun e -> let (v, m) = eval e env mem1 in v) args in
				let locs = List.map (fun _ -> new_location ()) params in
				let env' = List.fold_left2 (fun env' (p, loc) v -> extend_env (p, loc) env') proc_env (List.combine params locs) arg_vals in
				let mem3 = List.fold_left2 (fun mem3 loc v -> extend_mem (loc, v) mem3) mem1 locs arg_vals in
				eval body env' mem3
			| _ -> raise UndefinedSemantics)
	| CALLR (proc, args) ->
		let (v, mem1) = eval proc env mem in
		(match v with
			| Procedure (params, body, proc_env) ->
				let env' = List.fold_left2 (fun env' param arg -> extend_env (param, apply_env env arg) env') proc_env params args in
				eval body env' mem1
			| _ -> raise UndefinedSemantics)
	| ASSIGN (x, e) ->
		let (v, mem1) = eval e env mem in
		let loc = apply_env env x in
		(v, extend_mem (loc, v) mem1)
	| RECORD fields ->
		let (field_locs, mem') = 
			List.fold_left 
				(fun (acc, mem) (name, exp) ->
					let (v, mem') = eval exp env mem in
					let loc = new_location () in
					((name, loc) :: acc, extend_mem (loc, v) mem'))
				([], mem) fields
		in
		(Record (List.rev field_locs), mem')
	| FIELD (record_exp, field_name) ->
		let (record_val, mem') = eval record_exp env mem in
		(match record_val with
			| Record field_locs ->
				(match List.assoc_opt field_name field_locs with
				| Some loc -> (apply_mem mem' loc, mem')
				| None -> raise UndefinedSemantics)
			| _ -> raise UndefinedSemantics)
	| ASSIGNF (record_exp, field_name, exp) ->
		let (record_val, mem1) = eval record_exp env mem in
		let (v, mem2) = eval exp env mem1 in
		(match record_val with
			| Record field_locs ->
				(match List.assoc_opt field_name field_locs with
				| Some loc -> (v, extend_mem (loc, v) mem2)
				| None -> raise UndefinedSemantics)
			| _ -> raise UndefinedSemantics)
	| SEQ (e1, e2) ->
		let (_, mem1) = eval e1 env mem in
		eval e2 env mem1
	| BEGIN e -> eval e env mem
	| READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
		let v, mem' = eval e env mem in
		let _ = print_endline (value2str v) in
		(v, gc(env,mem')) (* Do not modify *)


let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))