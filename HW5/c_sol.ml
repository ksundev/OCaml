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

let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		
		
exception NotImplemented
exception UndefinedSemantics
let remove_garbage = ref false



let rec add_if_notexist n lst =  
  match lst with
  | [] -> []
  | hd :: tl -> if n = hd then add_if_notexist n tl else hd :: (add_if_notexist n tl)

let rec uniq lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd :: add_if_notexist hd (uniq tl)
	
let rec reach: env * mem * loc list -> loc list 
= fun (env, mem, locs) -> 
	let locs = uniq locs in 
	let locs' = locs @ (List.map (fun (x,l) -> l) env) in
	let locs' = 
		List.fold_left (fun locs' l -> 
  		let v = apply_mem mem l in
  		match v with 
  		| Loc l' -> l' :: locs'
			| Record record -> locs' @ (List.map (fun (var,loc) -> loc) record)
			| Procedure (params, e, env') -> locs' @ (reach (env', mem, []))
  		| _ -> locs'
		) locs' locs'    
	in 
	let locs' = uniq locs' in 
	if (List.length locs') > (List.length locs) then reach (env, mem, locs')
	else locs
	
let gc: env * mem -> mem
= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else 
	let locs = reach (env, mem, []) in 
	let mem' = List.filter (fun (l,v) -> List.mem l locs) mem in mem'

	
let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem -> 
	(* let mem = gc(env, mem) in *)
	let v, mem' = 
	  match pgm with
  	| SKIP -> (Unit, mem)
    | READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
  	(* | _ -> raise NotImplemented (* TODO *) *)
    | PRINT e ->
  		let v, mem' = eval e env mem in
			(* let _ = print_endline ("final env: " ^ (string_of_env env)) in *)
			(* let _ = print_endline ("final mem: " ^ (string_of_mem mem)) in *)
  		let _ = print_endline (value2str v) in
  		(v, gc(env,mem')) (* Do not modify *)  
  	| TRUE -> (Bool true, mem)
  	| FALSE -> (Bool false, mem)
    | CONST n -> (Int n, mem)
    | VAR x ->
  		let l : loc = (apply_env env x) in
  		let v : value = (apply_mem mem l) in 
  		(v, mem)
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
  		| Int n1, Int n2 -> (Int (n1 / n2), mem2)
  		| _ -> raise UndefinedSemantics)
  	| LE (e1, e2) -> 
  		let v1, mem1 = eval e1 env mem in
  		let v2, mem2 = eval e2 env mem1 in
  		(match v1, v2 with
  		| Int n1, Int n2 -> (Bool (n1 <= n2), mem2)
  		| _ -> raise UndefinedSemantics)
  	| EQ (e1, e2) ->
  		let v1, mem1 = eval e1 env mem in
  		let v2, mem2 = eval e2 env mem1 in
  		(match v1, v2 with
  		| Int n1, Int n2 -> (Bool (n1 = n2), mem2)
  		| Bool b1, Bool b2 -> (Bool (b1 = b2), mem2)
  		| Unit, Unit -> (Bool true, mem2)
  		| _ -> (Bool false, mem2))
  	| NOT e ->
  		let v, mem1 = eval e env mem in
  		(match v with
  		| Bool b -> (Bool (not b), mem1)
  		| _ -> raise UndefinedSemantics)
    | IF (e1, e2, e3) ->
  		let v1, mem1 = eval e1 env mem in
  		(match v1 with
  		| Bool b ->
  			if b then
  				eval e2 env mem1
  			else
  				eval e3 env mem1
  		| _ -> raise UndefinedSemantics)
  	| WHILE (e1, e2) ->
  		let v1, mem1 = eval e1 env mem in
  		(match v1 with
  		| Bool b ->
  			if b then
  				let _, mem2 = eval e2 env mem1 in
  				eval pgm env mem2
  			else
  				(Unit, mem1)
  		| _ -> raise UndefinedSemantics)
  	| LET (x, e1, e2) ->
  		let v1, mem1 = eval e1 env mem in
  		let l = new_location () in
  		let env' = extend_env (x, l) env in
  		let mem1' = extend_mem (l, v1) mem1 in
  		eval e2 env' mem1'
  	| PROC (params, e) -> (Procedure (params, e, env), mem)
  	| CALLV (fexp, args) ->
  		let v, mem0 = eval fexp env mem in
  		(match v with
  		| Procedure (params, e, env') ->
  			if (List.length params) != (List.length args) then raise UndefinedSemantics
  			else
  			let vs, memn =
  				List.fold_left (fun (vs, mem) arg ->
  					let v, mem' = eval arg env mem in
  					(vs @ [v], mem')
  				) ([], mem0) args
  			in
  			let locvs = List.fold_left (fun locvs v -> locvs @ [(new_location(), v)]) [] vs in
  			let env' = List.fold_left (fun env' (x, (l,_)) -> extend_env (x, l) env') env' (List.combine params locvs) in
  			let memn = List.fold_left (fun memn (l,v) -> extend_mem (l, v) memn) memn locvs in
  			let v, mem' = eval e env' memn in 
				(v, mem')
  		| _ -> raise UndefinedSemantics)
  	| CALLR (fexp, args) ->
  		let v, mem0 = eval fexp env mem in
  		(match v with
  		| Procedure (params, e, env') ->
  			if (List.length params) != (List.length args) then raise UndefinedSemantics
  			else
  			let env' = List.fold_left (fun env' (x, y) -> extend_env (x, apply_env env y) env') env' (List.combine params args) in
  			eval e env' mem0
  		| _ -> raise UndefinedSemantics)
  	| ASSIGN (x, e) ->
  		let v, mem1 = eval e env mem in
  		let l = apply_env env x in
  		(v, extend_mem (l,v) mem1)
  	| RECORD ves ->
  		if (List.length ves) = 0 then (Unit, mem) 
  		else
  		let vs, memn =
  			List.fold_left (fun (vs, mem) (_,e) ->
  				let v, mem' = eval e env mem in
  				(vs @ [v], mem')
  			) ([], mem) ves
  		in
  		let locvs = List.fold_left (fun locvs v -> locvs @ [(new_location(), v)]) [] vs in
  		let record = List.fold_left (fun env' ((x,_), (l,_)) -> extend_env (x, l) env') empty_env (List.combine ves locvs) in
  		let memn = List.fold_left (fun memn (l,v) -> extend_mem (l, v) memn) memn locvs in
  		(Record record, memn)
  	| FIELD (e,x) -> 
  		let r, mem1 = eval e env mem in 
  		(match r with 
  		| Record record -> (apply_mem mem (apply_env record x), mem1)
  		| _ -> raise UndefinedSemantics)	
  	| ASSIGNF (e1,x,e2) -> 
  		let r, mem1 = eval e1 env mem in
  		let v, mem2 = eval e2 env mem1 in
  		(match r with 
  		| Record record -> 
  			(v, extend_mem ((apply_env record x), v) mem2) 
  		| _ -> raise UndefinedSemantics)
    | SEQ (e1, e2) ->
  		let v1, mem1 = eval e1 env mem in
  		let v2, mem2 = eval e2 env mem1 in
  		(v2, mem2)
    | BEGIN e -> eval e env mem 
	in 
	(v, mem') 

let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))

