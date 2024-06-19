(* Exercise 1 *)

let rec revrev l =
  match l with
    | [] -> []
    | hd :: tl -> (revrev tl) @ [List.rev hd]

(* Exercise 2 *)

let rec rm_dup lst =
  match lst with
  | [] -> []
  | hd :: tl -> if List.mem hd tl then rm_dup tl
                else hd :: rm_dup tl

let union l1 l2 =
  rm_dup l1@l2

(* let union l1 l2 = List.sort_uniq compare (l1 @ l2) *)

(* Exercise 3 *)

let alterSum lst =
  let rec helper l flag acc =
    match l with
    | [] -> acc
    | hd :: tl -> helper tl (-flag) acc+flag*hd
  in
  match lst with
  | [] -> 0
  | hd :: tl -> helper tl 1 hd

(* Exercise 4 *)

let rec insert x l =
  match l with
  | [] -> [x]
  | hd :: tl -> if x > hd then x :: l
                else hd :: insert x tl

let rec dsort lst = 
  match lst with
  | [] -> []
  | hd :: tl -> insert hd (dsort tl)

(* Exercise 5 *)

 type circuit = IN
    | AND of circuit * circuit
    | OR of circuit * circuit

let rec and_depth c =
    match c with
    | IN -> 0
    | AND (c1, c2) -> (max (and_depth c1) (and_depth c2)) + 1
    | OR (c1, c2) -> (max (and_depth c1) (and_depth c2))


(* Exercise 6 *)

let rec iter n f = 
	if n = 0 then fun x -> x 
	else 
		(fun x -> f ((iter (n-1) f) x)) 

(* Exercise 7 *)

let rec mapn f n lst =
  if n = 0 then lst
  else mapn f (n-1) (List.map f lst) 

(* Exercise 8 *)

type 'a ntree =
  Leaf of 'a | Node of 'a ntree list

let rec findn nt = 
  match nt with
  | Leaf _ -> 0
  | Node ntlst -> max (List.length ntlst) (List.fold_left (fun acc t -> max acc (findn t)) 0 ntlst)

(* Exercise 9 *)

let rec flatten nt =
  match nt with
  | Leaf a -> [a]
  | Node ntlst -> List.fold_left (fun acc t -> acc @ (flatten t)) [] ntlst

(* Exercise 10 *)

type formula = TRUE | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
  and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec calc exp = 
	match exp with 
	| PLUS (exp1, exp2) -> (calc exp1) + (calc exp2)
	| MINUS (exp1, exp2) -> (calc exp1) - (calc exp2)
	| NUM n -> n   

let rec eval form =
	match form with 
	| TRUE -> true 
	| FALSE -> false
	| NOT form' -> not (eval form')
	| ANDALSO (form1, form2) -> (eval form1) && (eval form2) 
	| ORELSE (form1, form2) -> (eval form1) || (eval form2)
	| IMPLY (form1, form2) -> (not (eval form1)) || (eval form2)  
	| LESS (expr1, expr2) -> (calc expr1) < (calc expr2)

(* Exercise 11 *)

let rec sigma (a,b,f) = 
	if a > b then 0 
	else 
		(f a) + sigma (a+1, b, f) 

(* Exercise 12 *)

type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (ae, s) =
  match ae with
  | CONST i -> CONST 0
  | VAR v -> if v = s then CONST 1 else CONST 0
  | POWER (v, i) -> 
		if v = s then TIMES [CONST i; POWER (v, i-1)] else CONST 0
  | TIMES aes ->
		SUM (List.map (fun ae -> 
          let rec remains aei res = 
            match aei with
            | h::t -> if (Pervasives.compare ae h) == 0 then res@t else remains t (res@[h])
            | [] -> res
          in
					let remaining = remains aes [] in   
					TIMES (diff (ae, s)::remaining)
				) aes) 
  | SUM aes -> 
		SUM (List.map (fun ae -> diff (ae, s)) aes)

(* Exercise 13 *)

type exp = X | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let rec sigma l u e =
  if l > u then 0.0 else (calc_with_x_value e l) +. (sigma (l +. 1.0) u e)

and integral l u e =
  if l > u then 0.0 else (0.1 *. (calc_with_x_value e l)) +. (integral (l +. 0.1) u e)

and calc_with_x_value e v =
  match e with
  | X -> v
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calc_with_x_value e1 v) +. (calc_with_x_value e2 v)
  | SUB (e1, e2) -> (calc_with_x_value e1 v) -. (calc_with_x_value e2 v)
  | MUL (e1, e2) -> (calc_with_x_value e1 v) *. (calc_with_x_value e2 v)
  | DIV (e1, e2) -> (calc_with_x_value e1 v) /. (calc_with_x_value e2 v)
  | SIGMA (e1, e2, e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    sigma l u e3
  | INTEGRAL (e1, e2 ,e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    integral l (u -. 0.1) e3

let rec calculate e =
  match e with
  | X -> raise (Failure "FreeVariable")
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calculate e1) +. (calculate e2)
  | SUB (e1, e2) -> (calculate e1) -. (calculate e2)
  | MUL (e1, e2) -> (calculate e1) *. (calculate e2)
  | DIV (e1, e2) -> (calculate e1) /. (calculate e2)
  | SIGMA (e1, e2, e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    sigma l u e3
  | INTEGRAL (e1, e2 ,e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    integral l (u -. 0.1) e3


(* Exercise 14 *)
type talkingto = (string * string) list


let rec infected : talkingto -> string -> string -> bool = 
	fun rels talker listener ->
		let rec get_all_infected infected_sofar =
			let newly_infected = 
  			List.fold_left (fun result person ->
  				let infected_by_person = 
						List.map snd 
    					(List.filter (fun (p1, p2) -> 
  							(p1 = person) && not (List.mem p2 infected_sofar)
  						) rels)
  				in 
  				result @ infected_by_person 
  			) [] infected_sofar
			in
			if newly_infected = [] then infected_sofar 
			else get_all_infected (newly_infected @ infected_sofar) 
		in  
		let all_infected = get_all_infected [talker] in 
		List.mem listener all_infected
		
		 
(* Exercise 15 *)

let rec infected_vaccine : talkingto -> string list -> string -> string -> bool =
	fun rels vaccines talker listener -> 
		let rec get_all_infected infected_sofar =
			let newly_infected = 
  			List.fold_left (fun result person ->
  				let infected_by_person = 
						List.map snd 
    					(List.filter (fun (p1, p2) -> 
  							(p1 = person) && not (List.mem p2 infected_sofar) && not (List.mem p2 vaccines)
  						) rels)
  				in 
  				result @ infected_by_person 
  			) [] infected_sofar
			in
			if newly_infected = [] then infected_sofar 
			else get_all_infected (newly_infected @ infected_sofar) 
		in  
		if List.mem talker vaccines then false 
		else 
  		let all_infected = get_all_infected [talker] in 
  		List.mem listener all_infected

