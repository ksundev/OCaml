(* HW 2 *)

(* Exercise 1 *)
let rec npower x n =
	if n = 0 then 1. 
	else (npower x (n - 1)) *. (1. /. (float_of_int x))   

(* Exercise 2 *)
let rec gcd n m =
  let n, m = if n >= m then (n, m) else (m, n) in
  if m = 0 then n
  else gcd (n - m) m

(* Exercise 3*)
let rec min l =
  match l with
  | [] -> 0
  | hd :: tl ->
    let min_tl = min tl in
    if hd < min_tl then hd else min_tl

(* Exercise 4 *)
let rec map f l =
    match l with
    | [] -> []
    | hd :: tl -> (f hd) :: (map f tl)

let rec cartesian l1 l2 =
    match l1 with
    | [] -> []
    | hd1 :: tl1 ->
        (map (fun x -> (hd1, x)) l2) @ (cartesian tl1 l2)

(* Exercise 5 *)

type btree = Leaf | Node of int * btree * btree

let rec count_leaves t =
	match t with 
	| Leaf -> 1 
	| Node (_, t1, t2) -> (count_leaves t1) + (count_leaves t2) 

(* Exercise 6 *)

let rec count_oddnode t = 
	match t with 
	| Leaf -> 0 
	| Node (n, t1, t2) ->
		(if n mod 2 = 1 then 1 else 0) + (count_oddnode t1) + (count_oddnode t2)

(* Exercise 7 *)

let rec insert_btree x t = 
	match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) ->
     if x = y then t
     else if x < y then Node (y, insert_btree x l, r)
     else Node (y, l, insert_btree x r)

(* Exercise 8 *)

let rec duplicate l =
	match l with 
	| [] -> [] 
	| h :: t -> h :: h :: (duplicate t) 
	
(* Exercise 9 *)

let rec replicate l n =
	match l with 
	| [] -> [] 
	| h :: t -> 
		if n = 0 then []
		else 
			match t with 
			| [] -> h :: (replicate [h] (n - 1)) 
			| _ -> (replicate [h] n) @ (replicate t n)  
	
(* Exercise 10 *)

let rec remove x l = 
	match l with 
	| [] -> []
	| h :: t -> if x = h then remove x t else h :: (remove x t) 

let rec deduplicate l = 
	match l with 
	| [] -> [] 
	| h :: t -> h :: deduplicate (remove h t) 

(* Exercise 11 *)
let rec lall l p =
  match l with
  | [] -> true
  | hd :: tl -> (p hd) && (lall tl p)

(* Exercise 12 *)
let rec lany l p =
  match l with
  | [] -> false
  | hd :: tl -> (p hd) || (lany tl p)

(* Exercise 13 *)

let rec powerset l =
  match l with
  | [] -> [[]]
  | hd :: tl ->
    let powerset_tl = powerset tl in
    (map (fun s -> hd :: s) powerset_tl) @ powerset_tl
