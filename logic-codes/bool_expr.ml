type bool_expr = 
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or of bool_expr * bool_expr;;

(* Evaluation of expression within an environment*)
let rec eval env = function
	| Var v -> List.assoc v env
	| Not e -> not (eval env e)
	| And (e1,e2) -> (eval env e1) && (eval env e2)
	| Or (e1,e2) -> (eval env e1) || (eval env e2);;

(* Produce all boolean combinations for variables*)
let produce vars = 
	let rec aux acc = function
		| [] -> [List.rev acc]
		| v :: tl ->  (aux ((v, true) :: acc) tl) 
					 @ (aux ((v, false) :: acc) tl) in
	aux [] vars  
	 		
(* Truth tables*)
let table vars expr = 
	(* For each combination evaluate the expression*)
	let combinations = produce vars in
		List.map (function combi -> (combi,  (eval combi expr))) combinations
