type 'a node =
	| One of 'a
	| Many of 'a node list;;

(* Auxilary: turn a node into a list*)
let rec	aux: 'a node -> 'a list = function
	| One x -> [x]
	| Many ns ->  List.fold_left List.append []  (List.map (fun n -> aux n) ns);;

(* Flatten a list of nodes into a list using auxilary function *)	
let rec flatten (l: 'a node list): 'a list = 
	List.fold_left List.append []  ((List.map (fun x -> aux x) l));;

(* 
flatten [One "a"; Many [One "b"; One "c"; One "d"]; One "e" ];;
- : string list = ["a"; "b"; "c"; "d"; e]
*)
