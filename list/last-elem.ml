(* Return the last element of a list *)
let rec last: 'a list -> 'a option = function
	| [] -> None
	| [x] -> Some x 
	| _ :: t  -> last t;;
