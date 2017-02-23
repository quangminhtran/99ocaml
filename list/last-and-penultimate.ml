let rec last_two: 'a list -> ('a * 'a) option = function
	| [] | [_] -> None
	| [x;y] -> Some (x,y)
	| _ :: t -> last_two t;;
