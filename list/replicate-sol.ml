let replicate l n = 
	let rec prepend n acc x = 
		if n = 0 then acc else prepend (n-1) (x :: acc) x in
	let rec aux acc n = function
		| [] -> acc
		| h :: t -> aux (prepend n acc h) n t in
	List.rev (aux [] n l);; 	
