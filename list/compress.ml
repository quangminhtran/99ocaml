let compress l = 
	let rec aux acc prev = function
		| [] -> acc
		| x :: t -> match prev with
						| None -> aux (x :: acc) (Some x) t
						| Some p -> if (x = p) then aux acc prev t else aux (x :: acc) (Some x) t in
	List.rev (aux [] None l);;		
