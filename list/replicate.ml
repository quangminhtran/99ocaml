let rec replicate l n =
	let rec aux x n = 
		if (n = 0) then []
		else x :: (aux x (n-1)) in
	match l with
		| [] -> []
		| h :: t -> List.append (aux h n) (replicate t n);;
