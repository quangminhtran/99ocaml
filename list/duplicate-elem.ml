let duplicate l =
	let rec aux acc = function
		| [] -> acc
		| a :: t -> aux (a :: (a :: acc)) t 
	in List.rev (aux [] l);;
