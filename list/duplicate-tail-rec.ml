let duplicate l =
	let rec aux acc = function
		| [] -> acc
		| h :: t -> aux (h :: h :: acc) t
	in List.rev (aux [] l);;
