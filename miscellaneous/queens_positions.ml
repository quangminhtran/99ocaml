let rec num n = 
	let rec aux n acc = 
		if n = 0 then acc
		else aux (n-1) (n :: acc) in
	aux n [];;


let attack prev_rows row col =
	let rec aux prev_col row col = function
		| [] -> false
		| prev_row :: tl -> 
			if (prev_row = row ||  abs(prev_row - row) = abs(prev_col - col)) then true
			else aux (prev_col + 1) row col tl in 		
	aux 1 row col prev_rows

let queens_positions n =
	let all_rows = num n in
		let rec aux acc_rows col  =
			if col > n then [List.rev acc_rows] 
			else let safe_rows = List.filter ( function row -> not (attack (List.rev acc_rows) row col) ) all_rows in
					List.flatten (List.map (function row ->  aux (row :: acc_rows) (col + 1)) safe_rows) in
		aux [] 1;;	 
		
