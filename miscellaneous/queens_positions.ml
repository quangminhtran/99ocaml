let rec num n = 
	let rec aux n acc = 
		if n = 0 then acc
		else aux (n-1) (n :: acc) in
	aux n [];;


let attack prev_pos row col =
	List.exists (function (prev_col, prev_row) -> 
							(prev_row = row) ||  
							(abs(prev_row - row) = abs(prev_col - col)))  prev_pos;;

let queens_positions n =
	(* All possible row position 1 -> n *)
	let all_rows = num n in
		let rec aux acc_pos col  =
			if col > n then [ snd (List.split (List.rev acc_pos)) ] 
			(* Filter safe rows, i.e. rows that are not attacked by previous queens *)
			else let safe_rows = List.filter ( function row -> not (attack acc_pos row col) ) all_rows in
					List.flatten (List.map (function row ->  aux ( (col,row) :: acc_pos ) (col + 1)) safe_rows) in
		aux [] 1;;	 
		
