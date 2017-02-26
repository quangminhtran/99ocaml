(* Remove the k'th element from a list *)
let remove_at k list =
	let rec aux i k = function
		| [] -> []
		| h :: t -> 
			if (i = k) then aux (i+1) k t
			else h :: aux (i+1) k t in
	aux 0 k list;;		
 	
assert(remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]);;	
