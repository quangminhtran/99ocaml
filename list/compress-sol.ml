let rec compress = function
	| a :: (b :: _ as t) -> if (a = b) then compress t else compress a :: compress t
	| smaller -> smaller;; 	
