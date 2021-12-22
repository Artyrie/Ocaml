let rec balanced bt =
	match bt with
	| Empty -> true
	| Node(_, l, Empty) -> if height l > 1 then false else true
	| Node(_, Empty, r) -> if height r > 1 then false else true 
	| Node(_, l, r) -> if balanced l && balanced r then true else false;;