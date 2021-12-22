type btree = Empty | Node of int * btree * btree

let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))
let t3 = Node (1, Node (2, Node (3, Empty, Empty), Empty), Empty)

let rec height bt = 
	match bt with
	| Empty -> 0
	| Node(_, l, r) -> if height l > height r then 1 + height l else 1 + height r;;