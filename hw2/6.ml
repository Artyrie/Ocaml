let find_min a b c =
	if a >= b then Stdlib.min b c else Stdlib.min a c

let rec dist (a, b) =
	let a_len = String.length a in
	let b_len = String.length b in
	if a_len = 0 then b_len
	else if b_len = 0 then a_len
	else if String.equal (String.sub a 0 1) (String.sub b 0 1) = true
		then 0 + dist ((String.sub a 1 (String.length a - 1)), (String.sub b 1 (String.length b - 1)))
	else 1 + find_min (dist ((String.sub a 1 (String.length a - 1)), b)) (dist (a, (String.sub b 1 (String.length b - 1)))) (dist ((String.sub a 1 (String.length a - 1)), (String.sub b 1 (String.length b - 1))))


let closest str strs =
	let distance = List.map(fun x -> dist (str, x)) strs in
	let distance = List.sort compare distance in
	let min = List.hd distance in
	List.hd (List.filter (fun x -> dist(str, x) = min) strs)