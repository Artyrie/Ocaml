let rec cartesian al bl =
	let rec cartesian_tmp alhd bl2 =
		match bl2 with
		| [] -> []
		| hd::tl -> (alhd, hd) :: (cartesian_tmp alhd tl) in
  match al with
  | [] -> []
  | hd::tl -> (cartesian_tmp hd bl) @ (cartesian tl bl);;

let rec contains_all l1 l2 =
	match l1 with
	| [] -> true
	| hd::tl -> if List.mem hd l2 then contains_all tl l2 else false

let equivalence a b = (contains_all a b) && (contains_all b a)

let test t1 t2 answer =
  let v = cartesian t1 t2 in
  (equivalence v answer)