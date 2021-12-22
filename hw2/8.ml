type relationships = (string * string) list

let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []

let rec transitive graph =
	match graph with
	| [] -> []
	| (a, b)::tl -> 
		let likes_from = List.filter (fun (af, bf) -> af = b) graph in
		let rec transitive_in likes_f =
			match likes_f with
			| [] -> []
			| (a_i, b_i)::tl2 ->
				let new_rel = [(a, b_i)] in
				if List.length (List.filter (fun (a_ii, b_ii) -> (a_ii, b_ii) = (a, b_i)) graph) = 1
				then [] @ transitive_in tl2 else new_rel @ transitive_in tl2
			in
			[(a, b)] @ transitive_in likes_from @ transitive tl

let rec make_rel rels = 
	let llist = remove_duplicates (List.rev (transitive (List.rev (transitive rels))) ) in
	if (List.length rels) = (List.length llist) then rels else make_rel llist

let rec count_self rels =
	match rels with
	| [] -> 0
	| (a, b)::tl -> if a = b then 1 + count_self tl else 0 + count_self tl

let selflove graph = 
	let rels = make_rel graph in
	count_self rels
	
let equals v1 v2 = 
    v1 = v2

let test t1 answer =
  let v = (selflove t1) in
  (equals v answer)