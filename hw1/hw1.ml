(* 1 *)
let rec gcd n m = 
	if m = 0 then n else gcd m (n mod m);;

(* 2 *)
let rec merge l1 l2 =
	match l1, l2 with
	| _, [] -> l1
	| [], _ -> l2
	| h1::t1, h2::t2 -> if h1 >= h2 then [h1] @ (merge [h2] (merge t1 t2))
	else [h2] @ (merge[h1] (merge t1 t2));;

(* 3 *)
let rec range n m =
	if n > m then []
	else n :: (range (n+1) m);;

(* 4 *)
type formula = TRUE | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
 and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr;;

let rec plma op = 
	match op with
	| PLUS (op1, op2) -> (plma op1) + (plma op2)
	| MINUS (op1, op2) -> (plma op1) - (plma op2)
	| NUM n -> n;;

let rec eval oper = 
	match oper with
	| TRUE -> true
	| FALSE -> false
	| NOT oper -> not (eval oper)
	| ANDALSO (oper1, oper2) -> eval oper1 && eval oper2
	| ORELSE (oper1, oper2) -> eval oper1 || eval oper2
	| IMPLY (oper1, oper2) -> (not (eval oper1)) || (eval oper2)
	| LESS (oper1, oper2) -> plma oper1 < plma oper2;;

(* 5 *)
type btree = Empty | Node of int * btree * btree

let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))
let t3 = Node (1, Node (2, Node (3, Empty, Empty), Empty), Empty)

let rec height bt = 
	match bt with
	| Empty -> 0
	| Node(_, l, r) -> if height l > height r then 1 + height l else 1 + height r;;

(* 6 *)
let rec balanced bt =
	match bt with
	| Empty -> true
	| Node(_, l, Empty) -> if height l > 1 then false else true
	| Node(_, Empty, r) -> if height r > 1 then false else true 
	| Node(_, l, r) -> if balanced l && balanced r then true else false;;

(* 7 *)
let rec fold3 f a bl cl dl = 
	match bl, cl, dl with
	| [], [], [] -> a
	| hd1::tl1, hd2::tl2, hd3::tl3 -> fold3 f (f a hd1 hd2 hd3) tl1 tl2 tl3;;

(* 8 *)
let rec iter n f =
	if n = 0 then fun x -> x else (fun x -> f ((iter (n-1) f) x));;

(* 9 *)
let rec sigma (a, b, f) = 
	if a > b then 0 else if a = b then f a else (f a) + sigma (a + 1, b, f);;

(* 10 *)
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