type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let rec ext_val (ae, var) =
	match ae with
	| CONST num -> false
	| VAR v -> if v = var then true else false
	| POWER (v, num) -> if v = var then true else false
		
let rec diff (ae, var) = 
	match ae with
	| CONST num -> CONST 0
	| VAR v -> if v = var then CONST 1 else CONST 0
	| POWER (v, num) -> if v = var then TIMES [CONST num; POWER (v, num - 1)] else CONST 0
	| TIMES list -> SUM (List.map(fun ae -> let list2 = List.filter (fun ae2 -> ae != ae2) list in
		TIMES (diff (ae, var)::list2)) list)
	| SUM list -> SUM (List.map(fun ae -> diff (ae, var)) list)