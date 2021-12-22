type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec cal_sigma e1 e2 e3 = 
	if e1 <= e2 then (cal_in e3 e1) +. (cal_sigma (e1 +. 1.0) e2 e3) else 0.0
and cal_integ e1 e2 e3 = 
	if e1 <= e2 then 0.1 *. (cal_in e3 e1) +. (cal_integ (e1 +. 0.1) e2 e3) else 0.0

and cal_in e num = 
	match e with 
	| X -> num
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> cal_in e1 num +. cal_in e2 num
	| SUB (e1, e2) -> cal_in e1 num -. cal_in e2 num
	| MUL (e1, e2) -> cal_in e1 num *. cal_in e2 num
	| DIV (e1, e2) -> cal_in e1 num /. cal_in e2 num
	| SIGMA(e1, e2, e3) -> 
		let a = cal_in e1 num in
		let b = cal_in e2 num in
			cal_sigma a b e3
	| INTEGRAL(e1, e2, e3) ->
		let a = cal_in e1 num in
		let b = cal_in e2 num in
			cal_integ a (b -. 0.1) e3

let rec calculate e = 
	match e with
	| X -> raise (Failure "FreeVariable")
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> calculate e1 +. calculate e2
	| SUB (e1, e2) -> calculate e1 -. calculate e2
	| MUL (e1, e2) -> calculate e1 *. calculate e2
	| DIV (e1, e2) -> calculate e1 /. calculate e2
	| SIGMA(e1, e2, e3) -> 
		let a = calculate e1 in
		let b = calculate e2 in
			cal_sigma a b e3
	| INTEGRAL(e1, e2, e3) ->
		let a = calculate e1 in
		let b = calculate e2 in
			cal_integ a (b -. 0.1) e3
