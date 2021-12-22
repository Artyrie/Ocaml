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