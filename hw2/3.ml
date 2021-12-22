type bigint = BigInt of string
type op = ADD | MUL

let explode s =
  let rec expl i l =
    if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let ext_val e = 
	match e with
	| BigInt str -> explode str

let c2i e = List.map(fun x -> (int_of_char x) - 48) e
let rec cl2s e =
	match e with
	| [] -> ""
	| hd::tl -> hd ^ cl2s tl

let rec padding e len = 
	if (List.length e) < len then padding (['0']@e) len else e
	
let rec rm_padding e =
	match e with
	| [] -> []
	| hd::tl -> if hd != 0 then [hd] @ tl else rm_padding tl  

let rec add_op list1 list2 =
	match list1, list2 with
	| [], [] -> []
	| hd1::tl1, hd2::tl2 -> [hd1 + hd2] @ (add_op tl1 tl2)

let mul_list e1 num = List.map(fun x -> x * num) e1

let rec mul_op list1 list2 =
	match list2 with
	| [] -> c2i (padding ['0'] (List.length list1))
	| hd::tl -> if hd = 0 then mul_op list1 tl
		else add_op (mul_list list1 (hd * int_of_float(10.**(float_of_int((List.length list2) - 1))))) (mul_op list1 tl)

let rec organize_in e = 
	match e with
	| [] -> []
	| hd::[] -> [hd]
	| hd::md::tl -> if md >= 10 then
		[hd + (md / 10)] @ organize_in ([md - ((md / 10) * 10)] @ tl)
		else [hd] @ (organize_in ([md] @ tl))
		
let rec organize et = 
		if List.length (List.filter (fun x -> x >= 10) et) > 0 then
			organize (organize_in et) else et

let compute_bigint op (e1, e2) =
	match op with
	| ADD ->
		let a = c2i (padding (ext_val e1) 500) in
		let b = c2i (padding (ext_val e2) 500) in
		let c : bigint = BigInt (cl2s (List.map(fun x -> string_of_int x) (rm_padding (organize (add_op a b)))) ) in
		c
	| MUL -> 
		let a = c2i (padding (ext_val e1) 500) in
		let b = c2i (padding (ext_val e2) 500) in
		let c : bigint = BigInt (cl2s (List.map(fun x -> string_of_int x) (rm_padding (organize (mul_op a b)))) ) in
		c