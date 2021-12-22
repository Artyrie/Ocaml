let rec pascal (a, b) =
	match a, b with
	| 0, _ -> 1
	| _, 0 -> 1
	| a, b -> if a = b then 1
	else if a < b then 0
	else pascal (a - 1, b - 1) + pascal (a - 1, b)

let equals v1 v2 = 
    v1 = v2

let test t1 answer =
  let v = pascal t1 in
  (equals v answer)
		
4,2 = 3,1 + 3,2
3,1 = 2,0 + 2,1
3,2 = 2,1 + 2,2
2,0 = 1
2,1 = 1,0 + 1,1
2,2 = 1,1 + 1,2