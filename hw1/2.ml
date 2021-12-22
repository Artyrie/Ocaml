let rec merge l1 l2 =
	match l1, l2 with
	| _, [] -> l1
	| [], _ -> l2
	| h1::t1, h2::t2 -> if h1 >= h2 then [h1] @ (merge [h2] (merge t1 t2))
	else [h2] @ (merge[h1] (merge t1 t2));;