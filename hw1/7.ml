let rec fold3 f a bl cl dl = 
	match bl, cl, dl with
	| [], [], [] -> a
	| hd1::tl1, hd2::tl2, hd3::tl3 -> fold3 f (f a hd1 hd2 hd3) tl1 tl2 tl3;;