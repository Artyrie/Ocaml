let rec count_string str fstr = 
	let str_len = String.length str in
	let fstr_len = String.length fstr in
	if str_len < fstr_len then 0
	else if String.equal "" fstr then 0
	else if String.equal (String.sub str 0 fstr_len) fstr
		then 1 + count_string (String.sub str 1 (str_len - 1)) fstr
	else 0 + count_string (String.sub str 1 (str_len - 1)) fstr