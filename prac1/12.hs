perms :: [a]->[[a]]
perms [] = []
perms [x] = [[x]]
perms (x:xs) 

-- for every element in ys we want to create an element for each posiion x could be in
where 
	ys = perms xs


