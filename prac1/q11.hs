type Partition a = [[a]]
parts :: [a]->[Partition a]
parts [] = parts[]
parts [x] = [[[x]]]
parts (x:xs) = case1 ++ case2 where 
	case1 = map ([x]:) ys
	case2 = map (\(p:ps)->(x:p):ps) ys
	ys = parts xs 
