sublists :: [a]->[[a]]
sublists [] = [[]]
sublists (x:xs) =  (map (x:) r ) ++ r where 
					r = sublists xs
					
