segments :: [a]->[[a]]

segments (x:[]) = [x:[]]
segments (x:xs) = [x]:( (mapXcolon x r) ++ r)  where 
		r = segments xs
		h = head xs
--segments (x:xs) =  

mapXcolon :: a->[[a]]->[[a]]
mapXcolon x (r:[]) = [x:r]
mapXcolon x r = if that > thiss then (x:(head r) ):(mapXcolon x (tail r)) else [x:(head r)] where 	
	that = (length . head . tail) r
	thiss = (length . head) r
--lht :: [a]->Int
--lht r = 
--mapXcolon ::Eq a =>  a->a->[[a]]->[[a]]
--mapXcolon x h (r:[]) = [x:r]
--mapXcolon x h (r:rs) = if (head r) == h then  (xhr):(mapXcolon x h rs) else [xhr] where
--	xhr = x:r
--In the fold the accumulator is just the list 
--

--[[2],[2,3],[3]]

