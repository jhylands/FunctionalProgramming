duplicates :: Eq a=>[a]->[a]
duplicates [] = []
duplicates (x:xs) = if length (positions xs x) == 2 then x:duplicates xs else duplicates xs


 
positions :: Eq a => [a]->a->[Int]
positions [] _ = []
positions (x:xs) i = if x==i then 0:others else others where 
			others = increase (positions xs i)

increase :: [Int]->[Int]
increase [] = []
increase (x:xs) = (x+1):increase xs
