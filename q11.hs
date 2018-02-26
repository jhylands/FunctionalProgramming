--parts :: [a]->[[[a]]]
--parts x = lenSort (segments (a))

segOn :: Int-> [a] -> [[a]]
segOn _ [] = []
segOn 1 (x:xs) = [x]:(segOn 1 xs)
segOn n xs = partition n xs

partition :: Int->[a]->[[a]]
partition 0 _ = []
partition n xs |n<=(length xs) = (take n xs):(partition n (drop n xs))
	       |otherwise = partition (length xs) xs

--2 [1,2,3] -> [1,2] [2,3]

partsOflength :: Int->[a]->[[a]]
partsOflength n xs |(length xs)>=n = (take n xs ):partsOflength n (tail xs)
		   |otherwise = []


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

lenSort:: [[a]]->[[a]]
lenSort [] = []
lenSort [x] = [x]
lenSort (x:xs) = lenMerge (x:[]) (lenSort xs)

lenMerge :: [[a]]->[[a]]->[[a]]
lenMerge [] [] = []
lenMerge [] xs = xs
lenMerge xs [] = xs
lenMerge (x:xs) (y:ys) = if (length x) > (length y) then x:lenMerge xs (y:ys) else y:lenMerge (x:xs) ys
